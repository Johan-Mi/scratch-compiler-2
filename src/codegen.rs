use crate::{comptime, function, mir, ty::Ty};
use sb3_builder::{
    block, Constant, Costume, CustomBlock, InsertionPoint, List, ListRef,
    Operand, Parameter, ParameterKind, Project, Target, Variable, VariableRef,
};
use std::{
    collections::{hash_map::Entry, BTreeMap, HashMap, HashSet},
    path::Path,
};

type Result<T> = std::result::Result<T, Box<dyn std::error::Error>>;

enum CompiledOperand {
    String(Operand),
    Bool(Operand),
}

impl CompiledOperand {
    fn s(self) -> Operand {
        match self {
            Self::String(operand) | Self::Bool(operand) => operand,
        }
    }

    fn b(self, sprite: &mut Target) -> Operand {
        match self {
            Self::String(operand) => {
                sprite.eq(operand, "true".to_owned().into())
            }
            Self::Bool(operand) => operand,
        }
    }
}

use CompiledOperand::{Bool as B, String as S};

pub fn generate(
    mut document: mir::Document,
    output_path: &Path,
    stage_variables: &HashSet<mir::RealVar>,
) -> Result<()> {
    let mut project = Project::default();

    let mut stage = project.stage();
    let mut compiled_real_vars = stage_variables
        .iter()
        .map(|&var| {
            (
                var,
                stage.add_variable(Variable {
                    name: var.to_string(),
                    // FIXME: use initializer specified in source code
                    value: Constant::Number(0.0),
                }),
            )
        })
        .collect();

    for (name, sprite) in document.sprites {
        compile_sprite(
            sprite,
            &name,
            &mut document.functions,
            &mut project,
            &mut compiled_real_vars,
        )?;
    }

    let file = std::fs::File::create(output_path)?;
    project.finish(file)
}

fn compile_sprite(
    hir: crate::hir::Sprite,
    name: &str,
    functions: &mut BTreeMap<usize, mir::Function>,
    project: &mut Project,
    compiled_real_vars: &mut HashMap<mir::RealVar, VariableRef>,
) -> Result<()> {
    let mut sprite = if name == "Stage" {
        project.stage()
    } else {
        project.add_sprite(name.to_owned())
    };

    if name == "Stage" {
        if let Ok(comment) = std::env::var("SC2_COMMENT") {
            sprite.add_comment(comment);
        }
    }

    for costume in hir.costumes {
        sprite.add_costume(Costume::from_file(
            costume.name,
            costume.path.as_ref(),
        )?);
    }

    let mut compiled_ssa_vars = HashMap::new();

    let compiled_functions = functions
        .iter()
        .filter(|(_, function)| function.owning_sprite.as_deref() == Some(name))
        .filter_map(|(&index, function)| {
            Some(index).zip(CompiledFunctionRef::new(
                function,
                &mut sprite,
                &mut compiled_ssa_vars,
            ))
        })
        .collect();

    let mut cx = Context {
        sprite,
        compiled_ssa_vars,
        compiled_real_vars,
        compiled_real_lists: HashMap::new(),
        compiled_functions,
        return_variable: None,
        is_linear: HashSet::new(),
    };

    for (index, function) in crate::stdx::extract_if(functions, |function| {
        function.owning_sprite.as_deref() == Some(name)
    }) {
        compile_function(function, index, &mut cx);
    }

    Ok(())
}

struct Context<'a> {
    sprite: Target<'a>,
    compiled_ssa_vars: HashMap<mir::SsaVar, CompiledSsaVar>,
    compiled_real_vars: &'a mut HashMap<mir::RealVar, VariableRef>,
    compiled_real_lists: HashMap<mir::RealList, ListRef>,
    compiled_functions: HashMap<usize, CompiledFunctionRef>,
    return_variable: Option<VariableRef>,
    is_linear: HashSet<mir::SsaVar>,
}

impl Context<'_> {
    fn compile_real_var(&mut self, var: mir::RealVar) -> VariableRef {
        self.compiled_real_vars
            .entry(var)
            .or_insert_with(|| {
                self.sprite.add_variable(Variable {
                    name: var.to_string(),
                    // FIXME: use initializer specified in source code
                    value: Constant::Number(0.0),
                })
            })
            .clone()
    }

    fn compile_real_list(&mut self, list: mir::RealList) -> ListRef {
        self.compiled_real_lists
            .entry(list)
            .or_insert_with(|| {
                self.sprite.add_list(List {
                    name: list.to_string(),
                    // FIXME: use initializer specified in source code
                    items: Vec::new(),
                })
            })
            .clone()
    }
}

enum CompiledSsaVar {
    Linear(CompiledOperand),
    Relevant(VariableRef),
    CustomBlockParameter(Parameter),
}

struct CompiledFunctionRef {
    block: CustomBlock,
    insertion_point: Option<InsertionPoint>,
    return_variable: Option<VariableRef>,
}

impl CompiledFunctionRef {
    fn new(
        function: &mir::Function,
        sprite: &mut Target,
        compiled_ssa_vars: &mut HashMap<mir::SsaVar, CompiledSsaVar>,
    ) -> Option<Self> {
        if function::Special::try_from(&*function.name).is_ok() {
            return None;
        }

        let parameters = function
            .parameters
            .iter()
            .filter_map(|parameter| {
                let kind = parameter_kind_for_ty(&parameter.ty)?;
                Some(Parameter {
                    name: parameter.ssa_var.to_string(),
                    kind,
                })
            })
            .collect::<Vec<_>>();

        for (custom_block_param, function_param) in
            std::iter::zip(&parameters, &function.parameters)
        {
            compiled_ssa_vars.insert(
                function_param.ssa_var,
                CompiledSsaVar::CustomBlockParameter(
                    custom_block_param.clone(),
                ),
            );
        }

        let (block, insertion_point) =
            sprite.add_custom_block(function.name.to_string(), parameters);

        let return_variable = function.returns_something.then(|| {
            sprite.add_variable(Variable {
                name: format!("return {}", function.name),
                value: Constant::Number(0.0),
            })
        });

        Some(Self {
            block,
            insertion_point: Some(insertion_point),
            return_variable,
        })
    }
}

fn parameter_kind_for_ty(ty: &Ty) -> Option<ParameterKind> {
    match ty {
        Ty::Unit => None,
        Ty::Num | Ty::String => Some(ParameterKind::StringOrNumber),
        Ty::Bool => Some(ParameterKind::Boolean),
        Ty::Never
        | Ty::Sprite
        | Ty::Ty
        | Ty::Var(_)
        | Ty::List(_)
        | Ty::Generic(_)
        | Ty::Struct { .. } => unreachable!(),
    }
}

fn compile_function(
    mut function: mir::Function,
    index: usize,
    cx: &mut Context,
) {
    cx.return_variable = match function::Special::try_from(&*function.name) {
        Ok(function::Special::WhenFlagClicked) => {
            cx.sprite.start_script(block::when_flag_clicked());
            None
        }
        Ok(function::Special::WhenKeyPressed) => {
            cx.sprite.start_script(block::when_key_pressed(
                function.tag.take().unwrap(),
            ));
            None
        }
        Ok(function::Special::WhenCloned) => {
            cx.sprite.start_script(block::when_cloned());
            None
        }
        Ok(function::Special::WhenReceived) => {
            cx.sprite.start_script(block::when_received(
                function.tag.take().unwrap(),
            ));
            None
        }
        Err(()) => {
            let compiled_ref = cx.compiled_functions.get_mut(&index).unwrap();
            cx.sprite
                .insert_at(compiled_ref.insertion_point.take().unwrap());
            compiled_ref.return_variable.clone()
        }
    };

    cx.is_linear = mir::linearity::check(&mut function);

    compile_block(function.body, cx);
}

fn compile_block(block: mir::Block, cx: &mut Context) {
    for op in block.ops {
        compile_op(op, cx);
    }
}

fn compile_op(op: mir::Op, cx: &mut Context) {
    match op {
        mir::Op::Return { value, is_explicit } => {
            let value = compile_value(value, cx);
            let return_variable = cx.return_variable.clone().unwrap();
            cx.sprite
                .put(block::set_variable(return_variable, value.s()));
            if is_explicit {
                cx.sprite.put(block::stop_this_script());
            }
        }
        mir::Op::If {
            condition,
            then,
            else_,
        } => {
            let condition = compile_value(condition, cx);
            let condition = condition.b(&mut cx.sprite);
            let after = if else_.ops.is_empty() {
                let after = cx.sprite.if_(condition);
                compile_block(then, cx);
                after
            } else {
                let [after, else_clause] = cx.sprite.if_else(condition);
                compile_block(then, cx);
                cx.sprite.insert_at(else_clause);
                compile_block(else_, cx);
                after
            };
            cx.sprite.insert_at(after);
        }
        mir::Op::Forever { body } => {
            cx.sprite.forever();
            compile_block(body, cx);
        }
        mir::Op::While { condition, body } => {
            let condition = compile_value(condition, cx);
            let condition = condition.b(&mut cx.sprite);
            let after = cx.sprite.while_(condition);
            compile_block(body, cx);
            cx.sprite.insert_at(after);
        }
        mir::Op::For {
            variable,
            times,
            body,
        } => {
            let times = compile_value(times, cx);
            let after = if let Some(variable) = variable {
                let var = cx.sprite.add_variable(Variable {
                    name: variable.to_string(),
                    value: Constant::Number(0.0),
                });
                cx.compiled_ssa_vars
                    .insert(variable, CompiledSsaVar::Relevant(var.clone()));
                cx.sprite.for_(var, times.s())
            } else {
                cx.sprite.repeat(times.s())
            };
            compile_block(body, cx);
            cx.sprite.insert_at(after);
        }
        mir::Op::Call(variable, mir::Call::Custom { function, args }) => {
            compile_function_call(function, args, variable, cx);
        }
        mir::Op::Call(variable, mir::Call::Intrinsic { name, args }) => {
            let res = compile_intrinsic(&name, args, cx);
            if let Some(variable) = variable {
                store_result(variable, res.unwrap(), cx);
            }
        }
    }
}

fn compile_intrinsic(
    name: &str,
    mut args: Vec<mir::Value>,
    cx: &mut Context<'_>,
) -> Option<CompiledOperand> {
    match name {
        "get" => {
            let [mir::Value::Lvalue(var)] = *args else {
                unreachable!()
            };
            Some(S(cx.compile_real_var(var).into()))
        }
        "set" => {
            let mir::Value::Lvalue(var) = args[0] else {
                unreachable!()
            };
            let var = cx.compile_real_var(var);
            let value = compile_value(args.pop().unwrap(), cx);
            cx.sprite.put(block::set_variable(var, value.s()));
            None
        }
        "push" => {
            let mir::Value::List(list) = args[0] else {
                unreachable!()
            };
            let list = cx.compile_real_list(list);
            let value = compile_value(args.pop().unwrap(), cx);
            cx.sprite.put(block::append(list, value.s()));
            None
        }
        "delete" => {
            let mir::Value::List(list) = args[0] else {
                unreachable!()
            };
            let list = cx.compile_real_list(list);
            let index = compile_value(args.pop().unwrap(), cx);
            cx.sprite.put(block::delete_of_list(list, index.s()));
            None
        }
        "pop" => {
            let mir::Value::List(list) = args[0] else {
                unreachable!()
            };
            let list = cx.compile_real_list(list);
            cx.sprite
                .put(block::delete_of_list(list, "last".to_owned().into()));
            None
        }
        "delete-all" => {
            let mir::Value::List(list) = args[0] else {
                unreachable!()
            };
            let list = cx.compile_real_list(list);
            cx.sprite.put(block::delete_all_of_list(list));
            None
        }
        "insert" => {
            let mir::Value::List(list) = args[0] else {
                unreachable!()
            };
            let list = cx.compile_real_list(list);
            let index = compile_value(args.pop().unwrap(), cx);
            let item = compile_value(args.pop().unwrap(), cx);
            cx.sprite
                .put(block::insert_at_list(list, item.s(), index.s()));
            None
        }
        "replace" => {
            let mir::Value::List(list) = args[0] else {
                unreachable!()
            };
            let list = cx.compile_real_list(list);
            let item = compile_value(args.pop().unwrap(), cx);
            let index = compile_value(args.pop().unwrap(), cx);
            cx.sprite.put(block::replace(list, index.s(), item.s()));
            None
        }
        "replace-last" => {
            let mir::Value::List(list) = args[0] else {
                unreachable!()
            };
            let list = cx.compile_real_list(list);
            let item = compile_value(args.pop().unwrap(), cx);
            cx.sprite.put(block::replace(
                list,
                "last".to_owned().into(),
                item.s(),
            ));
            None
        }
        "at" => {
            let mir::Value::List(list) = args[0] else {
                unreachable!()
            };
            let list = cx.compile_real_list(list);
            let index = compile_value(args.pop().unwrap(), cx);
            Some(S(cx.sprite.item_of_list(list, index.s())))
        }
        "last" => {
            let mir::Value::List(list) = args[0] else {
                unreachable!()
            };
            let list = cx.compile_real_list(list);
            Some(S(cx.sprite.item_of_list(list, "last".to_owned().into())))
        }
        "index" => {
            let mir::Value::List(list) = args[0] else {
                unreachable!()
            };
            let list = cx.compile_real_list(list);
            let item = compile_value(args.pop().unwrap(), cx);
            Some(S(cx.sprite.item_num_of_list(list, item.s())))
        }
        "length" if matches!(&*args, [mir::Value::List(_)]) => {
            let mir::Value::List(list) = args[0] else {
                unreachable!()
            };
            let list = cx.compile_real_list(list);
            Some(S(cx.sprite.length_of_list(list)))
        }
        "contains" => {
            let mir::Value::List(list) = args[0] else {
                unreachable!()
            };
            let list = cx.compile_real_list(list);
            let item = compile_value(args.pop().unwrap(), cx);
            Some(B(cx.sprite.list_contains_item(list, item.s())))
        }
        _ => {
            let args =
                args.into_iter().map(|arg| compile_value(arg, cx)).collect();
            compile_regular_intrinsic(name, args, cx)
        }
    }
}

fn compile_function_call(
    function: usize,
    args: Vec<mir::Value>,
    variable: Option<mir::SsaVar>,
    cx: &mut Context,
) {
    let args = args
        .into_iter()
        .map(|arg| compile_value(arg, cx).s())
        .collect();
    let compiled_ref = &cx.compiled_functions[&function];
    cx.sprite.use_custom_block(&compiled_ref.block, args);
    if let Some(variable) = variable {
        let return_variable =
            S(compiled_ref.return_variable.clone().unwrap().into());
        store_result(variable, return_variable, cx);
    }
}

fn store_result(
    variable: mir::SsaVar,
    operand: CompiledOperand,
    cx: &mut Context,
) {
    if cx.is_linear.contains(&variable) {
        cx.compiled_ssa_vars
            .insert(variable, CompiledSsaVar::Linear(operand));
    } else {
        let var = cx.sprite.add_variable(Variable {
            name: variable.to_string(),
            value: Constant::Number(0.0),
        });
        cx.sprite.put(block::set_variable(var.clone(), operand.s()));
        cx.compiled_ssa_vars
            .insert(variable, CompiledSsaVar::Relevant(var));
    }
}

fn compile_value(value: mir::Value, cx: &mut Context) -> CompiledOperand {
    match value {
        mir::Value::Var(var) => {
            let Entry::Occupied(mut entry) = cx.compiled_ssa_vars.entry(var)
            else {
                panic!("undefined SSA variable: {var:?}");
            };
            match entry.get_mut() {
                CompiledSsaVar::Relevant(var_ref) => S(var_ref.clone().into()),
                CompiledSsaVar::CustomBlockParameter(param) => {
                    S(cx.sprite.custom_block_parameter(param.clone()))
                }
                CompiledSsaVar::Linear(_) => {
                    if let CompiledSsaVar::Linear(operand) = entry.remove() {
                        operand
                    } else {
                        unreachable!()
                    }
                }
            }
        }
        mir::Value::Imm(
            comptime::Value::Sprite { .. } | comptime::Value::Ty(_),
        )
        | mir::Value::Lvalue(_)
        | mir::Value::List(_) => unreachable!(),
        mir::Value::Imm(comptime::Value::Num(n)) => S(n.into()),
        mir::Value::Imm(comptime::Value::String(s)) => S(s.into()),
        // TODO: booleans are tricky since Scratch doesn't have literals,
        // variables or lists for them. We'll probably need a parameter to
        // specify what kind of slot the expression will be used in so we
        // can choose between leaving it empty, using an empty `not` or the
        // strings "false"/"true".
        mir::Value::Imm(comptime::Value::Bool(_)) => todo!(),
    }
}

fn compile_regular_intrinsic(
    name: &str,
    mut arguments: Vec<CompiledOperand>,
    cx: &mut Context,
) -> Option<CompiledOperand> {
    macro_rules! f {
        ($block:ident($($param:ident),*)) => {
            {
                let [$($param),*] = arguments.try_into().ok().unwrap();
                cx.sprite.put(block::$block($($param.s()),*));
                None
            }
        };

        (= $block:ident($($param:ident),*)) => {
            {
                let [$($param),*] = arguments.try_into().ok().unwrap();
                Some(S(cx.sprite.$block($($param.s()),*)))
            }
        };

        (= B $block:ident($($param:ident),*)) => {
            {
                let [$($param),*] = arguments.try_into().ok().unwrap();
                Some(B(cx.sprite.$block($($param.s()),*)))
            }
        };
    }

    match name {
        "to-string" | "to-num" => arguments.pop(),
        "length" => f! { = length(string) },
        "letter" => f! { = letter_of(string, index) },
        "add" => f! { = add(lhs, rhs) },
        "sub" => f! { = sub(lhs, rhs) },
        "mul" => f! { = mul(lhs, rhs) },
        "div" => f! { = div(lhs, rhs) },
        "mod" => f! { = modulo(lhs, rhs) },
        "lt" => f! { = B lt(lhs, rhs) },
        "eq" => f! { = B eq(lhs, rhs) },
        "gt" => f! { = B gt(lhs, rhs) },
        "abs" => Some(mathop("abs", arguments, cx)),
        "floor" => Some(mathop("floor", arguments, cx)),
        "ceil" => Some(mathop("ceiling", arguments, cx)),
        "sqrt" => Some(mathop("sqrt", arguments, cx)),
        "ln" => Some(mathop("ln", arguments, cx)),
        "log" => Some(mathop("log", arguments, cx)),
        "exp" => Some(mathop("e ^", arguments, cx)),
        "exp-10" => Some(mathop("10 ^", arguments, cx)),
        "sin" => Some(mathop("sin", arguments, cx)),
        "cos" => Some(mathop("cos", arguments, cx)),
        "tan" => Some(mathop("tan", arguments, cx)),
        "asin" => Some(mathop("asin", arguments, cx)),
        "acos" => Some(mathop("acos", arguments, cx)),
        "atan" => Some(mathop("atan", arguments, cx)),
        "not" => Some(not(arguments, cx)),
        "and" => {
            let [lhs, rhs] = arguments.try_into().ok().unwrap();
            let lhs = lhs.b(&mut cx.sprite);
            let rhs = rhs.b(&mut cx.sprite);
            Some(B(cx.sprite.and(lhs, rhs)))
        }
        "or" => {
            let [lhs, rhs] = arguments.try_into().ok().unwrap();
            let lhs = lhs.b(&mut cx.sprite);
            let rhs = rhs.b(&mut cx.sprite);
            Some(B(cx.sprite.or(lhs, rhs)))
        }
        "join" => f! { = join(lhs, rhs) },
        "answer" => f! { = answer() },
        "ask" => f! { ask(question) },
        "broadcast-and-wait" => f! { broadcast_and_wait(message) },
        "change-x" => f! { change_x(amount) },
        "change-y" => f! { change_y(amount) },
        "clone-self" => {
            cx.sprite.clone_self();
            None
        }
        "erase-all" => f! { erase_all() },
        "go-to" => f! { go_to_xy(x, y) },
        "go-to-back-layer" => f! { go_to_back_layer() },
        "go-to-front-layer" => f! { go_to_front_layer() },
        "hide" => f! { hide() },
        "move" => f! { move_steps(steps) },
        "mouse-x" => f! { = mouse_x() },
        "mouse-y" => f! { = mouse_y() },
        "pen-down" => f! { pen_down() },
        "pen-up" => f! { pen_up() },
        "pressing-key" => f! { = B key_is_pressed(key) },
        "random" => f! { = random(low, high) },
        "reset-timer" => f! { reset_timer() },
        "say" if arguments.len() == 1 => f! { say(message) },
        "say" => f! { say_for_seconds(seconds, message) },
        "set-costume" => f! { set_costume(costume) },
        "set-pen-color" => f! { set_pen_color(color) },
        "set-pen-size" => f! { set_pen_size(size) },
        "set-size" => f! { set_size(size) },
        "set-x" => f! { set_x(x) },
        "set-y" => f! { set_y(y) },
        "show" => f! { show() },
        "stamp" => f! { stamp() },
        "stop-all" => f! { stop_all() },
        "timer" => f! { = timer() },
        "wait" => f! { wait(seconds) },
        "x-pos" => f! { = x_position() },
        "y-pos" => f! { = y_position() },
        _ => panic!("invalid intrinsic: {name:?}"),
    }
}

fn not(
    arguments: Vec<CompiledOperand>,
    cx: &mut Context<'_>,
) -> CompiledOperand {
    let [operand] = arguments.try_into().ok().unwrap();
    let operand = operand.b(&mut cx.sprite);
    B(cx.sprite.not(operand))
}

fn mathop(
    operation: &'static str,
    arguments: Vec<CompiledOperand>,
    cx: &mut Context,
) -> CompiledOperand {
    let [operand] = arguments.try_into().ok().unwrap();
    S(cx.sprite.mathop(operation, operand.s()))
}
