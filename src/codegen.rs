use crate::{comptime, function, mir, ty::Ty};
use sb3_builder::{
    block, Costume, CustomBlock, InsertionPoint, Operand, Parameter,
    ParameterKind, Project, Target, Variable, VariableRef,
};
use std::{
    collections::{hash_map::Entry, HashMap, HashSet},
    path::Path,
};

type Result<T> = std::result::Result<T, Box<dyn std::error::Error>>;

pub fn generate(document: mir::Document, output_path: &Path) -> Result<()> {
    let mut project = Project::default();

    for (name, sprite) in document.sprites {
        compile_sprite(sprite, name, &document.functions, &mut project)?;
    }

    let file = std::fs::File::create(output_path)?;
    project.finish(file)
}

fn compile_sprite(
    mir: mir::Sprite,
    name: String,
    top_level_functions: &HashMap<usize, mir::Function>,
    project: &mut Project,
) -> Result<()> {
    let mut sprite = if name == "Stage" {
        project.stage()
    } else {
        project.add_sprite(name)
    };

    for costume in mir.costumes {
        sprite.add_costume(Costume::from_file(
            costume.name,
            costume.path.as_ref(),
        )?);
    }

    let mut compiled_ssa_vars = HashMap::new();

    let sprite_functions_refs =
        mir.functions.iter().map(|(&index, function)| {
            (function::Ref::SpriteLocal(index), function)
        });
    let top_level_functions_refs = top_level_functions
        .iter()
        .map(|(&index, function)| (function::Ref::TopLevel(index), function));
    let compiled_functions = sprite_functions_refs
        .chain(top_level_functions_refs)
        .filter_map(|(ref_, function)| {
            Some(ref_).zip(CompiledFunctionRef::new(
                function,
                &mut sprite,
                &mut compiled_ssa_vars,
            ))
        })
        .collect();

    let mut cx = Context {
        sprite,
        compiled_ssa_vars,
        compiled_functions,
        return_variable: None,
        is_linear: HashSet::new(),
    };

    for (index, function) in mir.functions {
        compile_function(function, function::Ref::SpriteLocal(index), &mut cx);
    }

    Ok(())
}

struct Context<'a> {
    sprite: Target<'a>,
    compiled_ssa_vars: HashMap<mir::SsaVar, CompiledSsaVar>,
    compiled_functions: HashMap<function::Ref, CompiledFunctionRef>,
    return_variable: Option<VariableRef>,
    is_linear: HashSet<mir::SsaVar>,
}

enum CompiledSsaVar {
    Linear(Operand),
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
        if function::name_is_special(&function.name) {
            return None;
        }

        let parameters = function
            .parameters
            .iter()
            .filter_map(|parameter| {
                let kind = match parameter.ty {
                    Ty::Unit => return None,
                    Ty::Num | Ty::String => ParameterKind::StringOrNumber,
                    Ty::Bool => ParameterKind::Boolean,
                    Ty::Ty | Ty::Var(_) => unreachable!(),
                };
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
            })
        });

        Some(Self {
            block,
            insertion_point: Some(insertion_point),
            return_variable,
        })
    }
}

fn compile_function(
    mut function: mir::Function,
    function_ref: function::Ref,
    cx: &mut Context,
) {
    cx.return_variable = match &*function.name {
        "when-flag-clicked" => {
            cx.sprite.start_script(block::when_flag_clicked());
            None
        }
        _ => {
            let compiled_ref =
                cx.compiled_functions.get_mut(&function_ref).unwrap();
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
        mir::Op::Return(value) => {
            let value = compile_value(value, cx);
            let return_variable = cx.return_variable.clone().unwrap();
            cx.sprite.put(block::set_variable(return_variable, value));
        }
        mir::Op::If {
            condition,
            then,
            else_,
        } => {
            let condition = compile_value(condition, cx);
            let condition = cx.sprite.eq(condition, "true".to_owned().into());
            let [after, else_clause] = cx.sprite.if_else(condition);
            compile_block(then, cx);
            cx.sprite.insert_at(else_clause);
            compile_block(else_, cx);
            cx.sprite.insert_at(after);
        }
        mir::Op::Forever { body } => {
            cx.sprite.forever();
            compile_block(body, cx);
        }
        mir::Op::While { condition, body } => {
            let condition = compile_value(condition, cx);
            let condition = cx.sprite.eq(condition, "true".to_owned().into());
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
                });
                cx.compiled_ssa_vars
                    .insert(variable, CompiledSsaVar::Relevant(var.clone()));
                cx.sprite.for_(var, times)
            } else {
                cx.sprite.repeat(times)
            };
            compile_block(body, cx);
            cx.sprite.insert_at(after);
        }
        mir::Op::Call {
            variable,
            function,
            args,
        } => {
            let args =
                args.into_iter().map(|arg| compile_value(arg, cx)).collect();
            let compiled_ref = &cx.compiled_functions[&function];
            cx.sprite.use_custom_block(&compiled_ref.block, args);
            if let Some(variable) = variable {
                let return_variable =
                    compiled_ref.return_variable.clone().unwrap().into();

                if cx.is_linear.contains(&variable) {
                    cx.compiled_ssa_vars.insert(
                        variable,
                        CompiledSsaVar::Linear(return_variable),
                    );
                } else {
                    let var = cx.sprite.add_variable(Variable {
                        name: variable.to_string(),
                    });
                    cx.sprite
                        .put(block::set_variable(var.clone(), return_variable));
                    cx.compiled_ssa_vars
                        .insert(variable, CompiledSsaVar::Relevant(var));
                }
            }
        }
        mir::Op::CallBuiltin {
            variable,
            name,
            args,
        } => {
            let args =
                args.into_iter().map(|arg| compile_value(arg, cx)).collect();
            let res = compile_builtin_function_call(&name, args, cx);
            if let Some(variable) = variable {
                if cx.is_linear.contains(&variable) {
                    cx.compiled_ssa_vars
                        .insert(variable, CompiledSsaVar::Linear(res.unwrap()));
                } else {
                    let var = cx.sprite.add_variable(Variable {
                        name: variable.to_string(),
                    });
                    cx.sprite
                        .put(block::set_variable(var.clone(), res.unwrap()));
                    cx.compiled_ssa_vars
                        .insert(variable, CompiledSsaVar::Relevant(var));
                }
            }
        }
    }
}

fn compile_value(value: mir::Value, cx: &mut Context) -> Operand {
    match value {
        mir::Value::Var(var) => {
            let Entry::Occupied(mut entry) = cx.compiled_ssa_vars.entry(var)
            else {
                panic!("undefined SSA variable: {var:?}");
            };
            match entry.get_mut() {
                CompiledSsaVar::Relevant(var_ref) => var_ref.clone().into(),
                CompiledSsaVar::CustomBlockParameter(param) => {
                    cx.sprite.custom_block_parameter(param.clone())
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
        mir::Value::Imm(comptime::Value::Ty(_)) => unreachable!(),
        mir::Value::Imm(comptime::Value::Num(n)) => n.into(),
        mir::Value::Imm(comptime::Value::String(s)) => s.into(),
        // TODO: booleans are tricky since Scratch doesn't have literals,
        // variables or lists for them. We'll probably need a parameter to
        // specify what kind of slot the expression will be used in so we
        // can choose between leaving it empty, using an empty `not` or the
        // strings "false"/"true".
        mir::Value::Imm(comptime::Value::Bool(_)) => todo!(),
    }
}

fn compile_builtin_function_call(
    name: &str,
    arguments: Vec<Operand>,
    cx: &mut Context,
) -> Option<Operand> {
    match name {
        "go-to" => {
            let [x, y] = arguments.try_into().ok().unwrap();
            cx.sprite.put(block::go_to_xy(x, y));
            None
        }
        "add" => {
            let [lhs, rhs] = arguments.try_into().ok().unwrap();
            Some(cx.sprite.add(lhs, rhs))
        }
        "sub" => {
            let [lhs, rhs] = arguments.try_into().ok().unwrap();
            Some(cx.sprite.sub(lhs, rhs))
        }
        "mul" => {
            let [lhs, rhs] = arguments.try_into().ok().unwrap();
            Some(cx.sprite.mul(lhs, rhs))
        }
        "div" => {
            let [lhs, rhs] = arguments.try_into().ok().unwrap();
            Some(cx.sprite.div(lhs, rhs))
        }
        "mod" => {
            let [lhs, rhs] = arguments.try_into().ok().unwrap();
            Some(cx.sprite.modulo(lhs, rhs))
        }
        "lt" => {
            let [lhs, rhs] = arguments.try_into().ok().unwrap();
            Some(cx.sprite.lt(lhs, rhs))
        }
        "eq" => {
            let [lhs, rhs] = arguments.try_into().ok().unwrap();
            Some(cx.sprite.eq(lhs, rhs))
        }
        "gt" => {
            let [lhs, rhs] = arguments.try_into().ok().unwrap();
            Some(cx.sprite.gt(lhs, rhs))
        }
        _ => unreachable!(),
    }
}
