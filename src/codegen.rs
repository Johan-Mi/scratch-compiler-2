use crate::{
    comptime::Value,
    function::{self, ResolvedCalls},
    hir,
    name::{self, Name},
    parser::SyntaxToken,
    ty::Ty,
};
use codemap::Pos;
use sb3_builder::{
    block, Costume, CustomBlock, InsertionPoint, Operand, Parameter,
    ParameterKind, Project, Target, Variable, VariableRef,
};
use std::{collections::HashMap, path::Path};

type Result<T> = std::result::Result<T, Box<dyn std::error::Error>>;

pub fn generate(
    document: hir::Document,
    resolved_calls: &ResolvedCalls,
    output_path: &Path,
) -> Result<()> {
    let mut project = Project::default();

    for (name, sprite) in document.sprites {
        compile_sprite(
            sprite,
            name,
            &document.functions,
            resolved_calls,
            &mut project,
        )?;
    }

    let file = std::fs::File::create(output_path)?;
    project.finish(file)
}

fn compile_sprite(
    hir: hir::Sprite,
    name: String,
    top_level_functions: &[hir::Function],
    resolved_calls: &ResolvedCalls,
    project: &mut Project,
) -> Result<()> {
    let mut sprite = if name == "Stage" {
        project.stage()
    } else {
        project.add_sprite(name)
    };

    for costume in hir.costumes {
        sprite.add_costume(Costume::from_file(
            costume.name,
            costume.path.as_ref(),
        )?);
    }

    let sprite_functions_refs =
        hir.functions.iter().enumerate().map(|(index, function)| {
            (function::Ref::SpriteLocal(index), function)
        });
    let top_level_functions_refs = top_level_functions
        .iter()
        .enumerate()
        .map(|(index, function)| (function::Ref::TopLevel(index), function));
    let mut custom_block_parameters = HashMap::new();
    let compiled_functions = sprite_functions_refs
        .chain(top_level_functions_refs)
        .filter_map(|(ref_, function)| {
            Some(ref_).zip(CompiledFunctionRef::new(
                function,
                &mut sprite,
                &mut custom_block_parameters,
            ))
        })
        .collect();
    let mut cx = Context {
        sprite,
        variables: HashMap::new(),
        custom_block_parameters,
        resolved_calls,
        compiled_functions,
    };

    for (index, function) in hir.functions.into_iter().enumerate() {
        if !function.is_dead {
            compile_function(
                function,
                function::Ref::SpriteLocal(index),
                &mut cx,
            );
        }
    }

    Ok(())
}

struct Context<'a> {
    sprite: Target<'a>,
    variables: HashMap<SyntaxToken, VariableRef>,
    custom_block_parameters: HashMap<SyntaxToken, Parameter>,
    resolved_calls: &'a ResolvedCalls,
    compiled_functions: HashMap<function::Ref, CompiledFunctionRef>,
}

enum CompiledFunctionRef {
    User {
        block: CustomBlock,
        insertion_point: Option<InsertionPoint>,
        return_variable: Option<VariableRef>,
    },
    Builtin,
}

impl CompiledFunctionRef {
    fn new(
        function: &hir::Function,
        sprite: &mut Target,
        custom_block_parameters: &mut HashMap<SyntaxToken, Parameter>,
    ) -> Option<Self> {
        if function.is_builtin {
            Some(Self::Builtin)
        } else if function.is_special() {
            None
        } else {
            let parameters = function
                .parameters
                .iter()
                .filter_map(|parameter| match parameter.ty.as_ref().unwrap() {
                    Ty::Unit => None,
                    Ty::Num | Ty::String => Some(Parameter {
                        name: parameter.internal_name.to_string(),
                        kind: ParameterKind::StringOrNumber,
                    }),
                    Ty::Bool => Some(Parameter {
                        name: parameter.internal_name.to_string(),
                        kind: ParameterKind::Boolean,
                    }),
                    Ty::Ty | Ty::Var(_) => unreachable!(),
                })
                .collect::<Vec<_>>();

            for (custom_block_param, function_param) in
                std::iter::zip(&parameters, &function.parameters)
            {
                custom_block_parameters.insert(
                    function_param.internal_name.clone(),
                    custom_block_param.clone(),
                );
            }

            let (block, insertion_point) =
                sprite.add_custom_block(function.name.to_string(), parameters);

            let return_variable =
                (!function.return_ty.as_ref().unwrap().is_zero_sized()).then(
                    || {
                        sprite.add_variable(Variable {
                            name: format!("return {}", *function.name),
                        })
                    },
                );

            Some(Self::User {
                block,
                insertion_point: Some(insertion_point),
                return_variable,
            })
        }
    }
}

fn compile_function(hir: hir::Function, ref_: function::Ref, cx: &mut Context) {
    let return_variable = match &**hir.name {
        "when-flag-clicked" => {
            cx.sprite.start_script(block::when_flag_clicked());
            None
        }
        _ => {
            let (insertion_point, return_variable) = match cx
                .compiled_functions
                .get_mut(&ref_)
                .unwrap()
            {
                CompiledFunctionRef::User {
                    insertion_point,
                    return_variable,
                    ..
                } => (insertion_point.take().unwrap(), return_variable.clone()),
                CompiledFunctionRef::Builtin => return,
            };
            cx.sprite.insert_at(insertion_point);
            return_variable
        }
    };

    let return_value = hir
        .body
        .statements
        .into_iter()
        .filter_map(|it| compile_statement(it, cx))
        .last();
    if let Some(return_variable) = return_variable {
        cx.sprite
            .put(block::set_variable(return_variable, return_value.unwrap()));
    }
}

fn compile_statement(hir: hir::Statement, cx: &mut Context) -> Option<Operand> {
    match hir {
        hir::Statement::Let {
            variable: token,
            value,
        } => {
            let value = compile_expression(&value, cx).unwrap();
            let var = cx.sprite.add_variable(Variable {
                name: token.to_string(),
            });
            cx.variables.insert(token, var.clone());
            cx.sprite.put(block::set_variable(var, value));
            None
        }
        hir::Statement::If { condition, then } => {
            let condition = compile_expression(&condition, cx).unwrap();
            let after = cx.sprite.if_(condition);
            for statement in then.unwrap().statements {
                compile_statement(statement, cx);
            }
            cx.sprite.insert_at(after);
            None
        }
        hir::Statement::Expr(expr) => compile_expression(&expr, cx),
        hir::Statement::Error => unreachable!(),
    }
}

fn compile_expression(
    hir: &hir::Expression,
    cx: &mut Context,
) -> Option<Operand> {
    match &hir.kind {
        hir::ExpressionKind::Variable(name) => match name {
            Name::User(token) => {
                Some(cx.variables.get(token).cloned().map_or_else(
                    || {
                        cx.sprite.custom_block_parameter(
                            cx.custom_block_parameters[token].clone(),
                        )
                    },
                    Operand::from,
                ))
            }
            Name::Builtin(builtin) => match builtin {
                // TODO: emit an error for this during semantic analysis
                name::Builtin::Unit
                | name::Builtin::Num
                | name::Builtin::String
                | name::Builtin::Bool
                | name::Builtin::Type => unreachable!(),
            },
        },
        hir::ExpressionKind::Imm(value) => match value {
            Value::Num(n) => Some((*n).into()),
            Value::String(s) => Some(s.clone().into()),
            // TODO: booleans are tricky since Scratch doesn't have literals,
            // variables or lists for them. We'll probably need a parameter to
            // specify what kind of slot the expression will be used in so we
            // can choose between leaving it empty, using an empty `not` or the
            // strings "false"/"true".
            Value::Bool(_) => todo!(),
            // TODO: emit an error for this during semantic analysis
            Value::Ty(_) => unreachable!(),
        },
        hir::ExpressionKind::FunctionCall {
            name_or_operator: name,
            arguments,
        } => {
            let name = hir::desugar_function_call_name(name);
            let arguments = arguments
                .iter()
                .map(|(_, arg)| {
                    compile_expression(arg, cx)
                        // TODO: write a pass that removes `Unit` parameters/arguments
                        // since they don't exist at runtime
                        .unwrap()
                })
                .collect();
            compile_function_call(name, hir.span.low(), arguments, cx)
        }
        hir::ExpressionKind::Error => unreachable!(),
    }
}

fn compile_function_call(
    name: &str,
    pos: Pos,
    arguments: Vec<Operand>,
    cx: &mut Context,
) -> Option<Operand> {
    let function = &cx.compiled_functions[&cx.resolved_calls[&pos]];
    match function {
        CompiledFunctionRef::User {
            block,
            return_variable,
            ..
        } => {
            cx.sprite.use_custom_block(block, arguments);
            return_variable.clone().map(Operand::from)
        }
        CompiledFunctionRef::Builtin => {
            compile_builtin_function_call(name, arguments, cx)
        }
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
