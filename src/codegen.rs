use crate::{
    comptime::Value,
    function, hir,
    name::{self, Name},
    parser::SyntaxToken,
    ty::Ty,
};
use codemap::Pos;
use sb3_builder::{
    block, Costume, CustomBlockRef, InsertionPoint, Operand, Parameter,
    ParameterKind, Project, Target, Variable, VariableRef,
};
use std::{collections::HashMap, path::Path};

type Result<T> = std::result::Result<T, Box<dyn std::error::Error>>;

pub fn generate(
    document: hir::Document,
    resolved_calls: &HashMap<Pos, function::Ref>,
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
    resolved_calls: &HashMap<Pos, function::Ref>,
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
    let compiled_functions = sprite_functions_refs
        .chain(top_level_functions_refs)
        .filter_map(|(ref_, function)| {
            Some(ref_).zip(CompiledFunctionRef::new(function, &mut sprite))
        })
        .collect();

    let mut cx = Context {
        sprite,
        variables: HashMap::new(),
        resolved_calls,
        compiled_functions,
    };

    for function in hir.functions {
        compile_function(function, &mut cx);
    }

    Ok(())
}

struct Context<'a> {
    sprite: Target<'a>,
    variables: HashMap<SyntaxToken, VariableRef>,
    resolved_calls: &'a HashMap<Pos, function::Ref>,
    compiled_functions: HashMap<function::Ref, CompiledFunctionRef>,
}

enum CompiledFunctionRef {
    User {
        block: CustomBlockRef,
        insertion_point: InsertionPoint,
    },
    Builtin,
}

impl CompiledFunctionRef {
    fn new(function: &hir::Function, sprite: &mut Target) -> Option<Self> {
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
                .collect();
            let (block, insertion_point) =
                sprite.add_custom_block(function.name.to_string(), parameters);
            Some(Self::User {
                block,
                insertion_point,
            })
        }
    }
}

fn compile_function(hir: hir::Function, cx: &mut Context) {
    match &**hir.name {
        "when-flag-clicked" => {
            cx.sprite.start_script(block::when_flag_clicked());
            for statement in hir.body.statements {
                compile_statement(statement, cx);
            }
        }
        _ => todo!(),
    }
}

fn compile_statement(hir: hir::Statement, cx: &mut Context) {
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
        }
        hir::Statement::Expr(expr) => {
            let operand = compile_expression(&expr, cx);
            // Don't create orphaned reporters; they're useless.
            // TODO: write a pass that extracts the non-pure parts of expression
            // statements to satisfy this assertion
            debug_assert!(operand.is_none());
        }
        hir::Statement::Error => unreachable!(),
    }
}

fn compile_expression(
    hir: &hir::Expression,
    cx: &mut Context,
) -> Option<Operand> {
    match &hir.kind {
        hir::ExpressionKind::Variable(name) => match name {
            Name::User(token) => Some(cx.variables[token].clone().into()),
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
        hir::ExpressionKind::BinaryOperation { lhs, operator, rhs } => {
            let lhs = compile_expression(lhs, cx).unwrap();
            let rhs = compile_expression(rhs, cx).unwrap();
            Some(match operator {
                hir::BinaryOperator::Add => cx.sprite.add(lhs, rhs),
                hir::BinaryOperator::Sub => cx.sprite.sub(lhs, rhs),
                hir::BinaryOperator::Mul => cx.sprite.mul(lhs, rhs),
                hir::BinaryOperator::Div => cx.sprite.div(lhs, rhs),
                hir::BinaryOperator::Mod => cx.sprite.modulo(lhs, rhs),
            })
        }
        hir::ExpressionKind::FunctionCall { name, arguments } => {
            let arguments = arguments
                .iter()
                .map(|(_, arg)| {
                    compile_expression(arg, cx)
                        // TODO: write a pass that removes `Unit` parameters/arguments
                        // since they don't exist at runtime
                        .unwrap()
                })
                .collect();
            compile_function_call(name.text(), hir.span.low(), arguments, cx)
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
        CompiledFunctionRef::User { block, .. } => {
            cx.sprite.use_custom_block(*block, &arguments);
            // TODO: pass return value somehow
            None
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
        _ => unreachable!(),
    }
}
