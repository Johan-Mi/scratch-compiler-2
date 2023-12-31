use crate::{
    comptime::Value,
    function, hir,
    name::{self, Name},
    parser::SyntaxToken,
};
use codemap::Pos;
use sb3_builder::{
    block, Costume, CustomBlockRef, InsertionPoint, Operand, Project, Target,
    Variable, VariableRef,
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
        compile_sprite(sprite, name, resolved_calls, &mut project)?;
    }

    let file = std::fs::File::create(output_path)?;
    project.finish(file)
}

fn compile_sprite(
    hir: hir::Sprite,
    name: String,
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

    let mut cx = Context {
        sprite,
        variables: HashMap::new(),
        resolved_calls,
        compiled_functions: HashMap::new(),
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
    compiled_functions: HashMap<function::Ref, CompiledFunction>,
}

enum CompiledFunction {
    User {
        block: CustomBlockRef,
        insertion_point: InsertionPoint,
    },
    Builtin,
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
                | name::Builtin::Type => unreachable!(),
            },
        },
        hir::ExpressionKind::Imm(value) => match value {
            Value::Num(n) => Some((*n).into()),
            Value::String(s) => Some(s.clone().into()),
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
        CompiledFunction::User { block, .. } => {
            cx.sprite.use_custom_block(*block, &arguments);
            // TODO: pass return value somehow
            None
        }
        CompiledFunction::Builtin => {
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
