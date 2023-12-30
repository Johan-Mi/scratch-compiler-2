use crate::{comptime::Value, hir, parser::SyntaxToken};
use sb3_builder::{
    block, Costume, Operand, Project, Target, Variable, VariableRef,
};
use std::{collections::HashMap, path::Path};

type Result<T> = std::result::Result<T, Box<dyn std::error::Error>>;

pub fn generate(document: hir::Document, output_path: &Path) -> Result<()> {
    let mut project = Project::default();

    for (name, sprite) in document.sprites {
        compile_sprite(sprite, name, &mut project)?;
    }

    let file = std::fs::File::create(output_path)?;
    project.finish(file)
}

fn compile_sprite(
    hir: hir::Sprite,
    name: String,
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
    };

    for function in hir.functions {
        compile_function(function, &mut cx);
    }

    Ok(())
}

struct Context<'a> {
    sprite: Target<'a>,
    variables: HashMap<SyntaxToken, VariableRef>,
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
        hir::ExpressionKind::Variable(_) => todo!(),
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
        hir::ExpressionKind::FunctionCall { name, arguments } => todo!(),
        hir::ExpressionKind::Error => unreachable!(),
    }
}
