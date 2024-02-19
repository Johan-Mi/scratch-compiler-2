use super::{
    Block, Document, Function, Generator, Op, Parameter, RealVar, Sprite, Value,
};
use crate::{
    function::{self, ResolvedCalls},
    hir::{self, desugar_function_call_name},
    name::{self, Name},
};
use rowan::TextSize;
use std::collections::HashMap;

pub fn lower(
    document: hir::Document,
    resolved_calls: &ResolvedCalls,
    generator: &mut Generator,
) -> Document {
    let top_level_functions = document
        .functions
        .iter()
        .map(|(&index, function)| (index, function.into()))
        .collect();

    let mut cx = Context {
        vars: HashMap::new(),
        real_vars: super::mutability::real_vars(&document, generator),
        generator,
        resolved_calls,
        sprite_functions: HashMap::new(),
        top_level_functions,
        block: Block::default(),
    };

    Document {
        sprites: document
            .sprites
            .into_iter()
            .map(|(name, sprite)| (name, lower_sprite(sprite, &mut cx)))
            .collect(),
        functions: document
            .functions
            .into_iter()
            .filter(|(_, function)| !function.is_builtin)
            .map(|(index, function)| (index, lower_function(function, &mut cx)))
            .collect(),
    }
}

#[derive(Clone, Copy)]
pub struct FunctionSignature {
    returns_something: bool,
    is_builtin: bool,
}

#[allow(clippy::fallible_impl_from)]
impl From<&hir::Function> for FunctionSignature {
    fn from(function: &hir::Function) -> Self {
        Self {
            returns_something: !function
                .return_ty
                .as_ref()
                .unwrap()
                .is_zero_sized(),
            is_builtin: function.is_builtin,
        }
    }
}

struct Context<'a> {
    vars: HashMap<TextSize, Value>,
    real_vars: HashMap<TextSize, RealVar>,
    generator: &'a mut Generator,
    resolved_calls: &'a ResolvedCalls,
    sprite_functions: HashMap<usize, FunctionSignature>,
    top_level_functions: HashMap<usize, FunctionSignature>,
    block: Block,
}

fn lower_sprite(sprite: hir::Sprite, cx: &mut Context) -> Sprite {
    cx.sprite_functions.clear();
    cx.sprite_functions.extend(
        sprite
            .functions
            .iter()
            .map(|(&index, function)| (index, function.into())),
    );

    Sprite {
        costumes: sprite.costumes,
        functions: sprite
            .functions
            .into_iter()
            .map(|(index, function)| (index, lower_function(function, cx)))
            .collect(),
    }
}

fn lower_function(function: hir::Function, cx: &mut Context) -> Function {
    let parameters = function
        .parameters
        .into_iter()
        .map(|param| {
            let ssa_var = cx.generator.new_ssa_var();
            cx.vars.insert(
                param.internal_name.text_range().start(),
                Value::Var(ssa_var),
            );
            Parameter {
                ssa_var,
                ty: param.ty.node.unwrap(),
            }
        })
        .collect();

    Function {
        name: function.name.node,
        parameters,
        body: lower_block(function.body, cx),
        returns_something: !function.return_ty.node.unwrap().is_zero_sized(),
        is_inline: function.is_inline,
    }
}

fn lower_block(block: hir::Block, cx: &mut Context) -> Block {
    let old_block = std::mem::take(&mut cx.block);
    if let Some(return_value) = block
        .statements
        .into_iter()
        .map(|statement| lower_statement(statement, cx))
        .last()
        .flatten()
    {
        cx.block.ops.push(Op::Return(return_value));
    }
    std::mem::replace(&mut cx.block, old_block)
}

fn lower_statement(
    statement: hir::Statement,
    cx: &mut Context,
) -> Option<Value> {
    match statement {
        hir::Statement::Let { variable, value } => {
            let value = lower_expression(value, cx).unwrap();
            if let Some(&real_var) =
                cx.real_vars.get(&variable.text_range().start())
            {
                cx.block.ops.push(Op::CallBuiltin {
                    variable: None,
                    name: "set".to_owned(),
                    args: vec![Value::Lvalue(real_var), value],
                });
            } else {
                cx.vars.insert(variable.text_range().start(), value);
            }
            None
        }
        hir::Statement::If {
            condition,
            then,
            else_,
        } => {
            let condition = lower_expression(condition, cx).unwrap();
            let then = lower_block(then.unwrap(), cx);
            let else_ = else_
                .map(|else_| lower_block(else_, cx))
                .unwrap_or_default();
            cx.block.ops.push(Op::If {
                condition,
                then,
                else_,
            });
            None
        }
        hir::Statement::Repeat { times, body } => {
            let times = lower_expression(times, cx).unwrap();
            let body = lower_block(body.unwrap(), cx);
            cx.block.ops.push(Op::For {
                variable: None,
                times,
                body,
            });
            None
        }
        hir::Statement::Forever { body, .. } => {
            let body = lower_block(body.unwrap(), cx);
            cx.block.ops.push(Op::Forever { body });
            None
        }
        hir::Statement::While { condition, body } => {
            let condition = lower_expression(condition, cx).unwrap();
            let body = lower_block(body.unwrap(), cx);
            cx.block.ops.push(Op::While { condition, body });
            None
        }
        hir::Statement::Until { condition, body } => {
            // desugar `until condition { ... }`
            //    into `while not(condition) { ... }`
            let condition = lower_expression(condition, cx).unwrap();
            let not_condition = cx.generator.new_ssa_var();
            cx.block.ops.push(Op::CallBuiltin {
                variable: Some(not_condition),
                name: "not".to_owned(),
                args: vec![condition],
            });
            let body = lower_block(body.unwrap(), cx);
            cx.block.ops.push(Op::While {
                condition: Value::Var(not_condition),
                body,
            });
            None
        }
        hir::Statement::For {
            variable,
            times,
            body,
        } => {
            let times = lower_expression(times, cx).unwrap();
            let var = cx.generator.new_ssa_var();
            cx.vars.insert(
                variable.unwrap().text_range().start(),
                Value::Var(var),
            );
            let body = lower_block(body.unwrap(), cx);
            cx.block.ops.push(Op::For {
                variable: Some(var),
                times,
                body,
            });
            None
        }
        hir::Statement::Expr(expr) => lower_expression(expr, cx),
        hir::Statement::Error => unreachable!(),
    }
}

fn lower_expression(expr: hir::Expression, cx: &mut Context) -> Option<Value> {
    match expr.kind {
        hir::ExpressionKind::Variable(Name::User(variable)) => {
            if let Some(&real_var) =
                cx.real_vars.get(&variable.text_range().start())
            {
                let ssa_var = cx.generator.new_ssa_var();
                cx.block.ops.push(Op::CallBuiltin {
                    variable: Some(ssa_var),
                    name: "get".to_owned(),
                    args: vec![Value::Lvalue(real_var)],
                });
                Some(Value::Var(ssa_var))
            } else {
                Some(cx.vars[&variable.text_range().start()].clone())
            }
        }
        hir::ExpressionKind::Variable(Name::Builtin(builtin)) => {
            match builtin {
                name::Builtin::Unit
                | name::Builtin::Num
                | name::Builtin::String
                | name::Builtin::Bool
                | name::Builtin::Var
                | name::Builtin::List
                | name::Builtin::Type => unreachable!(),
            }
        }
        hir::ExpressionKind::Imm(imm) => Some(Value::Imm(imm)),
        hir::ExpressionKind::FunctionCall {
            name_or_operator,
            arguments,
        } => {
            let name = desugar_function_call_name(&name_or_operator);
            let args = arguments
                .into_iter()
                .map(|(_, arg)| lower_expression(arg, cx).unwrap())
                .collect::<Vec<_>>();

            let function_ref = cx.resolved_calls[&expr.span.low()];
            let signature = match function_ref {
                function::Ref::SpriteLocal(index) => {
                    cx.sprite_functions[&index]
                }
                function::Ref::TopLevel(index) => {
                    cx.top_level_functions[&index]
                }
            };

            let variable = signature
                .returns_something
                .then(|| cx.generator.new_ssa_var());
            cx.block.ops.push(if signature.is_builtin {
                Op::CallBuiltin {
                    variable,
                    name: name.to_owned(),
                    args,
                }
            } else {
                Op::Call {
                    variable,
                    function: function_ref,
                    args,
                }
            });

            variable.map(Value::Var)
        }
        hir::ExpressionKind::Lvalue(var) => {
            Some(Value::Lvalue(cx.real_vars[&var]))
        }
        hir::ExpressionKind::ListLiteral(elements) => {
            let list = cx.generator.new_real_list();
            // TODO: clear list first
            for element in elements {
                let element = lower_expression(element, cx).unwrap();
                cx.block.ops.push(Op::CallBuiltin {
                    variable: None,
                    name: "push".to_owned(),
                    args: vec![Value::List(list), element],
                });
            }
            Some(Value::List(list))
        }
        hir::ExpressionKind::GenericTypeInstantiation { .. }
        | hir::ExpressionKind::Error => unreachable!(),
    }
}
