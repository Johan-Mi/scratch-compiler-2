use super::{
    Block, Call, Document, Function, Generator, Op, Parameter, RealList,
    RealVar, Value,
};
use crate::{
    function::ResolvedCalls,
    hir::{self, desugar_function_call_name},
    name::{self, Name},
    parser::SyntaxToken,
};
use std::collections::{HashMap, HashSet};

pub fn lower(
    document: hir::typed::Document,
    resolved_calls: &ResolvedCalls,
    generator: &mut Generator,
) -> (Document, HashSet<RealVar>, HashSet<RealList>) {
    let functions = document
        .functions
        .iter()
        .map(|(&index, function)| (index, function.into()))
        .collect();

    let mut cx = Context {
        vars: HashMap::new(),
        real_vars: super::mutability::real_vars(&document, generator),
        generator,
        resolved_calls,
        functions,
        block: Block::default(),
    };

    let mut stage_variables = HashSet::new();
    for variable in document.variables {
        lower_variable_initialization(
            variable.token.clone(),
            variable.initializer,
            &mut cx,
        );

        if variable.belongs_to_stage {
            stage_variables.extend(cx.real_vars.get(&variable.token));
        }
    }

    let functions = document
        .functions
        .into_iter()
        .map(|(index, function)| (index, lower_function(function, &mut cx)))
        .collect();

    let document = Document {
        sprites: document.sprites,
        functions,
    };

    // TODO
    let stage_lists = HashSet::new();

    (document, stage_variables, stage_lists)
}

#[derive(Clone, Copy)]
pub struct FunctionSignature {
    returns_something: bool,
    is_intrinsic: bool,
}

#[expect(clippy::fallible_impl_from)]
impl From<&hir::typed::Function> for FunctionSignature {
    fn from(function: &hir::typed::Function) -> Self {
        Self {
            returns_something: !function
                .return_ty
                .as_ref()
                .unwrap()
                .is_zero_sized(),
            is_intrinsic: function.is_intrinsic,
        }
    }
}

struct Context<'a> {
    vars: HashMap<SyntaxToken, Value>,
    real_vars: HashMap<SyntaxToken, RealVar>,
    generator: &'a mut Generator,
    resolved_calls: &'a ResolvedCalls,
    functions: HashMap<usize, FunctionSignature>,
    block: Block,
}

fn lower_function(
    function: hir::typed::Function,
    cx: &mut Context,
) -> Function {
    let parameters = function
        .parameters
        .into_iter()
        .map(|param| {
            let ssa_var = cx.generator.new_ssa_var();
            cx.vars.insert(param.internal_name, Value::Var(ssa_var));
            Parameter {
                ssa_var,
                ty: param.ty.node.unwrap(),
            }
        })
        .collect();

    Function {
        name: function.name.node,
        tag: function.tag,
        parameters,
        body: lower_block(function.body, cx),
        returns_something: !function.return_ty.node.unwrap().is_zero_sized(),
        is_inline: function.is_inline,
        owning_sprite: function.owning_sprite,
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
        cx.block.ops.push(Op::Return {
            value: return_value,
            is_explicit: false,
        });
    }
    std::mem::replace(&mut cx.block, old_block)
}

fn lower_variable_initialization(
    variable: SyntaxToken,
    initializer: hir::Expression,
    cx: &mut Context,
) {
    let value = lower_expression(initializer, cx).unwrap();
    if let Some(&real_var) = cx.real_vars.get(&variable) {
        cx.block.ops.push(Op::Call(
            None,
            Call::Intrinsic {
                name: "set".to_owned(),
                args: vec![Value::Lvalue(real_var), value],
            },
        ));
    } else {
        cx.vars.insert(variable, value);
    }
}

fn lower_statement(
    statement: hir::Statement,
    cx: &mut Context,
) -> Option<Value> {
    match statement.kind {
        hir::StatementKind::Let { variable, value } => {
            lower_variable_initialization(variable, value, cx);
            None
        }
        hir::StatementKind::If {
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
        hir::StatementKind::Repeat { times, body } => {
            let times = lower_expression(times, cx).unwrap();
            let body = lower_block(body.unwrap(), cx);
            cx.block.ops.push(Op::For {
                variable: None,
                times,
                body,
            });
            None
        }
        hir::StatementKind::Forever { body, .. } => {
            let body = lower_block(body.unwrap(), cx);
            cx.block.ops.push(Op::Forever { body });
            None
        }
        hir::StatementKind::While { condition, body } => {
            let condition = lower_expression(condition, cx).unwrap();
            let body = lower_block(body.unwrap(), cx);
            cx.block.ops.push(Op::While { condition, body });
            None
        }
        hir::StatementKind::Until { condition, body } => {
            // desugar `until condition { ... }`
            //    into `while not(condition) { ... }`
            let condition = lower_expression(condition, cx).unwrap();
            let not_condition = cx.generator.new_ssa_var();
            cx.block.ops.push(Op::Call(
                Some(not_condition),
                Call::Intrinsic {
                    name: "not".to_owned(),
                    args: vec![condition],
                },
            ));
            let body = lower_block(body.unwrap(), cx);
            cx.block.ops.push(Op::While {
                condition: Value::Var(not_condition),
                body,
            });
            None
        }
        hir::StatementKind::For {
            variable,
            times,
            body,
        } => {
            let times = lower_expression(times, cx).unwrap();
            let var = cx.generator.new_ssa_var();
            cx.vars.insert(variable.unwrap(), Value::Var(var));
            let body = lower_block(body.unwrap(), cx);
            cx.block.ops.push(Op::For {
                variable: Some(var),
                times,
                body,
            });
            None
        }
        hir::StatementKind::Return(value) => {
            let value = lower_expression(value, cx).unwrap();
            cx.block.ops.push(Op::Return {
                value,
                is_explicit: true,
            });
            None
        }
        hir::StatementKind::Expr(expr) => lower_expression(expr, cx),
        hir::StatementKind::Error => unreachable!(),
    }
}

fn lower_expression(expr: hir::Expression, cx: &mut Context) -> Option<Value> {
    match expr.kind {
        hir::ExpressionKind::Variable(Name::User(variable)) => {
            if let Some(&real_var) = cx.real_vars.get(&variable) {
                let ssa_var = cx.generator.new_ssa_var();
                cx.block.ops.push(Op::Call(
                    Some(ssa_var),
                    Call::Intrinsic {
                        name: "get".to_owned(),
                        args: vec![Value::Lvalue(real_var)],
                    },
                ));
                Some(Value::Var(ssa_var))
            } else {
                Some(cx.vars[&variable].clone())
            }
        }
        hir::ExpressionKind::Variable(Name::Builtin(builtin)) => {
            match builtin {
                name::Builtin::Never
                | name::Builtin::Unit
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
            name_span,
            arguments,
            ..
        } => {
            let args = arguments
                .into_iter()
                .map(|(_, arg)| lower_expression(arg, cx).unwrap())
                .collect::<Vec<_>>();

            let function = cx.resolved_calls[&name_span.low()];
            let signature = cx.functions[&function];

            let variable = signature
                .returns_something
                .then(|| cx.generator.new_ssa_var());
            cx.block.ops.push(Op::Call(
                variable,
                if signature.is_intrinsic {
                    let name = desugar_function_call_name(&name_or_operator);
                    Call::Intrinsic {
                        name: name.to_owned(),
                        args,
                    }
                } else {
                    Call::Custom { function, args }
                },
            ));

            variable.map(Value::Var)
        }
        hir::ExpressionKind::Lvalue(var) => {
            Some(Value::Lvalue(cx.real_vars[&var]))
        }
        hir::ExpressionKind::ListLiteral(elements) => {
            let list = cx.generator.new_real_list();
            cx.block.ops.push(Op::Call(
                None,
                Call::Intrinsic {
                    name: "delete-all".to_owned(),
                    args: vec![Value::List(list)],
                },
            ));
            for element in elements {
                let element = lower_expression(element, cx).unwrap();
                cx.block.ops.push(Op::Call(
                    None,
                    Call::Intrinsic {
                        name: "push".to_owned(),
                        args: vec![Value::List(list), element],
                    },
                ));
            }
            Some(Value::List(list))
        }
        hir::ExpressionKind::TypeAscription { inner, .. } => {
            lower_expression(*inner, cx)
        }
        hir::ExpressionKind::GenericTypeInstantiation { .. }
        | hir::ExpressionKind::Error => unreachable!(),
    }
}
