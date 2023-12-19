use crate::{
    diagnostics::{primary, Diagnostics},
    hir::{Expression, ExpressionKind},
    name::{Builtin, Name},
};

#[derive(Debug)]
pub enum Value {
    Ty(Ty),
}

#[derive(Debug)]
pub enum Ty {
    Unit,
    Num,
}

pub fn evaluate(
    expr: Expression,
    diagnostics: &mut Diagnostics,
) -> Result<Value, ()> {
    let mut error = |message| {
        diagnostics.error(message, [primary(expr.span, "")]);
        Err(())
    };

    match expr.kind {
        ExpressionKind::Variable(Name::User(_)) => {
            error("user-defined variables are not supported at compile-time")
        }
        ExpressionKind::Variable(Name::Builtin(builtin)) => match builtin {
            Builtin::Num => Ok(Value::Ty(Ty::Num)),
        },
        ExpressionKind::Imm(value) => Ok(value),
        ExpressionKind::BinaryOperation { .. } => {
            error("binary operations are not supported at compile-time")
        }
        ExpressionKind::FunctionCall { .. } => {
            error("function calls are not supported at compile-time")
        }
        ExpressionKind::Error => Err(()),
    }
}
