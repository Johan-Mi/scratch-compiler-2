use crate::{
    diagnostics::{primary, Diagnostics},
    hir::{Expression, ExpressionKind},
    name::{Builtin, Name},
    ty::Ty,
};

#[derive(Debug)]
pub enum Value {
    Ty(Ty),
    Num(f64),
    String(String),
    Bool(bool),
}

impl Value {
    pub const fn ty(&self) -> Ty {
        match self {
            Self::Ty(_) => Ty::Ty,
            Self::Num(_) => Ty::Num,
            Self::String(_) => Ty::String,
            Self::Bool(_) => Ty::Bool,
        }
    }
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
            Builtin::Unit => Ok(Value::Ty(Ty::Unit)),
            Builtin::Num => Ok(Value::Ty(Ty::Num)),
            Builtin::String => Ok(Value::Ty(Ty::String)),
            Builtin::Bool => Ok(Value::Ty(Ty::Bool)),
            Builtin::Type => Ok(Value::Ty(Ty::Ty)),
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
