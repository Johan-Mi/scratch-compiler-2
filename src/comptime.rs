use crate::{
    diagnostics::{primary, Diagnostics},
    hir::{Expression, ExpressionKind},
    name::{Builtin, Name},
    parser::SyntaxKind,
    ty::Ty,
};
use std::fmt;

#[derive(Clone)]
pub enum Value {
    Ty(Ty),
    Num(f64),
    String(String),
    Bool(bool),
    Sprite { name: String },
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Ty(ty) => fmt::Debug::fmt(ty, f),
            Self::Num(n) => fmt::Debug::fmt(n, f),
            Self::String(s) => fmt::Debug::fmt(s, f),
            Self::Bool(b) => fmt::Debug::fmt(b, f),
            Self::Sprite { name } => {
                f.debug_struct("Sprite").field("name", name).finish()
            }
        }
    }
}

impl Value {
    pub const fn ty(&self) -> Ty {
        match self {
            Self::Ty(_) => Ty::Ty,
            Self::Num(_) => Ty::Num,
            Self::String(_) => Ty::String,
            Self::Bool(_) => Ty::Bool,
            Self::Sprite { .. } => Ty::Sprite,
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
        ExpressionKind::Variable(Name::User(token)) => {
            match token.parent().map(|it| it.kind()) {
                Some(SyntaxKind::SPRITE) => Ok(Value::Sprite {
                    name: token.to_string(),
                }),
                Some(SyntaxKind::GENERICS) => Ok(Value::Ty(Ty::Generic(token))),
                _ => error(
                    "user-defined variables are not supported at compile-time",
                ),
            }
        }
        ExpressionKind::Variable(Name::Builtin(builtin)) => match builtin {
            Builtin::Unit => Ok(Value::Ty(Ty::Unit)),
            Builtin::Num => Ok(Value::Ty(Ty::Num)),
            Builtin::String => Ok(Value::Ty(Ty::String)),
            Builtin::Bool => Ok(Value::Ty(Ty::Bool)),
            Builtin::Var => {
                error("generic type `Var` must have one type parameter applied")
            }
            Builtin::Type => Ok(Value::Ty(Ty::Ty)),
        },
        ExpressionKind::Imm(value) => Ok(value),
        ExpressionKind::FunctionCall { .. } => {
            error("function calls are not supported at compile-time")
        }
        ExpressionKind::Lvalue(_) => {
            error("mutable variables are not supported at compile-time")
        }
        ExpressionKind::Error => Err(()),
    }
}
