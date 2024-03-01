use crate::{
    hir::{Expression, ExpressionKind},
    name::{Builtin, Name},
    parser::SyntaxKind,
    ty::{self, Ty},
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

pub fn evaluate(expr: Expression) -> Result<Value, ()> {
    match expr.kind {
        ExpressionKind::Variable(Name::User(token)) => {
            match token.parent().map(|it| it.kind()) {
                Some(SyntaxKind::SPRITE) => Ok(Value::Sprite {
                    name: token.to_string(),
                }),
                Some(SyntaxKind::GENERICS) => Ok(Value::Ty(Ty::Generic(token))),
                _ => Err(()),
            }
        }
        ExpressionKind::Variable(Name::Builtin(builtin)) => match builtin {
            Builtin::Unit => Ok(Value::Ty(Ty::Unit)),
            Builtin::Num => Ok(Value::Ty(Ty::Num)),
            Builtin::String => Ok(Value::Ty(Ty::String)),
            Builtin::Bool => Ok(Value::Ty(Ty::Bool)),
            Builtin::Var | Builtin::List => Err(()),
            Builtin::Type => Ok(Value::Ty(Ty::Ty)),
        },
        ExpressionKind::Imm(value) => Ok(value),
        ExpressionKind::GenericTypeInstantiation { generic, arguments } => {
            let Ok([arg]) = <[Expression; 1]>::try_from(arguments) else {
                return Err(());
            };
            let Value::Ty(ty) = evaluate(arg)? else {
                return Err(());
            };
            Ok(Value::Ty(match generic {
                ty::Generic::Var => Ty::Var(Box::new(ty)),
                ty::Generic::List => Ty::List(Box::new(ty)),
            }))
        }
        ExpressionKind::TypeAscription { inner, .. } => evaluate(*inner),
        ExpressionKind::FunctionCall { .. }
        | ExpressionKind::Lvalue(_)
        | ExpressionKind::ListLiteral(_)
        | ExpressionKind::Error => Err(()),
    }
}

// FIXME: get rid of this and use actual compile-time evaluation instead
pub const fn is_known(expr: &Expression) -> bool {
    matches!(
        expr.kind,
        ExpressionKind::Imm(_)
            | ExpressionKind::Variable(_)
            | ExpressionKind::Lvalue(_)
    )
}
