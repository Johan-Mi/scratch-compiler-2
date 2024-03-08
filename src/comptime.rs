use crate::{
    hir::{Expression, ExpressionKind},
    name::Name,
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

pub fn evaluate(expr: &mut Expression) {
    match &mut expr.kind {
        ExpressionKind::Variable(Name::User(token)) => {
            expr.kind =
                ExpressionKind::Imm(match token.parent().map(|it| it.kind()) {
                    Some(SyntaxKind::SPRITE) => Value::Sprite {
                        name: token.to_string(),
                    },
                    Some(SyntaxKind::GENERICS) => {
                        Value::Ty(Ty::Generic(token.clone()))
                    }
                    _ => return,
                });
        }
        ExpressionKind::Variable(Name::Builtin(builtin)) => {
            if let Ok(ty) = (*builtin).try_into() {
                expr.kind = ExpressionKind::Imm(Value::Ty(ty));
            }
        }
        ExpressionKind::GenericTypeInstantiation { generic, arguments } => {
            if let [Expression {
                kind: ExpressionKind::Imm(Value::Ty(ty)),
                ..
            }] = &mut **arguments
            {
                expr.kind = ExpressionKind::Imm(Value::Ty(match generic {
                    ty::Generic::Var => Ty::Var(Box::new(ty.clone())),
                    ty::Generic::List => Ty::List(Box::new(ty.clone())),
                }));
            }
        }
        _ => {}
    }
}

// FIXME: get rid of this and use actual compile-time evaluation instead
pub fn is_known(expr: &Expression, tcx: &ty::Context) -> bool {
    matches!(
        expr.kind,
        ExpressionKind::Imm(_) | ExpressionKind::Lvalue(_)
    ) || matches!(
        &expr.kind,
        ExpressionKind::Variable(Name::User(var))
            if tcx.comptime_known_variables.contains(&var.text_range().start())
    ) || matches!(
        &expr.kind,
        ExpressionKind::ListLiteral(items)
            if items.iter().all(|item| is_known(item, tcx))
    ) || matches!(
        &expr.kind,
        ExpressionKind::TypeAscription { inner, .. }
            if is_known(inner, tcx)
    )
}
