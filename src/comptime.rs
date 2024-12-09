use crate::{hir::Expression, parser::SyntaxToken, ty::Ty};
use std::fmt;

#[derive(Clone)]
pub enum Value<L = SyntaxToken> {
    Ty(Ty),
    Num(f64),
    String(String),
    Bool(bool),
    Sprite {
        name: String,
    },
    VariableRef(L),
    ListRef {
        token: SyntaxToken,
        initializer: Option<Vec<Expression>>,
    },
}

impl<L: fmt::Debug> fmt::Debug for Value<L> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Ty(ty) => fmt::Debug::fmt(ty, f),
            Self::Num(n) => fmt::Debug::fmt(n, f),
            Self::String(s) => fmt::Debug::fmt(s, f),
            Self::Bool(b) => fmt::Debug::fmt(b, f),
            Self::Sprite { name } => f.debug_struct("Sprite").field("name", name).finish(),
            Self::VariableRef(var) => write!(f, "&{var:?}"),
            Self::ListRef { token, initializer } => f
                .debug_struct("ListRef")
                .field("token", token)
                .field("initializer", initializer)
                .finish(),
        }
    }
}
