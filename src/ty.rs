use crate::{
    hir,
    name::{self, Name},
    parser::SyntaxToken,
};
use codemap::Spanned;
use std::fmt;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Ty {
    Never,
    Unit,
    Num,
    String,
    Bool,
    Sprite,
    #[expect(clippy::enum_variant_names, reason = "`Type` is a type")]
    Ty,
    Var(Box<Self>),
    List(Box<Self>),
    Generic(SyntaxToken),
    // TODO: which struct is it?
    Struct {
        name: Spanned<String>,
    },
}

impl fmt::Display for Ty {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Never => write!(f, "Never"),
            Self::Unit => write!(f, "Unit"),
            Self::Num => write!(f, "Num"),
            Self::String => write!(f, "String"),
            Self::Bool => write!(f, "Bool"),
            Self::Sprite => write!(f, "Sprite"),
            Self::Ty => write!(f, "Type"),
            Self::Var(inner) => write!(f, "Var[{inner}]"),
            Self::List(inner) => write!(f, "List[{inner}]"),
            Self::Generic(token) => write!(f, "{token}"),
            Self::Struct { name } => write!(f, "{}", name.node),
        }
    }
}

impl TryFrom<name::Builtin> for Ty {
    type Error = ();

    fn try_from(builtin: name::Builtin) -> Result<Self, ()> {
        match builtin {
            name::Builtin::Never => Ok(Self::Never),
            name::Builtin::Unit => Ok(Self::Unit),
            name::Builtin::Num => Ok(Self::Num),
            name::Builtin::String => Ok(Self::String),
            name::Builtin::Bool => Ok(Self::Bool),
            name::Builtin::Type => Ok(Self::Ty),
            name::Builtin::Var | name::Builtin::List => Err(()),
        }
    }
}

impl Ty {
    pub const fn is_zero_sized(&self) -> bool {
        matches!(self, Self::Never | Self::Unit)
    }

    pub const fn has_runtime_repr(&self) -> bool {
        matches!(
            self,
            Self::Num | Self::String | Self::Bool | Self::Struct { .. }
            // Generics are limited to runtime types for now.
            | Self::Generic(_)
        ) || self.is_zero_sized()
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Generic {
    Var,
    List,
}

impl fmt::Display for Generic {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Self::Var => "Var",
            Self::List => "List",
        })
    }
}

impl TryFrom<hir::Expression> for Generic {
    type Error = ();

    fn try_from(expr: hir::Expression) -> Result<Self, ()> {
        match expr.kind {
            hir::ExpressionKind::Variable(Name::Builtin(name::Builtin::Var)) => Ok(Self::Var),
            hir::ExpressionKind::Variable(Name::Builtin(name::Builtin::List)) => Ok(Self::List),
            _ => Err(()),
        }
    }
}
