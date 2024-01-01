use crate::{
    diagnostics::{primary, Diagnostics},
    function, hir,
};
use codemap::{File, Pos};
use rowan::TextSize;
use std::{collections::HashMap, fmt};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Ty {
    Unit,
    Num,
    String,
    Bool,
    #[allow(clippy::enum_variant_names)]
    Ty,
    Var(Box<Self>),
}

impl fmt::Display for Ty {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Unit => write!(f, "Unit"),
            Self::Num => write!(f, "Num"),
            Self::String => write!(f, "String"),
            Self::Bool => write!(f, "Bool"),
            Self::Ty => write!(f, "Type"),
            Self::Var(inner) => write!(f, "Var[{inner}]"),
        }
    }
}

impl Ty {
    pub fn is_subtype_of(&self, other: &Self) -> bool {
        //          v <: t
        // ------   -----------
        // t <: t   Var[v] <: t

        self == other
            || matches!(self, Self::Var(inner) if inner.is_subtype_of(other))
    }

    pub fn is_zero_sized(&self) -> bool {
        *self == Self::Unit
    }
}

pub fn check(
    document: &hir::Document,
    file: &File,
    diagnostics: &mut Diagnostics,
) -> HashMap<Pos, function::Ref> {
    let mut resolved_calls = HashMap::new();

    for sprite in document.sprites.values() {
        let mut tcx = Context {
            sprite,
            top_level_functions: &document.functions,
            file,
            diagnostics,
            variable_types: HashMap::new(),
            resolved_calls: &mut resolved_calls,
        };

        for function in &sprite.functions {
            check_function(function, &mut tcx);
        }
    }

    resolved_calls
}

fn check_function(function: &hir::Function, tcx: &mut Context) {
    tcx.variable_types
        .extend(function.parameters.iter().map(|parameter| {
            (
                parameter.internal_name.text_range().start(),
                parameter.ty.clone(),
            )
        }));

    let actual_return_ty = function
        .body
        .statements
        .iter()
        .map(|statement| check_statement(statement, tcx))
        .last()
        .unwrap_or(Ok(Ty::Unit));
    if let (Ok(return_ty), Ok(actual_return_ty)) =
        (&function.return_ty, actual_return_ty)
    {
        if !actual_return_ty.is_subtype_of(return_ty) {
            tcx.diagnostics.error(
                "function has wrong return type",
                [primary(
                    function.name.span,
                    format!("according to the signature, this function should return `{return_ty}` but it actually returns `{actual_return_ty}`"),
                )],
            );
        }
    }
}

fn check_statement(
    statement: &hir::Statement,
    tcx: &mut Context<'_>,
) -> Result<Ty, ()> {
    match statement {
        hir::Statement::Let { variable, value } => {
            let ty = value.ty(tcx).map(Box::new).map(Ty::Var);
            tcx.variable_types.insert(variable.text_range().start(), ty);
            Ok(Ty::Unit)
        }
        hir::Statement::Expr(expr) => expr.ty(tcx),
        hir::Statement::Error => Err(()),
    }
}

pub struct Context<'a> {
    pub sprite: &'a hir::Sprite,
    pub top_level_functions: &'a [hir::Function],
    pub file: &'a File,
    pub diagnostics: &'a mut Diagnostics,
    pub variable_types: HashMap<TextSize, Result<Ty, ()>>,
    pub resolved_calls: &'a mut HashMap<Pos, function::Ref>,
}
