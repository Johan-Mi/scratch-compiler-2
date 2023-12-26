use crate::{
    diagnostics::{primary, span, Diagnostics},
    hir,
};
use codemap::File;
use rowan::TextSize;
use std::{collections::HashMap, fmt};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Ty {
    Unit,
    Num,
    Ty,
    Var(Box<Self>),
}

impl fmt::Display for Ty {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Unit => write!(f, "Unit"),
            Self::Num => write!(f, "Num"),
            Self::Ty => write!(f, "Ty"),
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
}

pub fn check(
    document: &hir::Document,
    file: &File,
    diagnostics: &mut Diagnostics,
) {
    for sprite in &document.sprites {
        let mut tcx = Context {
            sprite,
            file,
            diagnostics,
            variable_types: HashMap::new(),
        };

        for function in &sprite.functions {
            check_function(function, &mut tcx);
        }
    }
}

fn check_function(function: &hir::Function, tcx: &mut Context) {
    tcx.variable_types.extend(function.parameters.iter().map(
        |(parameter, internal_name)| {
            (internal_name.text_range().start(), parameter.ty.clone())
        },
    ));

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
                    span(tcx.file, function.name.text_range()),
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
    pub file: &'a File,
    pub diagnostics: &'a mut Diagnostics,
    pub variable_types: HashMap<TextSize, Result<Ty, ()>>,
}
