use crate::{
    diagnostics::{primary, span, Diagnostics},
    hir,
};
use codemap::File;
use std::fmt;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Ty {
    Unit,
    Num,
    Ty,
}

impl fmt::Display for Ty {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Self::Unit => "`Unit`",
            Self::Num => "`Num`",
            Self::Ty => "`Ty`",
        })
    }
}

impl Ty {
    pub fn is_subtype_of(&self, other: &Self) -> bool {
        self == other
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
        };

        for function in &sprite.functions {
            check_function(function, &mut tcx);
        }
    }
}

fn check_function(function: &hir::Function, tcx: &mut Context) {
    let actual_return_ty = function
        .body
        .statements
        .iter()
        .map(|statement| match statement {
            hir::Statement::Let { value, .. } => {
                let _ = value.ty(tcx);
                Ok(Ty::Unit)
            }
            hir::Statement::Expr(expr) => expr.ty(tcx),
            hir::Statement::Error => Err(()),
        })
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
                    format!("according to the signature, this function should return {return_ty} but it actually returns {actual_return_ty}"),
                )],
            );
        }
    }
}

pub struct Context<'a> {
    pub sprite: &'a hir::Sprite,
    pub file: &'a File,
    pub diagnostics: &'a mut Diagnostics,
}
