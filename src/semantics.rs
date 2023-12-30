use crate::{
    diagnostics::{primary, Diagnostics},
    hir,
    ty::Ty,
};

pub fn check(document: &hir::Document, diagnostics: &mut Diagnostics) {
    for sprite in document.sprites.values() {
        for function in &sprite.functions {
            check_function(function, diagnostics);
        }
    }
}

fn check_function(function: &hir::Function, diagnostics: &mut Diagnostics) {
    match &**function.name {
        "when-flag-clicked" => {
            if !function.parameters.is_empty()
                || function.return_ty.as_ref().is_ok_and(|it| *it != Ty::Unit)
            {
                diagnostics.error(
                    "special function `when-flag-clicked` has incorrect signature",
                    [primary(function.name.span, "")],
                );
                diagnostics
                    .note("expected signature: `fn when-flag-clicked()`", []);
            }
        }
        _ => {}
    }
}
