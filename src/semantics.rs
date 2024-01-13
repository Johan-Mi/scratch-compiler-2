use crate::{
    diagnostics::{primary, Diagnostics},
    function,
    hir::{self, Visitor},
    ty::Ty,
};

pub fn check(document: &hir::Document, diagnostics: &mut Diagnostics) {
    if !document.sprites.contains_key("Stage") {
        diagnostics.error("project has no stage", []);
        diagnostics.help("try creating a stage: `sprite Stage {}`", []);
    }

    SemanticVisitor { diagnostics }.traverse_document(document);
}

struct SemanticVisitor<'a> {
    diagnostics: &'a mut Diagnostics,
}

impl Visitor for SemanticVisitor<'_> {
    fn visit_function(&mut self, function: &hir::Function, is_top_level: bool) {
        if is_top_level && function::name_is_special(&function.name) {
            self.diagnostics.error(
                format!(
                    "special function `{}` cannot be defined outside of a sprite",
                    *function.name
                ),
                [primary(function.name.span, "")],
            );
        }

        match &**function.name {
            "when-flag-clicked" => {
                if !function.parameters.is_empty()
                    || function
                        .return_ty
                        .as_ref()
                        .is_ok_and(|it| *it != Ty::Unit)
                {
                    self.diagnostics.error(
                    "special function `when-flag-clicked` has incorrect signature",
                    [primary(function.name.span, "")],
                );
                    self.diagnostics.note(
                        "expected signature: `fn when-flag-clicked()`",
                        [],
                    );
                }
            }
            _ => {}
        }
    }

    fn visit_block(&mut self, block: &hir::Block) {
        let Some(span) = block
            .statements
            .iter()
            // A `forever` loop at the end of the block is fine.
            .take(block.statements.len().saturating_sub(1))
            .find_map(|statement| match statement {
                hir::Statement::Forever { span, .. } => Some(*span),
                _ => None,
            })
        else {
            return;
        };
        self.diagnostics.error(
            "unreachable code",
            [primary(span, "any code after this loop is unreachable")],
        );
    }
}
