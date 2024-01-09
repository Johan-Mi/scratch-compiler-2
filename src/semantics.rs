use crate::{
    diagnostics::{primary, Diagnostics},
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
        if is_top_level && function.is_special() {
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
        let Some(index) = block.statements.iter().position(|statement| {
            matches!(statement, hir::Statement::Forever { .. })
        }) else {
            return;
        };

        if index == block.statements.len() - 1 {
            return;
        }

        let hir::Statement::Forever { span, .. } = block.statements[index]
        else {
            unreachable!()
        };
        self.diagnostics.error(
            "unreachable code",
            [primary(span, "any code after this loop is unreachable")],
        );
    }
}
