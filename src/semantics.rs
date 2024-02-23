use crate::{
    diagnostics::{primary, Diagnostics},
    function,
    hir::{self, ExpressionKind, Visitor},
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
        self.check_generics(function);
        self.check_special_function(is_top_level, function);
        self.check_function_staging(function);
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

    fn visit_expression(&mut self, expr: &hir::Expression) {
        if let ExpressionKind::FunctionCall {
            name_or_operator, ..
        } = &expr.kind
        {
            if function::name_is_special(name_or_operator.text()) {
                self.diagnostics.error(format!("special function `{name_or_operator}` cannot be called"), [primary(expr.span, "")]);
            }
        }
    }
}

impl SemanticVisitor<'_> {
    fn check_special_function(
        &mut self,
        is_top_level: bool,
        function: &hir::Function,
    ) {
        if is_top_level && function::name_is_special(&function.name) {
            self.diagnostics.error(
                format!(
                    "special function `{}` cannot be defined outside of a sprite",
                    *function.name
                ),
                [primary(function.name.span, "")],
            );
        }

        if function.is_inline && function::name_is_special(&function.name) {
            self.diagnostics.error(
                format!(
                    "special function `{}` cannot be inline",
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

    fn check_function_staging(&mut self, function: &hir::Function) {
        for ty in function
            .parameters
            .iter()
            .filter(|param| !param.is_comptime)
            .map(|param| &param.ty)
            .chain([&function.return_ty])
        {
            let span = ty.span;
            let Ok(ty) = &ty.node else { continue };
            if !ty.has_runtime_repr() {
                self.diagnostics.error(
                    format!("type `{ty}` cannot be used at runtime"),
                    [primary(span, "")],
                );
            }
        }
    }

    fn check_generics(&mut self, function: &hir::Function) {
        if !function.is_inline && function.generics.is_some() {
            self.diagnostics.error(
                "user-defined functions with generics are not supported yet",
                [primary(function.name.span, "")],
            );
        }
    }
}
