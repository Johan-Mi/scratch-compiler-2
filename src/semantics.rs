use crate::{
    diagnostics::{primary, Diagnostics},
    function,
    hir::{
        self,
        typed::{Document, Function},
        ExpressionKind, Visitor,
    },
    ty::Ty,
};

pub fn check(document: &Document, diagnostics: &mut Diagnostics) {
    if !document.sprites.contains_key("Stage") {
        diagnostics.error("project has no stage", []);
        diagnostics.help("try creating a stage: `sprite Stage {}`", []);
    }

    SemanticVisitor {
        diagnostics,
        is_inline: false,
    }
    .traverse_document(document);
}

struct SemanticVisitor<'a> {
    diagnostics: &'a mut Diagnostics,
    is_inline: bool,
}

impl Visitor for SemanticVisitor<'_> {
    fn visit_function(&mut self, function: &Function) {
        self.is_inline = function.kind.is_inline();
        self.check_generics(function);
        self.check_special_function(function);
        self.check_function_staging(function);
    }

    fn visit_block(&mut self, block: &hir::Block) {
        let Some(span) = block
            .statements
            .iter()
            // A `forever` loop at the end of the block is fine.
            .take(block.statements.len().saturating_sub(1))
            .find_map(|statement| {
                matches!(statement.kind, hir::StatementKind::Forever { .. })
                    .then_some(statement.span)
            })
        else {
            return;
        };
        self.diagnostics.error(
            "unreachable code",
            [primary(span, "any code after this loop is unreachable")],
        );
    }

    fn visit_statement(&mut self, statement: &hir::Statement) {
        if self.is_inline && matches!(statement.kind, hir::StatementKind::Return(_)) {
            self.diagnostics.error(
                "`return` is not supported in inline functions yet",
                [primary(statement.span, "")],
            );
        }
    }

    fn visit_expression(&mut self, expr: &hir::Expression) {
        if let ExpressionKind::FunctionCall {
            name_or_operator, ..
        } = &expr.kind
        {
            if function::Special::try_from(name_or_operator.text()).is_ok() {
                self.diagnostics.error(
                    format!("special function `{name_or_operator}` cannot be called"),
                    [primary(expr.span, "")],
                );
            }
        }
    }
}

impl SemanticVisitor<'_> {
    fn check_special_function(&mut self, function: &Function) {
        let Ok(special) = function::Special::try_from(&**function.name) else {
            return;
        };

        if function.owning_sprite.is_none() {
            self.diagnostics.error(
                format!(
                    "special function `{}` cannot be defined outside of a sprite",
                    *function.name
                ),
                [primary(function.name.span, "")],
            );
        }

        if function.kind.is_inline() {
            self.diagnostics.error(
                format!("special function `{}` cannot be inline", *function.name),
                [primary(function.name.span, "")],
            );
        }

        if let Some(expected_signature) = match special {
            function::Special::WhenFlagClicked => (!function.parameters.is_empty()
                || function.return_ty.as_ref().is_ok_and(|it| *it != Ty::Unit))
            .then_some("fn when-flag-clicked"),
            function::Special::WhenKeyPressed => (!function.parameters.is_empty()
                || function.return_ty.as_ref().is_ok_and(|it| *it != Ty::Unit)
                || function.tag.is_none())
            .then_some("fn when-key-pressed \"key-name\""),
            function::Special::WhenCloned => (!function.parameters.is_empty()
                || function.return_ty.as_ref().is_ok_and(|it| *it != Ty::Unit))
            .then_some("fn when-cloned"),
            function::Special::WhenReceived => (!function.parameters.is_empty()
                || function.return_ty.as_ref().is_ok_and(|it| *it != Ty::Unit)
                || function.tag.is_none())
            .then_some("fn when-received \"message\""),
        } {
            self.diagnostics.error(
                format!(
                    "special function `{}` has incorrect signature",
                    *function.name,
                ),
                [primary(function.name.span, "")],
            );
            self.diagnostics
                .note(format!("expected signature: `{expected_signature}`"), []);
        }
    }

    fn check_function_staging(&mut self, function: &Function) {
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

        if matches!(
            function.kind,
            hir::FunctionKind::Regular { is_inline: false }
        ) {
            for param in &function.parameters {
                if param.is_comptime {
                    self.diagnostics.error(
                        "comptime parameters are not supported in \
                        user-defined, non-inline functions yet",
                        [primary(param.span, "")],
                    );
                }
            }
        }
    }

    fn check_generics(&mut self, function: &Function) {
        if matches!(function.kind, hir::FunctionKind::Regular { .. })
            && !function.generics.is_empty()
        {
            self.diagnostics.error(
                "user-defined functions with generics are not supported yet",
                [primary(function.name.span, "")],
            );
        }
    }
}
