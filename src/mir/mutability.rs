use super::{Generator, RealVar};
use crate::hir::{self, Visitor};
use rowan::TextSize;
use std::collections::HashMap;

pub(super) fn real_vars(
    document: &hir::Document,
    generator: &mut Generator,
) -> HashMap<TextSize, RealVar> {
    let mut visitor = RealVarVisitor {
        real_vars: HashMap::new(),
        generator,
    };
    visitor.traverse_document(document);
    visitor.real_vars
}

struct RealVarVisitor<'a> {
    real_vars: HashMap<TextSize, RealVar>,
    generator: &'a mut Generator,
}

impl Visitor for RealVarVisitor<'_> {
    fn visit_expression(&mut self, expr: &hir::Expression) {
        if let hir::ExpressionKind::Lvalue(var) = expr.kind {
            self.real_vars.insert(var, self.generator.new_real_var());
        }
    }
}
