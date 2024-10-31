use super::{Generator, RealVar};
use crate::{
    hir::{self, Visitor},
    parser::SyntaxToken,
};
use std::collections::HashMap;

pub(super) fn real_vars(
    document: &hir::typed::Document,
    generator: &mut Generator,
) -> HashMap<SyntaxToken, RealVar> {
    let mut visitor = RealVarVisitor {
        real_vars: HashMap::new(),
        generator,
    };
    visitor.traverse_document(document);
    visitor.real_vars
}

struct RealVarVisitor<'a> {
    real_vars: HashMap<SyntaxToken, RealVar>,
    generator: &'a mut Generator,
}

impl Visitor for RealVarVisitor<'_> {
    fn visit_expression(&mut self, expr: &hir::Expression) {
        if let hir::ExpressionKind::Lvalue(var) = &expr.kind {
            let _: &mut RealVar = self
                .real_vars
                .entry(var.clone())
                .or_insert_with(|| self.generator.new_real_var());
        }
    }
}
