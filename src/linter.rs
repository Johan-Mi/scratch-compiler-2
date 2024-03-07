use crate::{
    diagnostics::{primary, span, Diagnostics},
    hir::{Document, ExpressionKind, Statement, Visitor},
    name::Name,
    parser::SyntaxToken,
};
use codemap::File;
use std::collections::HashSet;

pub fn lint(document: &Document, file: &File, diagnostics: &mut Diagnostics) {
    let mut visitor = LintVisitor {
        unused_variables: HashSet::new(),
    };
    for variable in &document.variables {
        visitor.unused_variables.insert(variable.token.clone());
    }
    visitor.traverse_document(document);
    for variable in &visitor.unused_variables {
        diagnostics.warning(
            "unused variable",
            [primary(span(file, variable.text_range()), "")],
        );
    }
}

struct LintVisitor {
    unused_variables: HashSet<SyntaxToken>,
}

impl Visitor for LintVisitor {
    fn visit_statement(&mut self, statement: &Statement) {
        if let Statement::Let { variable, .. } = statement {
            self.unused_variables.insert(variable.clone());
        }
    }

    fn visit_expression(&mut self, expr: &crate::hir::Expression) {
        if let ExpressionKind::Variable(Name::User(variable)) = &expr.kind {
            self.unused_variables.remove(variable);
        }
    }
}
