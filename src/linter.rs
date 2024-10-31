use crate::{
    diagnostics::{primary, span, Diagnostics},
    hir::{
        Document, ExpressionKind, GlobalVariable, Statement, StatementKind,
        Visitor,
    },
    name::Name,
    parser::SyntaxToken,
};
use codemap::File;
use std::collections::HashSet;

pub fn lint(document: &Document, file: &File, diagnostics: &mut Diagnostics) {
    let mut visitor = LintVisitor {
        unused_variables: HashSet::new(),
    };
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

impl Visitor<crate::hir::Function> for LintVisitor {
    fn visit_global_variable(&mut self, variable: &GlobalVariable) {
        let _: bool = self.unused_variables.insert(variable.token.clone());
    }

    fn visit_statement(&mut self, statement: &Statement) {
        if let StatementKind::Let { variable, .. } = &statement.kind {
            let _: bool = self.unused_variables.insert(variable.clone());
        }
    }

    fn visit_expression(&mut self, expr: &crate::hir::Expression) {
        if let ExpressionKind::Variable(Name::User(variable)) = &expr.kind {
            let _: bool = self.unused_variables.remove(variable);
        }
    }
}
