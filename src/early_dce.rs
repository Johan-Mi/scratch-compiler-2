//! Dead code elimination performed before any other optimizations to avoid
//! wasting time optimizing unused functions.

use crate::{
    diagnostics::{primary, Diagnostics},
    function::{self, ResolvedCalls},
    hir::{
        typed::{Document, Function},
        ExpressionKind, FunctionKind, Visitor,
    },
};
use std::collections::BTreeSet;

pub fn perform(
    document: &mut Document,
    resolved_calls: &ResolvedCalls,
    diagnostics: &mut Diagnostics,
) {
    let mut visitor = DceVisitor {
        resolved_calls,
        pending_functions: document
            .functions
            .iter()
            .filter(|(_, function)| function::Special::try_from(&**function.name).is_ok())
            .map(|(&index, _)| index)
            .collect(),
        required_functions: BTreeSet::new(),
    };

    while let Some(index) = visitor.pending_functions.pop_last() {
        let _: bool = visitor.required_functions.insert(index);
        visitor.traverse_function(&document.functions[&index]);
    }
    document.functions.retain(|index, function| {
        visitor.required_functions.contains(index) || {
            warn(diagnostics, function);
            false
        }
    });
}

fn warn(diagnostics: &mut Diagnostics, function: &Function) {
    let message = match function.kind {
        FunctionKind::Regular { .. } => "unused function",
        FunctionKind::Intrinsic => return,
        FunctionKind::Constructor => "struct is never constructed",
    };
    diagnostics.warning(message, [primary(function.name.span, "")]);
}

struct DceVisitor<'a> {
    resolved_calls: &'a ResolvedCalls,
    pending_functions: BTreeSet<usize>,
    required_functions: BTreeSet<usize>,
}

impl Visitor for DceVisitor<'_> {
    fn visit_expression(&mut self, expr: &crate::hir::Expression) {
        let ExpressionKind::FunctionCall { name_span, .. } = expr.kind else {
            return;
        };
        let Some(&function) = self.resolved_calls.get(&name_span.low()) else {
            return;
        };
        if !self.required_functions.contains(&function) {
            let _: bool = self.pending_functions.insert(function);
        }
    }
}
