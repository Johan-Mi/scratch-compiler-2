//! Dead code elimination performed before any other optimizations to avoid
//! wasting time optimizing unused functions.

use crate::{
    diagnostics::{primary, Diagnostics},
    function::{self, ResolvedCalls},
    hir::{Document, ExpressionKind, Function, Sprite, Visitor},
};
use std::collections::BTreeSet;

pub fn perform(
    document: &mut Document,
    resolved_calls: &ResolvedCalls,
    diagnostics: &mut Diagnostics,
) {
    let mut visitor = DceVisitor {
        resolved_calls,
        pending_sprite_functions: BTreeSet::new(),
        required_sprite_functions: BTreeSet::new(),
        pending_top_level_functions: BTreeSet::new(),
        required_top_level_functions: BTreeSet::new(),
        diagnostics,
    };

    for sprite in document.sprites.values_mut() {
        visitor.run(sprite);
    }

    while let Some(index) = visitor.pending_top_level_functions.pop_last() {
        visitor.required_top_level_functions.insert(index);
        visitor.traverse_function(&document.functions[&index], true);
    }
    document.functions.retain(|index, function| {
        visitor.required_top_level_functions.contains(index) || {
            warn(diagnostics, function);
            false
        }
    });
}

fn warn(diagnostics: &mut Diagnostics, function: &Function) {
    if !function.is_builtin {
        diagnostics
            .warning("unused function", [primary(function.name.span, "")]);
    }
}

struct DceVisitor<'a> {
    resolved_calls: &'a ResolvedCalls,
    pending_sprite_functions: BTreeSet<usize>,
    required_sprite_functions: BTreeSet<usize>,
    pending_top_level_functions: BTreeSet<usize>,
    required_top_level_functions: BTreeSet<usize>,
    diagnostics: &'a mut Diagnostics,
}

impl DceVisitor<'_> {
    fn run(&mut self, sprite: &mut Sprite) {
        self.pending_sprite_functions = sprite
            .functions
            .iter()
            .filter(|(_, function)| function::name_is_special(&function.name))
            .map(|(&index, _)| index)
            .collect();
        self.required_sprite_functions.clear();
        while let Some(index) = self.pending_sprite_functions.pop_last() {
            self.required_sprite_functions.insert(index);
            self.traverse_function(&sprite.functions[&index], false);
        }
        sprite.functions.retain(|index, function| {
            self.required_sprite_functions.contains(index) || {
                warn(self.diagnostics, function);
                false
            }
        });
    }
}

impl Visitor for DceVisitor<'_> {
    fn visit_expression(&mut self, expr: &crate::hir::Expression) {
        if !matches!(expr.kind, ExpressionKind::FunctionCall { .. }) {
            return;
        }
        let function = self.resolved_calls[&expr.span.low()];
        match function {
            function::Ref::SpriteLocal(index) => {
                if !self.required_sprite_functions.contains(&index) {
                    self.pending_sprite_functions.insert(index);
                }
            }
            function::Ref::TopLevel(index) => {
                if !self.required_top_level_functions.contains(&index) {
                    self.pending_top_level_functions.insert(index);
                }
            }
        }
    }
}
