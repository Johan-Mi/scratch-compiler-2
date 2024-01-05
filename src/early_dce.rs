//! Dead code elimination performed before any other optimizations to avoid
//! wasting time optimizing unused functions.

use crate::{
    function::{self, ResolvedCalls},
    hir::{Document, ExpressionKind, Sprite, Visitor},
};
use std::collections::BTreeSet;

pub fn perform(document: &mut Document, resolved_calls: &ResolvedCalls) {
    let mut visitor = DceVisitor {
        resolved_calls,
        pending_sprite_functions: BTreeSet::new(),
        required_sprite_functions: BTreeSet::new(),
        pending_top_level_functions: BTreeSet::new(),
        required_top_level_functions: BTreeSet::new(),
    };

    for sprite in document.sprites.values_mut() {
        visitor.run(sprite);
    }

    while let Some(index) = visitor.pending_top_level_functions.pop_last() {
        visitor.required_top_level_functions.insert(index);
        visitor.traverse_function(&document.functions[index]);
    }
    for (index, function) in document.functions.iter_mut().enumerate() {
        if !visitor.required_top_level_functions.contains(&index) {
            function.is_dead = true;
        }
    }
}

struct DceVisitor<'a> {
    resolved_calls: &'a ResolvedCalls,
    pending_sprite_functions: BTreeSet<usize>,
    required_sprite_functions: BTreeSet<usize>,
    pending_top_level_functions: BTreeSet<usize>,
    required_top_level_functions: BTreeSet<usize>,
}

impl DceVisitor<'_> {
    fn run(&mut self, sprite: &mut Sprite) {
        self.pending_sprite_functions = sprite
            .functions
            .iter()
            .enumerate()
            .filter(|(_, function)| function.is_special())
            .map(|(index, _)| index)
            .collect();
        self.required_sprite_functions.clear();
        while let Some(index) = self.pending_sprite_functions.pop_last() {
            self.required_sprite_functions.insert(index);
            self.traverse_function(&sprite.functions[index]);
        }
        for (index, function) in sprite.functions.iter_mut().enumerate() {
            if !self.required_sprite_functions.contains(&index) {
                function.is_dead = true;
            }
        }
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
