use super::{Document, Op, Sprite, Visitor};
use crate::function;
use std::collections::BTreeSet;

pub fn perform(document: &mut Document) {
    let mut visitor = DceVisitor {
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
        visitor.traverse_function(document.functions.get_mut(&index).unwrap());
    }
    document.functions.retain(|index, _| {
        visitor.required_top_level_functions.contains(index)
    });
}

#[allow(clippy::struct_field_names)]
struct DceVisitor {
    pending_sprite_functions: BTreeSet<usize>,
    required_sprite_functions: BTreeSet<usize>,
    pending_top_level_functions: BTreeSet<usize>,
    required_top_level_functions: BTreeSet<usize>,
}

impl DceVisitor {
    fn run(&mut self, sprite: &mut Sprite) {
        self.pending_sprite_functions = sprite
            .functions
            .iter()
            .filter(|(_, function)| {
                function::Special::try_from(&*function.name).is_ok()
            })
            .map(|(&index, _)| index)
            .collect();
        self.required_sprite_functions.clear();
        while let Some(index) = self.pending_sprite_functions.pop_last() {
            self.required_sprite_functions.insert(index);
            self.traverse_function(sprite.functions.get_mut(&index).unwrap());
        }
        sprite
            .functions
            .retain(|index, _| self.required_sprite_functions.contains(index));
    }
}

impl Visitor for DceVisitor {
    fn visit_op(&mut self, op: &mut Op) {
        let Op::Call { function, .. } = *op else {
            return;
        };
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
