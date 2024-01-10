mod control_flow;

use super::{Function, Visitor as _};

pub(super) fn optimize(function: &mut Function) {
    let mut visitor = Visitor { dirty: false };
    while {
        visitor.traverse_function(function);
        visitor.dirty
    } {
        visitor.dirty = false;
    }
}

struct Visitor {
    dirty: bool,
}

impl super::Visitor for Visitor {
    fn visit_block(&mut self, block: &mut super::Block) {
        self.dirty |= control_flow::const_if_condition(block);
        self.dirty |= control_flow::const_while_condition(block);
        self.dirty |= control_flow::no_repeat(block);
    }
}
