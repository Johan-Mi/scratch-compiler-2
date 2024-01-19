mod canon;
mod constant_propagation;
mod control_flow;
mod dce;

use super::{Function, Op, Visitor as _};

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
    fn visit_function(&mut self, function: &mut Function) {
        self.dirty |= dce::eliminate_unused_ssa_vars(function);
    }

    fn visit_block(&mut self, block: &mut super::Block) {
        self.dirty |= constant_propagation::propagate_constants(block);
        self.dirty |= dce::eliminate_useless_ops(block);
        self.dirty |= control_flow::const_if_condition(block);
        self.dirty |= control_flow::const_while_condition(block);
        self.dirty |= control_flow::no_repeat(block);
        self.dirty |= control_flow::repeat_once(block);
        self.dirty |= control_flow::remove_unreachable_ops(block);
    }

    fn visit_op(&mut self, op: &mut Op) {
        self.dirty |= canon::icalize(op);
        self.dirty |= control_flow::divergent_loop_body(op);
    }
}
