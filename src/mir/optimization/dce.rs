use crate::mir::{Block, Function, Op, SsaVar, Value, Visitor};
use std::collections::HashSet;

pub(super) fn eliminate_unused_ssa_vars(function: &mut Function) -> bool {
    let mut finder = Finder {
        used: HashSet::new(),
    };
    finder.traverse_function(function);
    let mut eliminator = Eliminator {
        used: finder.used,
        dirty: false,
    };
    eliminator.traverse_function(function);
    eliminator.dirty
}

struct Finder {
    used: HashSet<SsaVar>,
}

impl Visitor for Finder {
    fn visit_value(&mut self, value: &mut Value) {
        if let Value::Var(var) = *value {
            self.used.insert(var);
        }
    }
}

struct Eliminator {
    used: HashSet<SsaVar>,
    dirty: bool,
}

impl Visitor for Eliminator {
    fn visit_op(&mut self, op: &mut Op) {
        if let Op::Call { variable, .. } | Op::CallBuiltin { variable, .. } = op
        {
            if matches!(variable, Some(var) if !self.used.contains(var)) {
                *variable = None;
                self.dirty = true;
            }
        }
    }
}

pub(super) fn eliminate_useless_ops(block: &mut Block) -> bool {
    let len = block.ops.len();
    block.ops.retain(|op| !is_useless(op));
    block.ops.len() != len
}

fn is_useless(op: &Op) -> bool {
    matches!(op, Op::CallBuiltin { name, .. } if matches!(&**name,
        "add" | "sub" | "mul" | "div" | "mod" | "lt" | "eq" | "gt"
    ))
}
