use crate::mir::{Block, Function, Imm, Op, SsaVar, Value, Visitor};
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
        if let Op::Call(variable, _) | Op::For { variable, .. } = op {
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
    matches!(op, Op::Call(None, _) if !op.has_side_effects())
        || matches!(op,
            Op::If {
                then, else_, ..
            } if then.ops.is_empty() && else_.ops.is_empty()
        )
        || matches!(op, Op::For { body, .. } if body.ops.is_empty())
        || matches!(
            op,
            Op::While {
                condition: Value::Imm(Imm::Bool(false)),
                ..
            }
        )
}
