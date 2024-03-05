use super::{Block, Function, Op, SsaVar, Value, Visitor};
use std::{
    collections::{HashMap, HashSet},
    slice,
};

pub fn check(function: &mut Function) -> HashSet<SsaVar> {
    let mut counter = Counter {
        used_once: HashMap::new(),
    };
    counter.traverse_function(function);

    let mut finder = Finder {
        used_once: counter.used_once,
        is_linear: HashSet::new(),
    };
    finder.traverse_function(function);

    finder.is_linear
}

struct Counter {
    used_once: HashMap<SsaVar, bool>,
}

impl Visitor for Counter {
    fn visit_value(&mut self, value: &mut Value) {
        if let Value::Var(var) = *value {
            self.used_once
                .entry(var)
                .and_modify(|entry| *entry = false)
                .or_insert(true);
        }
    }
}

struct Finder {
    used_once: HashMap<SsaVar, bool>,
    is_linear: HashSet<SsaVar>,
}

impl Visitor for Finder {
    fn visit_block(&mut self, block: &mut Block) {
        let mut candidates = HashSet::new();
        for op in block.ops.iter().rev() {
            let this_is_linear = match *op {
                Op::Call {
                    variable: Some(variable),
                    ..
                }
                | Op::Intrinsic {
                    variable: Some(variable),
                    ..
                } if candidates.remove(&variable) => {
                    self.is_linear.insert(variable);
                    true
                }
                _ => false,
            };

            if !this_is_linear && op.has_side_effects() {
                candidates.clear();
            }

            candidates.extend(
                direct_args(op)
                    .iter()
                    .filter_map(Value::as_var)
                    .filter(|var| self.used_once[var]),
            );
        }
    }
}

fn direct_args(op: &Op) -> &[Value] {
    match op {
        Op::Return(value)
        | Op::If {
            condition: value, ..
        }
        | Op::While {
            condition: value, ..
        }
        | Op::For { times: value, .. } => slice::from_ref(value),
        Op::Forever { .. } => &[],
        Op::Call { args, .. } | Op::Intrinsic { args, .. } => args,
    }
}
