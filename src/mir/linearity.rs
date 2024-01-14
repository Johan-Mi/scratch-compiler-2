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
        let mut steps = HashMap::<SsaVar, usize>::new();
        for op in block.ops.iter().rev() {
            let is_pure = op.is_pure();

            if !is_pure {
                for step in steps.values_mut() {
                    *step += 1;
                }
            }

            for var in direct_args(op)
                .iter()
                .filter_map(Value::as_var)
                .filter(|var| self.used_once[var])
            {
                steps.insert(var, 0);
            }

            if let Op::Call {
                variable: Some(var),
                ..
            }
            | Op::CallBuiltin {
                variable: Some(var),
                ..
            } = *op
            {
                if steps.get(&var) == Some(&usize::from(!is_pure)) {
                    self.is_linear.insert(var);
                    steps.remove(&var);
                    if !is_pure {
                        for step in steps.values_mut() {
                            *step += 1;
                        }
                    }
                }
            }
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
        Op::Call { args, .. } | Op::CallBuiltin { args, .. } => args,
    }
}
