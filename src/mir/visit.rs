use super::{Block, Function, Op, SsaVar, Value};

/// Define a struct, implement this trait, override some `visit_*` methods and
/// traverse the MIR.
pub(super) trait Visitor {
    fn visit_block(&mut self, _block: &mut Block) {}

    fn visit_value(&mut self, _value: &mut Value) {}

    fn traverse_function(&mut self, function: &mut Function) {
        self.traverse_block(&mut function.body);
    }

    fn traverse_block(&mut self, block: &mut Block) {
        self.visit_block(block);
    }

    fn traverse_op(&mut self, op: &mut Op) {
        match op {
            Op::If {
                condition,
                then,
                else_,
            } => {
                self.visit_value(condition);
                self.traverse_block(&mut then.borrow_mut());
                self.traverse_block(&mut else_.borrow_mut());
            }
            Op::Forever { body } => {
                self.traverse_block(&mut body.borrow_mut());
            }
            Op::For { times, body, .. } => {
                self.visit_value(times);
                self.traverse_block(&mut body.borrow_mut());
            }
            Op::While { condition, body } => {
                self.visit_value(condition);
                self.traverse_block(&mut body.borrow_mut());
            }
            Op::IgnoreCall { args, .. }
            | Op::IgnoreCallBuiltin { args, .. }
            | Op::Call { args, .. }
            | Op::CallBuiltin { args, .. } => {
                for arg in args {
                    self.visit_value(arg);
                }
            }
        }
    }
}

/// Replaces all uses of an SSA variable with the given value.
pub(super) struct SsaVarReplacer {
    pub variable: SsaVar,
    pub replacement: Value,
}

impl Visitor for SsaVarReplacer {
    fn visit_value(&mut self, value: &mut Value) {
        if matches!(*value, Value::Var(variable) if variable == self.variable) {
            *value = self.replacement.clone();
        }
    }
}
