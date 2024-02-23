use super::{Block, Document, Function, Op, Sprite, SsaVar, Value};

/// Define a struct, implement this trait, override some `visit_*` methods and
/// traverse the MIR.
pub(super) trait Visitor {
    fn visit_function(&mut self, _function: &mut Function) {}

    fn visit_block(&mut self, _block: &mut Block) {}

    fn visit_op(&mut self, _op: &mut Op) {}

    fn visit_value(&mut self, _value: &mut Value) {}

    fn traverse_document(&mut self, document: &mut Document) {
        for sprite in document.sprites.values_mut() {
            self.traverse_sprite(sprite);
        }
        for function in document.functions.values_mut() {
            self.traverse_function(function);
        }
    }

    fn traverse_sprite(&mut self, sprite: &mut Sprite) {
        for function in sprite.functions.values_mut() {
            self.traverse_function(function);
        }
    }

    fn traverse_function(&mut self, function: &mut Function) {
        self.visit_function(function);
        self.traverse_block(&mut function.body);
    }

    fn traverse_block(&mut self, block: &mut Block) {
        self.visit_block(block);
        for op in &mut block.ops {
            self.traverse_op(op);
        }
    }

    fn traverse_op(&mut self, op: &mut Op) {
        self.visit_op(op);
        match op {
            Op::Return(value) => self.visit_value(value),
            Op::If {
                condition,
                then,
                else_,
            } => {
                self.visit_value(condition);
                self.traverse_block(then);
                self.traverse_block(else_);
            }
            Op::Forever { body } => {
                self.traverse_block(body);
            }
            Op::For { times, body, .. } => {
                self.visit_value(times);
                self.traverse_block(body);
            }
            Op::While { condition, body } => {
                self.visit_value(condition);
                self.traverse_block(body);
            }
            Op::Call { args, .. } | Op::Intrinsic { args, .. } => {
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
