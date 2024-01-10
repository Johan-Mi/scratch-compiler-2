use super::{Block, Function, Op};

/// Define a struct, implement this trait, override some `visit_*` methods and
/// traverse the MIR.
pub(super) trait Visitor {
    fn visit_block(&mut self, _block: &mut Block) {}

    fn traverse_function(&mut self, function: &mut Function) {
        self.traverse_block(&mut function.body);
    }

    fn traverse_block(&mut self, block: &mut Block) {
        self.visit_block(block);
    }

    fn traverse_op(&mut self, op: &mut Op) {
        match op {
            Op::If { then, else_, .. } => {
                self.traverse_block(then);
                self.traverse_block(else_);
            }
            Op::Forever { body }
            | Op::While { body, .. }
            | Op::For { body, .. } => self.traverse_block(body),
        }
    }
}
