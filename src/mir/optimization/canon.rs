use crate::mir::{Op, Value::Imm};

pub(super) fn icalize(op: &mut Op) -> bool {
    if let Op::CallBuiltin { name, args, .. } = op {
        if matches!(&**name, "add" | "mul") && matches!(&**args, [Imm(_), _]) {
            args.reverse();
            return true;
        }
    }

    false
}
