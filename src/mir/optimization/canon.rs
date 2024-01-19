use crate::mir::{Imm::Num, Op, Value::Imm};

pub(super) fn icalize(op: &mut Op) -> bool {
    if let Op::CallBuiltin { name, args, .. } = op {
        if matches!(&**name, "add" | "mul") && matches!(&**args, [Imm(_), _]) {
            args.reverse();
            return true;
        } else if let ("sub", [_, Imm(Num(n))]) = (&**name, &mut **args) {
            *name = "add".to_owned();
            *n *= -1.0;
            return true;
        }
    }

    false
}
