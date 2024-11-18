use crate::{
    comptime::Value::Num,
    mir::{
        Call, Op,
        Value::{Imm, Var},
    },
};

pub(super) fn icalize(op: &mut Op) -> bool {
    if let Op::Call(_, Call::Intrinsic { name, args }) = op {
        if matches!(&**name, "add" | "mul") && matches!(&**args, [Imm(_), Var(_)]) {
            args.reverse();
            return true;
        } else if let ("sub", [Var(_), Imm(Num(n))]) = (&**name, &mut **args) {
            "add".clone_into(name);
            *n *= -1.0;
            return true;
        }
    }

    false
}
