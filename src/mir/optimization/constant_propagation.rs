use std::mem;

use crate::mir::{
    visit::{SsaVarReplacer, Visitor},
    Block, Op, Value,
};

pub(super) fn propagate_constants(block: &mut Block) -> bool {
    let mut dirty = false;
    let mut index = 0;
    while index < block.ops.len() {
        if let Op::CallBuiltin {
            variable: Some(variable),
            name,
            args,
        } = &mut block.ops[index]
        {
            let variable = *variable;
            if let Some(value) = evaluate_builtin_call(name, args) {
                dirty = true;
                block.ops.remove(index);
                SsaVarReplacer {
                    variable,
                    replacement: value,
                }
                .traverse_block(block);
                continue;
            }
        }

        index += 1;
    }

    dirty
}

fn evaluate_builtin_call(name: &str, args: &mut [Value]) -> Option<Value> {
    use crate::mir::Imm::{Bool, Num, String};
    use Value::Imm;

    match (name, args) {
        ("add", [Imm(Num(lhs)), Imm(Num(rhs))]) => Some(Imm(Num(*lhs + *rhs))),
        ("sub", [Imm(Num(lhs)), Imm(Num(rhs))]) => Some(Imm(Num(*lhs - *rhs))),
        ("mul", [Imm(Num(lhs)), Imm(Num(rhs))]) => Some(Imm(Num(*lhs * *rhs))),
        ("div", [Imm(Num(lhs)), Imm(Num(rhs))]) => Some(Imm(Num(*lhs / *rhs))),
        ("mod", [Imm(Num(lhs)), Imm(Num(rhs))]) => {
            Some(Imm(Num(lhs.rem_euclid(*rhs))))
        }
        ("lt", [Imm(Num(lhs)), Imm(Num(rhs))]) => Some(Imm(Bool(lhs < rhs))),
        #[allow(clippy::float_cmp)]
        ("eq", [Imm(Num(lhs)), Imm(Num(rhs))]) => Some(Imm(Bool(lhs == rhs))),
        ("gt", [Imm(Num(lhs)), Imm(Num(rhs))]) => Some(Imm(Bool(lhs > rhs))),
        // FIXME: this should be case insensitive.
        ("lt", [Imm(String(lhs)), Imm(String(rhs))]) => {
            Some(Imm(Bool(lhs < rhs)))
        }
        // FIXME: this should be case insensitive.
        ("eq", [Imm(String(lhs)), Imm(String(rhs))]) => {
            Some(Imm(Bool(lhs == rhs)))
        }
        // FIXME: this should be case insensitive.
        ("gt", [Imm(String(lhs)), Imm(String(rhs))]) => {
            Some(Imm(Bool(lhs > rhs)))
        }
        ("not", [Imm(Bool(operand))]) => Some(Imm(Bool(!*operand))),

        #[allow(clippy::float_cmp)]
        ("add", [n, Imm(Num(identity))]) if *identity == 0.0 => {
            Some(mem::take(n))
        }
        #[allow(clippy::float_cmp)]
        ("mul", [n, Imm(Num(identity))]) if *identity == 1.0 => {
            Some(mem::take(n))
        }

        _ => None,
    }
}
