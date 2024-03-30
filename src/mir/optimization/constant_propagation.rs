use std::mem;

use crate::mir::{
    visit::{SsaVarReplacer, Visitor},
    Block, Op, Value,
};

pub(super) fn propagate_constants(block: &mut Block) -> bool {
    let mut dirty = false;
    let mut index = 0;
    while index < block.ops.len() {
        if let Op::Intrinsic {
            variable: Some(variable),
            name,
            args,
        } = &mut block.ops[index]
        {
            let variable = *variable;
            if let Some(value) = evaluate_intrinsic(name, args) {
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

fn evaluate_intrinsic(name: &str, args: &mut [Value]) -> Option<Value> {
    use crate::mir::Imm::{Bool, Num, String};
    use Value::Imm;

    match (name, args) {
        ("to-string", [Imm(Num(num))]) => {
            Some(Imm(String(ryu_js::Buffer::new().format(*num).to_owned())))
        }
        ("to-string", [Imm(Bool(b))]) => Some(Imm(String(b.to_string()))),
        #[allow(clippy::cast_precision_loss)]
        ("length", [Imm(String(s))]) => {
            Some(Imm(Num(s.chars().count() as f64)))
        }
        #[allow(clippy::cast_possible_truncation, clippy::cast_sign_loss)]
        ("letter", [Imm(String(s)), Imm(Num(index))]) => Some(Imm(String(
            (*index as usize)
                .checked_sub(1)
                .and_then(|index| s.chars().nth(index))
                .map(Into::into)
                .unwrap_or_default(),
        ))),
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
        ("lt", [Imm(String(lhs)), Imm(String(rhs))]) => {
            Some(Imm(Bool(lhs.to_lowercase() < rhs.to_lowercase())))
        }
        ("eq", [Imm(String(lhs)), Imm(String(rhs))]) => {
            Some(Imm(Bool(lhs.to_lowercase() == rhs.to_lowercase())))
        }
        ("gt", [Imm(String(lhs)), Imm(String(rhs))]) => {
            Some(Imm(Bool(lhs.to_lowercase() > rhs.to_lowercase())))
        }
        ("not", [Imm(Bool(operand))]) => Some(Imm(Bool(!*operand))),
        ("join", [Imm(String(lhs)), Imm(String(rhs))]) => {
            Some(Imm(String(format!("{lhs}{rhs}"))))
        }
        ("ceil", [Imm(Num(lhs))]) => Some(Imm(Num(lhs.ceil()))),

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
