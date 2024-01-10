use crate::mir::{Block, Imm, Op, Value};
use std::mem;

pub(super) fn const_if_condition(block: &mut Block) -> bool {
    let Some((index, condition, then, else_)) = block
        .ops
        .iter_mut()
        .enumerate()
        .find_map(|(index, op)| match op {
            Op::If {
                condition: Value::Imm(Imm::Bool(condition)),
                then,
                else_,
            } => Some((index, *condition, mem::take(then), mem::take(else_))),
            _ => None,
        })
    else {
        return false;
    };
    block
        .ops
        .splice(index..=index, if condition { then } else { else_ }.ops);
    true
}

pub(super) fn const_while_condition(block: &mut Block) -> bool {
    let Some((index, condition, body)) = block
        .ops
        .iter_mut()
        .enumerate()
        .find_map(|(index, op)| match op {
            Op::While {
                condition: Value::Imm(Imm::Bool(condition)),
                body,
            } => Some((index, *condition, mem::take(body))),
            _ => None,
        })
    else {
        return false;
    };
    if condition {
        block.ops[index] = Op::Forever { body };
    } else {
        block.ops.remove(index);
    }
    true
}

pub(super) fn no_repeat(block: &mut Block) -> bool {
    let Some(index) = block.ops.iter().position(|op| {
        matches!(op,
            Op::For { times: Value::Imm(Imm::Num(n)), .. } if *n < 0.5
        )
    }) else {
        return false;
    };
    block.ops.remove(index);
    true
}
