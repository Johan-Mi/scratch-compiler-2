use crate::mir::{
    visit::{SsaVarReplacer, Visitor},
    Block, Imm, Op, Value,
};
use std::mem;

pub(super) fn const_if_condition(block: &mut Block) -> bool {
    let Some((index, branch)) =
        block
            .ops
            .iter_mut()
            .enumerate()
            .find_map(|(index, op)| match op {
                Op::If {
                    condition: Value::Imm(Imm::Bool(condition)),
                    then,
                    else_,
                } => Some((
                    index,
                    mem::take(if *condition { then } else { else_ }),
                )),
                _ => None,
            })
    else {
        return false;
    };
    block.ops.splice(index..=index, branch.ops);
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

pub(super) fn repeat_once(block: &mut Block) -> bool {
    #[allow(clippy::float_cmp)]
    let Some((index, variable, mut body)) = block
        .ops
        .iter_mut()
        .enumerate()
        .find_map(|(index, op)| match op {
            Op::For {
                variable,
                times: Value::Imm(Imm::Num(times)),
                body,
            } if times.round() == 1.0 => {
                Some((index, *variable, mem::take(body)))
            }
            _ => None,
        })
    else {
        return false;
    };
    if let Some(variable) = variable {
        SsaVarReplacer {
            variable,
            replacement: Value::Imm(Imm::Num(1.0)),
        }
        .traverse_block(&mut body);
    }
    block.ops.splice(index..=index, body.ops);
    true
}

pub(super) fn remove_unreachable_ops(block: &mut Block) -> bool {
    let Some(index) = block.ops.iter().position(Op::is_guaranteed_to_diverge)
    else {
        return false;
    };
    if index == block.ops.len() - 1 {
        return false;
    }
    block.ops.truncate(index + 1);
    true
}

pub(super) fn divergent_loop_body(op: &mut Op) -> bool {
    match op {
        Op::While { condition, body } if body.is_guaranteed_to_diverge() => {
            *op = Op::If {
                condition: mem::take(condition),
                then: mem::take(body),
                else_: Block::default(),
            };
            true
        }
        Op::Forever { body } if body.is_guaranteed_to_diverge() => {
            // This gets simplified by `const_if_condition`.
            *op = Op::If {
                condition: Value::Imm(Imm::Bool(true)),
                then: mem::take(body),
                else_: Block::default(),
            };
            true
        }
        Op::For { times, body, .. } if body.is_guaranteed_to_diverge() => {
            // This gets simplified by `repeat_once`.
            *times = Value::Imm(Imm::Num(1.0));
            true
        }
        _ => false,
    }
}
