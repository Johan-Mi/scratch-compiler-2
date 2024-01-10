//! The MIR (mid-level intermediate representation) is like SSA (static single
//! assignment) except it uses structured control flow instead of basic blocks.

mod optimization;
mod visit;
use visit::Visitor;

use crate::comptime::Value as Imm;

struct Function {
    parameters: Vec<SsaVar>,
    body: Block,
}

#[derive(Default)]
struct Block {
    ops: Vec<Op>,
}

struct SsaVar {}

enum Value {
    Var(SsaVar),
    Imm(Imm),
}

enum Op {
    If {
        condition: Value,
        then: Block,
        else_: Block,
    },
    Forever {
        body: Block,
    },
    While {
        condition: Value,
        body: Block,
    },
    For {
        variable: Option<SsaVar>,
        times: Value,
        body: Block,
    },
}
