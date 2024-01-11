//! The MIR (mid-level intermediate representation) is like SSA (static single
//! assignment) except it uses structured control flow instead of basic blocks.

mod optimization;
mod visit;
use std::{
    cell::RefCell,
    rc::{Rc, Weak},
};

use visit::*;

use crate::comptime::Value as Imm;

struct Function {
    parameters: Vec<SsaVar>,
    body: Block,
}

#[derive(Default)]
struct Block {
    parent: Option<Weak<RefCell<Block>>>,
    ops: Vec<Op>,
}

#[derive(Clone, Copy, PartialEq, Eq)]
struct SsaVar {}

#[derive(Clone)]
enum Value {
    Var(SsaVar),
    Imm(Imm),
}

enum Op {
    If {
        condition: Value,
        then: Rc<RefCell<Block>>,
        else_: Rc<RefCell<Block>>,
    },
    Forever {
        body: Rc<RefCell<Block>>,
    },
    While {
        condition: Value,
        body: Rc<RefCell<Block>>,
    },
    For {
        variable: Option<SsaVar>,
        times: Value,
        body: Rc<RefCell<Block>>,
    },
    IgnoreCall {
        function: usize,
        args: Vec<Value>,
    },
    IgnoreCallBuiltin {
        name: String,
        args: Vec<Value>,
    },
    Call {
        variable: SsaVar,
        function: usize,
        args: Vec<Value>,
    },
    CallBuiltin {
        variable: SsaVar,
        name: String,
        args: Vec<Value>,
    },
}
