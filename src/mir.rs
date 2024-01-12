//! The MIR (mid-level intermediate representation) is like SSA (static single
//! assignment) except it uses structured control flow instead of basic blocks.

mod lowering;
mod optimization;
mod visit;

pub use lowering::lower;
use visit::*;

use crate::{comptime::Value as Imm, function, hir::Costume};
use std::{
    cell::RefCell,
    collections::HashMap,
    rc::{Rc, Weak},
};

pub struct Document {
    sprites: HashMap<String, Sprite>,
    functions: HashMap<usize, Function>,
}

struct Sprite {
    costumes: Vec<Costume>,
    functions: HashMap<usize, Function>,
}

struct Function {
    parameters: Vec<SsaVar>,
    body: Block,
}

#[derive(Default)]
struct Block {
    parent: Option<Weak<RefCell<Block>>>,
    ops: Vec<Op>,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
struct SsaVar(u16);

#[derive(Clone)]
enum Value {
    Var(SsaVar),
    Imm(Imm),
}

enum Op {
    Return(Value),
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
    Call {
        variable: Option<SsaVar>,
        function: function::Ref,
        args: Vec<Value>,
    },
    CallBuiltin {
        variable: Option<SsaVar>,
        name: String,
        args: Vec<Value>,
    },
}
