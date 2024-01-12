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

pub fn optimize(document: &mut Document) {
    struct OptimizationVistior;

    impl Visitor for OptimizationVistior {
        fn visit_function(&mut self, function: &mut Function) {
            optimization::optimize(function);
        }
    }

    OptimizationVistior.traverse_document(document);
}

pub struct Document {
    pub sprites: HashMap<String, Sprite>,
    pub functions: HashMap<usize, Function>,
}

pub struct Sprite {
    pub costumes: Vec<Costume>,
    pub functions: HashMap<usize, Function>,
}

pub struct Function {
    pub parameters: Vec<SsaVar>,
    pub body: Block,
}

#[derive(Default)]
pub struct Block {
    parent: Option<Weak<RefCell<Block>>>,
    pub ops: Vec<Op>,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct SsaVar(u16);

#[derive(Clone)]
pub enum Value {
    Var(SsaVar),
    Imm(Imm),
}

pub enum Op {
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
