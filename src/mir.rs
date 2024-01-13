//! The MIR (mid-level intermediate representation) is like SSA (static single
//! assignment) except it uses structured control flow instead of basic blocks.

mod lowering;
mod optimization;
mod visit;

pub use lowering::lower;
use visit::*;

use crate::{comptime::Value as Imm, function, hir::Costume, ty::Ty};
use std::{collections::HashMap, fmt};

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
    pub name: String,
    pub parameters: Vec<Parameter>,
    pub body: Block,
    pub returns_something: bool,
}

pub struct Parameter {
    pub ssa_var: SsaVar,
    pub ty: Ty,
}

#[derive(Default)]
pub struct Block {
    pub ops: Vec<Op>,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct SsaVar(u16);

impl fmt::Display for SsaVar {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

#[derive(Clone)]
pub enum Value {
    Var(SsaVar),
    Imm(Imm),
}

pub enum Op {
    Return(Value),
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
