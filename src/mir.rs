//! The MIR (mid-level intermediate representation) is like SSA (static single
//! assignment) except it uses structured control flow instead of basic blocks.

pub mod linearity;
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

#[derive(Debug)]
pub struct Document {
    pub sprites: HashMap<String, Sprite>,
    pub functions: HashMap<usize, Function>,
}

#[derive(Debug)]
pub struct Sprite {
    pub costumes: Vec<Costume>,
    pub functions: HashMap<usize, Function>,
}

#[derive(Debug)]
pub struct Function {
    pub name: String,
    pub parameters: Vec<Parameter>,
    pub body: Block,
    pub returns_something: bool,
}

#[derive(Debug)]
pub struct Parameter {
    pub ssa_var: SsaVar,
    pub ty: Ty,
}

#[derive(Debug, Default)]
pub struct Block {
    pub ops: Vec<Op>,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct SsaVar(u16);

impl fmt::Debug for SsaVar {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "v{}", self.0)
    }
}

impl fmt::Display for SsaVar {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

#[derive(Debug, Clone)]
pub enum Value {
    Var(SsaVar),
    Imm(Imm),
}

impl Value {
    const fn as_var(&self) -> Option<SsaVar> {
        if let Self::Var(v) = *self {
            Some(v)
        } else {
            None
        }
    }
}

#[derive(Debug)]
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
