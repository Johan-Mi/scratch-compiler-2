//! The MIR (mid-level intermediate representation) is like SSA (static single
//! assignment) except it uses structured control flow instead of basic blocks.

mod inlining;
mod late_dce;
pub mod linearity;
mod lowering;
mod mutability;
mod optimization;
mod visit;

pub use lowering::lower;
use visit::*;

use crate::{comptime::Value as Imm, function, hir::Costume, ty::Ty};
use std::{collections::HashMap, fmt};

pub fn optimize(document: &mut Document, generator: &mut Generator) {
    struct OptimizationVistior;

    impl Visitor for OptimizationVistior {
        fn visit_function(&mut self, function: &mut Function) {
            optimization::optimize(function);
        }
    }

    inlining::inline(document, generator);
    OptimizationVistior.traverse_document(document);
    late_dce::perform(document);
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
    pub is_inline: bool,
}

#[derive(Debug)]
pub struct Parameter {
    pub ssa_var: SsaVar,
    pub ty: Ty,
}

#[derive(Debug, Default, Clone)]
pub struct Block {
    pub ops: Vec<Op>,
}

impl Block {
    fn is_guaranteed_to_diverge(&self) -> bool {
        self.ops.last().is_some_and(Op::is_guaranteed_to_diverge)
    }
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

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct RealVar(u16);

impl fmt::Debug for RealVar {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "r{}", self.0)
    }
}

impl fmt::Display for RealVar {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct RealList(u16);

impl fmt::Debug for RealList {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "l{}", self.0)
    }
}

impl fmt::Display for RealList {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

#[derive(Default)]
pub struct Generator {
    counter: u16,
}

impl Generator {
    fn new_ssa_var(&mut self) -> SsaVar {
        let var = self.counter;
        self.counter += 1;
        SsaVar(var)
    }

    fn new_real_var(&mut self) -> RealVar {
        let var = self.counter;
        self.counter += 1;
        RealVar(var)
    }

    fn new_real_list(&mut self) -> RealList {
        let list = self.counter;
        self.counter += 1;
        RealList(list)
    }
}

#[derive(Clone)]
pub enum Value {
    Var(SsaVar),
    Imm(Imm),
    Lvalue(RealVar),
    List(RealList),
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Var(var) => fmt::Debug::fmt(var, f),
            Self::Imm(imm) => fmt::Debug::fmt(imm, f),
            Self::Lvalue(var) => write!(f, "&{var:?}"),
            Self::List(list) => fmt::Debug::fmt(list, f),
        }
    }
}

impl Default for Value {
    fn default() -> Self {
        Self::Imm(Imm::Num(0.0))
    }
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

#[derive(Debug, Clone)]
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
    Intrinsic {
        variable: Option<SsaVar>,
        name: String,
        args: Vec<Value>,
    },
}

impl Op {
    fn is_pure(&self) -> bool {
        matches!(self, Self::Intrinsic { name, .. } if matches!(&**name,
            "add" | "sub" | "mul" | "div" | "mod" | "lt" | "eq" | "gt" | "not" | "join"
        ))
    }

    fn is_guaranteed_to_diverge(&self) -> bool {
        matches!(self, Self::Forever { .. })
            || matches!(self, Self::If { then, else_, .. }
                if then.is_guaranteed_to_diverge() && else_.is_guaranteed_to_diverge())
    }
}
