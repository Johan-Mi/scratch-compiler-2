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

use crate::{
    comptime::Value as Imm, generator::Generator, hir::Sprite, ty::Ty,
};
use std::{collections::BTreeMap, fmt};

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
    pub sprites: BTreeMap<String, Sprite>,
    pub functions: BTreeMap<usize, Function>,
}

#[derive(Debug)]
pub struct Function {
    pub owning_sprite: Option<String>,
    pub name: String,
    pub tag: Option<String>,
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

impl Generator {
    fn new_ssa_var(&mut self) -> SsaVar {
        SsaVar(self.new_u16())
    }

    fn new_real_var(&mut self) -> RealVar {
        RealVar(self.new_u16())
    }

    fn new_real_list(&mut self) -> RealList {
        RealList(self.new_u16())
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
    Return {
        value: Value,
        is_explicit: bool,
    },
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
    Call(Option<SsaVar>, Call),
}

impl Op {
    fn has_side_effects(&self) -> bool {
        !matches!(self, Self::Call(_, Call::Intrinsic { name, ..}) if matches!(&**name,
            "add" | "sub" | "mul" | "div" | "mod"
            | "lt" | "eq" | "gt"
            | "not" | "and" | "or"
            | "join" | "to-string" | "to-num" | "length" | "letter" | "answer" | "pressing-key" | "timer" | "random"
            | "x-pos" | "y-pos"
            | "mouse-x" | "mouse-y"
        ))
    }

    fn is_guaranteed_to_diverge(&self) -> bool {
        matches!(self, Self::Forever { .. })
            || matches!(self, Self::If { then, else_, .. }
                if then.is_guaranteed_to_diverge() && else_.is_guaranteed_to_diverge())
    }
}

#[derive(Debug, Clone)]
pub enum Call {
    Custom { function: usize, args: Vec<Value> },
    Intrinsic { name: String, args: Vec<Value> },
}
