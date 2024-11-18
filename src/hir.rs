pub mod lowering;
pub mod typed;
mod visit;
pub use visit::{Visitor, VisitorPostorderMut};

use crate::{
    comptime::Value,
    diagnostics::{primary, Diagnostics},
    name::Name,
    parser::SyntaxToken,
    ty,
};
use codemap::{Span, Spanned};
use std::collections::{btree_map::Entry, BTreeMap};

/// All error reporting uses the `Diagnostics` struct. This typedef is only
/// used to make short-circuiting more convenient. A result of `Ok(())` does not
/// necessarily mean that there are no errors; broken HIR may be constructed for
/// the sake of resilience.
type Result<T> = std::result::Result<T, ()>;

#[derive(Debug)]
pub struct Document<Func = Function, Struct = self::Struct> {
    pub structs: BTreeMap<String, Struct>,
    pub sprites: BTreeMap<String, Sprite>,
    pub functions: BTreeMap<usize, Func>,
    pub variables: Vec<GlobalVariable>,
}

impl<Func> Document<Func> {
    pub fn merge(&mut self, other: Self, diagnostics: &mut Diagnostics) {
        for (name, struct_) in other.structs {
            match self.structs.entry(name) {
                Entry::Vacant(vacant) => {
                    let _: &mut Struct = vacant.insert(struct_);
                }
                Entry::Occupied(old_struct) => {
                    diagnostics.error(
                        format!("multiple definitions of struct `{}`", old_struct.key()),
                        [
                            primary(struct_.name.span, ""),
                            primary(old_struct.get().name.span, ""),
                        ],
                    );
                }
            }
        }
        for (name, sprite) in other.sprites {
            match self.sprites.entry(name) {
                Entry::Occupied(mut existing) => {
                    existing.get_mut().merge(sprite);
                }
                Entry::Vacant(new) => _ = new.insert(sprite),
            }
        }
        self.functions.extend(other.functions);
        self.variables.extend(other.variables);
    }
}

#[derive(Debug)]
pub struct GlobalVariable {
    pub token: SyntaxToken,
    pub initializer: Expression,
    pub belongs_to_stage: bool,
}

#[derive(Debug)]
pub struct Struct {
    name: Spanned<SyntaxToken>,
    pub fields: Vec<Spanned<Field>>,
}

#[derive(Debug)]
pub struct Field {
    name: SyntaxToken,
    pub ty: Expression,
}

#[derive(Debug)]
pub struct Sprite {
    pub costumes: Vec<Costume>,
}

impl Sprite {
    fn merge(&mut self, other: Self) {
        self.costumes.extend(other.costumes);
    }
}

#[derive(Debug)]
pub struct Costume {
    pub name: String,
    pub path: String,
}

#[derive(Debug)]
pub struct Function {
    pub owning_sprite: Option<String>,
    pub name: Spanned<String>,
    pub tag: Option<String>,
    pub generics: Vec<SyntaxToken>,
    pub parameters: Vec<Parameter>,
    pub return_ty: Expression,
    pub body: Block,
    pub kind: FunctionKind,
}

#[derive(Debug, Clone, Copy)]
pub enum FunctionKind {
    Regular { is_inline: bool },
    Intrinsic,
    Constructor,
}

impl FunctionKind {
    pub const fn is_inline(self) -> bool {
        matches!(self, Self::Regular { is_inline: true })
    }
}

#[derive(Debug)]
pub struct Parameter {
    pub external_name: Option<String>,
    pub internal_name: SyntaxToken,
    pub ty: Expression,
    pub is_comptime: bool,
    pub span: Span,
}

#[derive(Debug, Default)]
pub struct Block {
    pub statements: Vec<Statement>,
}

#[derive(Debug)]
pub struct Statement {
    pub kind: StatementKind,
    pub span: Span,
}

#[derive(Debug)]
pub enum StatementKind {
    Let {
        variable: SyntaxToken,
        value: Expression,
    },
    If {
        condition: Expression,
        then: Result<Block>,
        else_: Result<Block>,
    },
    Repeat {
        times: Expression,
        body: Result<Block>,
    },
    Forever {
        body: Result<Block>,
    },
    While {
        condition: Expression,
        body: Result<Block>,
    },
    Until {
        condition: Expression,
        body: Result<Block>,
    },
    For {
        variable: Result<SyntaxToken>,
        times: Expression,
        body: Result<Block>,
    },
    Return(Expression),
    Expr(Expression),
    Error,
}

#[derive(Debug)]
pub struct Expression {
    pub kind: ExpressionKind,
    pub span: Span,
}

#[derive(Debug)]
pub enum ExpressionKind {
    Variable(Name),
    Imm(Value),
    FunctionCall {
        name_or_operator: SyntaxToken,
        name_span: Span,
        arguments: Vec<Argument>,
    },
    GenericTypeInstantiation {
        generic: ty::Generic,
        arguments: Vec<Expression>,
    },
    ListLiteral(Vec<Expression>),
    TypeAscription {
        inner: Box<Expression>,
        ty: Box<Expression>,
    },
    Error,
}

pub type Argument = (Option<String>, Expression);

pub fn desugar_function_call_name(token: &SyntaxToken) -> &str {
    use crate::parser::SyntaxKind::*;
    match token.kind() {
        PLUS => "add",
        MINUS => "sub",
        STAR => "mul",
        SLASH => "div",
        PERCENT => "mod",
        LT => "lt",
        EQ_EQ => "eq",
        GT => "gt",
        EQ => "set",
        _ => token.text(),
    }
}
