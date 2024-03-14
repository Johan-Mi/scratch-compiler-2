pub mod lowering;
pub mod typed;
mod visit;
pub use visit::Visitor;

use crate::{
    comptime::{self, Value},
    diagnostics::{primary, secondary},
    function,
    name::Name,
    parser::{SyntaxKind, SyntaxToken},
    ty::{self, Context, Ty},
};
use codemap::{Span, Spanned};
use rowan::TextSize;
use std::collections::{BTreeMap, HashMap};

/// All error reporting uses the `Diagnostics` struct. This typedef is only
/// used to make short-circuiting more convenient. A result of `Ok(())` does not
/// necessarily mean that there are no errors; broken HIR may be constructed for
/// the sake of resilience.
type Result<T> = std::result::Result<T, ()>;

#[derive(Debug)]
pub struct Document<Func = Function> {
    pub sprites: HashMap<String, Sprite<Func>>,
    pub functions: BTreeMap<usize, Func>,
    pub variables: Vec<GlobalVariable>,
}

impl<Func> Document<Func> {
    pub fn merge(&mut self, other: Self) {
        self.sprites.extend(other.sprites);
        self.functions.extend(other.functions);
        self.variables.extend(other.variables);
    }
}

#[derive(Debug)]
pub struct GlobalVariable {
    pub token: SyntaxToken,
    pub initializer: Expression,
    // FIXME: ensure that truly global variables belong to the stage and not to
    // a sprite that uses them and just so happens to get compiled earlier
    owning_sprite: String,
}

#[derive(Debug)]
pub struct Sprite<Func = Function> {
    pub costumes: Vec<Costume>,
    pub functions: BTreeMap<usize, Func>,
}

impl<Func> Sprite<Func> {
    fn merge(&mut self, other: Self) {
        self.costumes.extend(other.costumes);
        self.functions.extend(other.functions);
    }
}

#[derive(Debug)]
pub struct Costume {
    pub name: String,
    pub path: String,
}

#[derive(Debug)]
pub struct Function {
    pub name: Spanned<String>,
    pub generics: Vec<SyntaxToken>,
    pub parameters: Vec<Parameter>,
    pub return_ty: Expression,
    pub body: Block,
    pub is_from_builtins: bool,
    pub is_intrinsic: bool,
    pub is_inline: bool,
}

#[derive(Debug)]
pub struct Parameter {
    pub external_name: Option<String>,
    pub internal_name: SyntaxToken,
    pub ty: Expression,
    pub is_comptime: bool,
    pub span: Span,
}

#[derive(Debug)]
pub struct Block {
    pub statements: Vec<Statement>,
}

#[derive(Debug)]
pub enum Statement {
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
        span: Span,
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
    Lvalue(TextSize),
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

impl Expression {
    pub fn ty(&self, ascribed: Option<&Ty>, tcx: &mut Context) -> Result<Ty> {
        match &self.kind {
            ExpressionKind::Variable(Name::User(variable))
                if variable
                    .parent()
                    .is_some_and(|it| it.kind() == SyntaxKind::SPRITE) =>
            {
                Ok(Ty::Sprite)
            }
            ExpressionKind::Variable(Name::User(variable)) => tcx
                .variable_types
                .get(&variable.text_range().start())
                .unwrap_or_else(|| {
                    panic!("variable `{variable:?}` has no type")
                })
                .clone(),
            ExpressionKind::Variable(Name::Builtin(builtin)) => {
                ty::of_builtin_name(*builtin, self.span, tcx.diagnostics)
            }
            ExpressionKind::Imm(value) => Ok(value.ty()),
            ExpressionKind::FunctionCall {
                name_or_operator,
                name_span,
                arguments,
            } => {
                let name = desugar_function_call_name(name_or_operator);
                let (resolved, return_ty) =
                    function::resolve(name, arguments, *name_span, tcx)?;
                tcx.resolved_calls.insert(self.span.low(), resolved);

                for (param, (_, arg)) in std::iter::zip(
                    &tcx.function(resolved).parameters,
                    arguments,
                ) {
                    if param.is_comptime && !comptime::is_known(arg, tcx) {
                        tcx.diagnostics.error(
                            "function argument is not comptime-known",
                            [
                                primary(arg.span, ""),
                                secondary(
                                    param.span,
                                    "comptime parameter declared here",
                                ),
                            ],
                        );
                    }
                }

                return_ty
            }
            ExpressionKind::Lvalue(var) => {
                tcx.variable_types[var].clone().map(Box::new).map(Ty::Var)
            }
            ExpressionKind::GenericTypeInstantiation { generic, arguments } => {
                ty::check_generic_type_instantiation(
                    *generic, arguments, self.span, tcx,
                );
                Ok(Ty::Ty)
            }
            ExpressionKind::ListLiteral(list) => {
                ty::of_list_literal(list, self.span, ascribed, tcx)
            }
            ExpressionKind::TypeAscription { inner, ty } => {
                let Ok(ty_ty) = ty.ty(None, tcx) else {
                    return Err(());
                };
                if !matches!(ty_ty, Ty::Ty) {
                    tcx.diagnostics.error(
                        "ascribed type must be a type",
                        [primary(
                            ty.span,
                            format!("expected `Type`, got `{ty_ty}`"),
                        )],
                    );
                };

                let ty = match &ty.kind {
                    ExpressionKind::Imm(Value::Ty(ty)) => ty,
                    ExpressionKind::Imm(_) => return Err(()),
                    _ => {
                        tcx.diagnostics.error(
                            "ascribed type must be comptime-known",
                            [primary(ty.span, "")],
                        );
                        return Err(());
                    }
                };

                if let Ok(inner_ty) = inner.ty(Some(ty), tcx) {
                    if inner_ty != *ty {
                        tcx.diagnostics.error(
                            "type ascription mismatch",
                            [primary(
                                inner.span,
                                format!(
                                    "expected type `{ty}`, got `{inner_ty}`"
                                ),
                            )],
                        );
                    }
                }

                Ok(ty.clone())
            }
            ExpressionKind::Error => Err(()),
        }
    }
}

pub fn desugar_function_call_name(token: &SyntaxToken) -> &str {
    use SyntaxKind::*;
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

pub fn parse_string_literal(lit: &str) -> Result<String> {
    // Remove the quotes.
    Ok(lit[1..].strip_suffix('"').ok_or(())?.to_owned())
}
