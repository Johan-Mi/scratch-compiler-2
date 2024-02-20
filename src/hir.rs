mod visit;
pub use visit::Visitor;

use crate::{
    ast,
    comptime::{self, Value},
    diagnostics::{primary, secondary, span, Diagnostics},
    function,
    name::Name,
    parser::{SyntaxKind, SyntaxNode, SyntaxToken},
    ty::{self, Context, Ty},
};
use codemap::{File, Span, Spanned};
use rowan::{ast::AstNode, TextRange, TextSize};
use std::collections::{BTreeMap, HashMap};

/// All error reporting uses the `Diagnostics` struct. This typedef is only
/// used to make short-circuiting more convenient. A result of `Ok(())` does not
/// necessarily mean that there are no errors; broken HIR may be constructed for
/// the sake of resilience.
type Result<T> = std::result::Result<T, ()>;

pub fn lower(
    document: SyntaxNode,
    file: &File,
    diagnostics: &mut Diagnostics,
) -> Document {
    Document::lower(&ast::Document::cast(document).unwrap(), file, diagnostics)
}

#[derive(Debug)]
pub struct Document {
    pub sprites: HashMap<String, Sprite>,
    pub functions: BTreeMap<usize, Function>,
}

impl Document {
    pub fn lower(
        ast: &ast::Document,
        file: &File,
        diagnostics: &mut Diagnostics,
    ) -> Self {
        let mut sprites = HashMap::<_, Sprite>::new();

        for sprite in ast.sprites() {
            let Ok((name, sprite)) = Sprite::lower(&sprite, file, diagnostics)
            else {
                continue;
            };

            if let Some(prev_sprite) = sprites.get(&*name) {
                diagnostics.error(
                    format!("redefinition of sprite `{name}`"),
                    [
                        primary(sprite.name_span, "second definition here"),
                        secondary(
                            prev_sprite.name_span,
                            "previously defined here",
                        ),
                    ],
                );
            } else {
                sprites.insert(name, sprite);
            }
        }

        let functions = ast
            .functions()
            .filter_map(|function| {
                Function::lower(&function, file, diagnostics).ok()
            })
            .enumerate()
            .collect();

        Self { sprites, functions }
    }
}

#[derive(Debug)]
pub struct Sprite {
    name_span: Span,
    pub costumes: Vec<Costume>,
    pub functions: BTreeMap<usize, Function>,
}

impl Sprite {
    fn lower(
        ast: &ast::Sprite,
        file: &File,
        diagnostics: &mut Diagnostics,
    ) -> Result<(String, Self)> {
        let name = ast.name().ok_or_else(|| {
            diagnostics.error(
                "sprite has no name",
                [primary(
                    span(file, ast.syntax().text_range()),
                    "defined here",
                )],
            );
        })?;
        let name_span = span(file, name.text_range());

        let costumes = ast
            .costume_lists()
            .flat_map(|it| it.iter())
            .filter_map(|it| Costume::lower(&it).ok())
            .collect();

        let functions = ast
            .functions()
            .filter_map(|function| {
                Function::lower(&function, file, diagnostics).ok()
            })
            .enumerate()
            .collect();

        Ok((
            name.to_string(),
            Self {
                name_span,
                costumes,
                functions,
            },
        ))
    }
}

#[derive(Debug)]
pub struct Costume {
    pub name: String,
    pub path: String,
}

impl Costume {
    fn lower(ast: &ast::Costume) -> Result<Self> {
        Ok(Self {
            name: parse_string_literal(ast.name().ok_or(())?.text())?,
            path: parse_string_literal(ast.path().ok_or(())?.text())?,
        })
    }
}

#[derive(Debug)]
pub struct Function {
    pub name: Spanned<String>,
    pub generics: Option<Vec<SyntaxToken>>,
    pub parameters: Vec<Parameter>,
    pub return_ty: Spanned<Result<Ty>>,
    pub body: Block,
    pub is_builtin: bool,
    pub is_inline: bool,
}

impl Function {
    fn lower(
        ast: &ast::Function,
        file: &File,
        diagnostics: &mut Diagnostics,
    ) -> Result<Self> {
        let defined_here = || {
            [primary(
                span(file, ast.syntax().text_range()),
                "defined here",
            )]
        };

        let name = ast.name().ok_or_else(|| {
            diagnostics.error("function has no name", defined_here());
        })?;
        let name = Spanned {
            node: name.text().to_owned(),
            span: span(file, name.text_range()),
        };

        let parameters = ast
            .parameters()
            .ok_or_else(|| {
                diagnostics
                    .error("function has no parameter list", defined_here());
            })
            .into_iter()
            .flat_map(|p| p.parameters())
            .map(|parameter| Parameter::lower(&parameter, file, diagnostics))
            .collect::<Result<_>>()?;

        let body = ast.body().ok_or_else(|| {
            diagnostics.error("function has no body", defined_here());
        })?;

        let return_ty = ast
            .return_ty()
            .map(|it| Expression::lower(&it, file, diagnostics));
        let return_ty_span = return_ty.as_ref().map_or(name.span, |it| it.span);
        let return_ty = return_ty.map_or(Ok(Ty::Unit), |expr| {
            match comptime::evaluate(expr, diagnostics)? {
                Value::Ty(ty) => Ok(ty),
                value => {
                    diagnostics.error(
                        "function return type must be a type",
                        [primary(
                            return_ty_span,
                            format!("expected `Type`, got `{}`", value.ty()),
                        )],
                    );
                    Err(())
                }
            }
        });

        Ok(Self {
            name,
            generics: ast.generics().map(|it| it.iter().collect()),
            parameters,
            return_ty: Spanned {
                node: return_ty,
                span: return_ty_span,
            },
            body: Block::lower(&body, file, diagnostics),
            is_builtin: false,
            is_inline: ast.kw_inline().is_some(),
        })
    }
}

#[derive(Debug)]
pub struct Parameter {
    pub external_name: Option<String>,
    pub internal_name: SyntaxToken,
    pub ty: Spanned<Result<Ty>>,
    pub is_comptime: bool,
    pub span: Span,
}

impl Parameter {
    fn lower(
        ast: &ast::Parameter,
        file: &File,
        diagnostics: &mut Diagnostics,
    ) -> Result<Self> {
        let external_name = ast.external_name().unwrap().identifier();
        let internal_name = ast.internal_name().ok_or_else(|| {
            diagnostics.error(
                "function parameter has no internal name",
                [primary(span(file, external_name.text_range()), "")],
            );
        })?;

        let ty = ast.ty().ok_or_else(|| {
            diagnostics.error(
                "function parameter has no type",
                [primary(span(file, external_name.text_range()), "")],
            );
        })?;
        let ty = Expression::lower(&ty, file, diagnostics);
        let ty_span = ty.span;
        let ty = match comptime::evaluate(ty, diagnostics) {
            Ok(Value::Ty(ty)) => Ok(ty),
            _ => Err(()),
        };

        Ok(Self {
            external_name: match external_name.text() {
                "_" => None,
                name => Some(name.to_owned()),
            },
            internal_name,
            ty: Spanned {
                node: ty,
                span: ty_span,
            },
            is_comptime: ast.is_comptime(),
            span: span(file, ast.syntax().text_range()),
        })
    }
}

#[derive(Debug)]
pub struct Block {
    pub statements: Vec<Statement>,
}

impl Block {
    fn lower(
        ast: &ast::Block,
        file: &File,
        diagnostics: &mut Diagnostics,
    ) -> Self {
        Self {
            statements: ast
                .statements()
                .map(|statement| {
                    Statement::lower(&statement, file, diagnostics)
                })
                .collect(),
        }
    }

    fn lower_opt(
        ast: Option<ast::Block>,
        file: &File,
        diagnostics: &mut Diagnostics,
    ) -> Result<Self> {
        ast.map(|block| Self::lower(&block, file, diagnostics))
            .ok_or(())
    }
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

impl Statement {
    fn lower(
        ast: &ast::Statement,
        file: &File,
        diagnostics: &mut Diagnostics,
    ) -> Self {
        match ast {
            ast::Statement::Let(let_) => {
                let Some(variable) = let_.variable() else {
                    diagnostics.error(
                        "expected variable name after `let`",
                        [primary(span(file, ast.syntax().text_range()), "")],
                    );
                    return Self::Error;
                };

                let value = if let Some(value) = let_.value() {
                    Expression::lower(&value, file, diagnostics)
                } else {
                    diagnostics.error(
                        "expected expression after `=` in variable definition",
                        [primary(span(file, ast.syntax().text_range()), "")],
                    );
                    Expression {
                        kind: ExpressionKind::Error,
                        span: span(file, variable.text_range()),
                    }
                };

                Self::Let { variable, value }
            }
            ast::Statement::If(if_) => Self::If {
                condition: Expression::lower_opt(
                    if_.condition(),
                    file,
                    diagnostics,
                    ast.syntax().text_range(),
                ),
                then: Block::lower_opt(if_.then(), file, diagnostics),
                else_: if_
                    .else_clause()
                    .and_then(|clause| {
                        clause
                            .block()
                            .map(|block| {
                                Block::lower(&block, file, diagnostics)
                            })
                            .or_else(|| {
                                clause.if_().map(|if_| Block {
                                    statements: vec![Self::lower(
                                        &ast::Statement::If(if_),
                                        file,
                                        diagnostics,
                                    )],
                                })
                            })
                    })
                    .ok_or(()),
            },
            ast::Statement::Repeat(repeat_) => Self::Repeat {
                times: Expression::lower_opt(
                    repeat_.times(),
                    file,
                    diagnostics,
                    ast.syntax().text_range(),
                ),
                body: Block::lower_opt(repeat_.body(), file, diagnostics),
            },
            ast::Statement::Forever(forever) => Self::Forever {
                body: Block::lower_opt(forever.body(), file, diagnostics),
                span: span(file, ast.syntax().text_range()),
            },
            ast::Statement::While(while_) => Self::While {
                body: Block::lower_opt(while_.body(), file, diagnostics),
                condition: Expression::lower_opt(
                    while_.condition(),
                    file,
                    diagnostics,
                    ast.syntax().text_range(),
                ),
            },
            ast::Statement::Until(until_) => Self::Until {
                body: Block::lower_opt(until_.body(), file, diagnostics),
                condition: Expression::lower_opt(
                    until_.condition(),
                    file,
                    diagnostics,
                    ast.syntax().text_range(),
                ),
            },
            ast::Statement::For(for_) => Self::For {
                variable: for_.variable().ok_or(()),
                times: Expression::lower_opt(
                    for_.times(),
                    file,
                    diagnostics,
                    ast.syntax().text_range(),
                ),
                body: Block::lower_opt(for_.body(), file, diagnostics),
            },
            ast::Statement::Expr(expr) => {
                Self::Expr(Expression::lower(expr, file, diagnostics))
            }
        }
    }
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
        ty: Ty,
    },
    Error,
}

pub type Argument = (Option<String>, Expression);

impl Expression {
    fn lower(
        ast: &ast::Expression,
        file: &File,
        diagnostics: &mut Diagnostics,
    ) -> Self {
        let kind = match ast {
            ast::Expression::Variable(var) => {
                let identifier = var.identifier();
                Name::resolve(&identifier).map_or_else(
                    || {
                        diagnostics.error(
                            "undefined variable",
                            [primary(span(file, identifier.text_range()), "")],
                        );
                        ExpressionKind::Error
                    },
                    ExpressionKind::Variable,
                )
            }
            ast::Expression::FunctionCall(call) => {
                lower_function_call(call, file, diagnostics)
            }
            ast::Expression::BinaryOperation(op) => {
                let operator = op.operator();
                let operator_range = operator.text_range();
                ExpressionKind::FunctionCall {
                    name_or_operator: operator,
                    arguments: vec![
                        (
                            None,
                            Self::lower_opt(
                                op.lhs(),
                                file,
                                diagnostics,
                                operator_range,
                            ),
                        ),
                        (
                            None,
                            Self::lower_opt(
                                op.rhs(),
                                file,
                                diagnostics,
                                operator_range,
                            ),
                        ),
                    ],
                }
            }
            ast::Expression::Parenthesized(expr) => {
                return Self::lower_opt(
                    expr.inner(),
                    file,
                    diagnostics,
                    expr.syntax().text_range(),
                );
            }
            ast::Expression::NamedArgument(named_arg) => {
                diagnostics.error(
                    "unexpected named argument",
                    [primary(
                        span(file, named_arg.syntax().text_range()),
                        "expected expression",
                    )],
                );
                let _ = Self::lower_opt(
                    named_arg.value(),
                    file,
                    diagnostics,
                    named_arg.syntax().text_range(),
                );
                ExpressionKind::Error
            }
            ast::Expression::Literal(lit) => lower_literal(lit),
            ast::Expression::Lvalue(lvalue) => lower_lvalue(
                lvalue,
                ast.syntax().text_range(),
                file,
                diagnostics,
            ),
            ast::Expression::GenericTypeInstantiation(instantiation) => {
                lower_generic_type_instantiation(
                    instantiation,
                    file,
                    diagnostics,
                )
            }
            ast::Expression::ListLiteral(list) => ExpressionKind::ListLiteral(
                list.iter()
                    .map(|it| Self::lower(&it, file, diagnostics))
                    .collect(),
            ),
            ast::Expression::TypeAscription(ascription) => {
                lower_type_ascription(ascription, diagnostics, file)
            }
        };

        Self {
            kind,
            span: span(file, ast.syntax().text_range()),
        }
    }

    fn lower_opt(
        ast: Option<ast::Expression>,
        file: &File,
        diagnostics: &mut Diagnostics,
        fallback_text_range: TextRange,
    ) -> Self {
        ast.map_or_else(
            || Self {
                kind: ExpressionKind::Error,
                span: span(file, fallback_text_range),
            },
            |expr| Self::lower(&expr, file, diagnostics),
        )
    }

    pub fn ty(&self, tcx: &mut Context) -> Result<Ty> {
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
                .unwrap_or_else(|| panic!("variable `{variable}` has no type"))
                .clone(),
            ExpressionKind::Variable(Name::Builtin(builtin)) => {
                ty::of_builtin_name(*builtin, self.span, tcx.diagnostics)
            }
            ExpressionKind::Imm(value) => Ok(value.ty()),
            ExpressionKind::FunctionCall {
                name_or_operator,
                arguments,
            } => {
                let name = desugar_function_call_name(name_or_operator);
                let (resolved, return_ty) = function::resolve(
                    name,
                    arguments,
                    self.span.subspan(
                        0,
                        u32::from(name_or_operator.text_range().len()).into(),
                    ),
                    tcx,
                )?;
                tcx.resolved_calls.insert(self.span.low(), resolved);

                for (param, (_, arg)) in std::iter::zip(
                    &tcx.function(resolved).parameters,
                    arguments,
                ) {
                    if param.is_comptime && !comptime::is_known(arg) {
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
            ExpressionKind::GenericTypeInstantiation { .. } => Ok(Ty::Ty),
            ExpressionKind::ListLiteral(list) => {
                let [first, rest @ ..] = &**list else {
                    tcx.diagnostics.error(
                        "cannot infer type of empty list literal",
                        [primary(self.span, "")],
                    );
                    tcx.diagnostics.note("sorry!", []);
                    return Err(());
                };
                let first_ty = first.ty(tcx)?;

                for element in rest {
                    let ty = element.ty(tcx)?;
                    if ty != first_ty {
                        tcx.diagnostics.error(
                            "conflicting types in list literal",
                            [
                                primary(first.span, format!("expected element type `{first_ty}` because of because of the first item...")),
                                primary(element.span, format!("...but this has type `{ty}`")),
                            ]
                        );
                        return Err(());
                    }
                }

                Ok(Ty::List(Box::new(first_ty)))
            }
            ExpressionKind::TypeAscription { inner, ty } => {
                if let Ok(inner_ty) = inner.ty(tcx) {
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

fn lower_function_call(
    call: &ast::FunctionCall,
    file: &File,
    diagnostics: &mut Diagnostics,
) -> ExpressionKind {
    ExpressionKind::FunctionCall {
        name_or_operator: call.name(),
        arguments: call
            .args()
            .iter()
            .map(|arg| {
                if let ast::Expression::NamedArgument(named_arg) = arg {
                    (
                        Some(named_arg.name().to_string()),
                        Expression::lower_opt(
                            named_arg.value(),
                            file,
                            diagnostics,
                            named_arg.syntax().text_range(),
                        ),
                    )
                } else {
                    (None, Expression::lower(&arg, file, diagnostics))
                }
            })
            .collect(),
    }
}

fn lower_lvalue(
    lvalue: &ast::Lvalue,
    text_range: TextRange,
    file: &File,
    diagnostics: &mut Diagnostics,
) -> ExpressionKind {
    let Some(inner) = lvalue.inner() else {
        return ExpressionKind::Error;
    };
    let inner = Expression::lower(&inner, file, diagnostics);
    if let ExpressionKind::Variable(Name::User(var)) = inner.kind {
        if var.parent().is_some_and(|it| it.kind() == SyntaxKind::LET) {
            return ExpressionKind::Lvalue(var.text_range().start());
        }
    }
    diagnostics.error(
        "`&` can only be applied to variables declared with `let`",
        [primary(span(file, text_range), "")],
    );
    ExpressionKind::Error
}

fn lower_generic_type_instantiation(
    instantiation: &ast::GenericTypeInstantiation,
    file: &File,
    diagnostics: &mut Diagnostics,
) -> ExpressionKind {
    let generic =
        Expression::lower(&instantiation.generic(), file, diagnostics);
    let span = generic.span;
    let Ok(generic) = ty::Generic::try_from(generic) else {
        diagnostics.error(
            "type parameters can only be applied to generic types",
            [primary(span, "this is not a generic type")],
        );
        return ExpressionKind::Error;
    };

    let arguments = instantiation
        .type_parameters()
        .iter()
        .map(|it| Expression::lower(&it, file, diagnostics))
        .collect::<Vec<_>>();

    ExpressionKind::GenericTypeInstantiation { generic, arguments }
}

fn lower_type_ascription(
    ascription: &ast::TypeAscription,
    diagnostics: &mut Diagnostics,
    file: &File,
) -> ExpressionKind {
    let Some(inner) = ascription.inner() else {
        return ExpressionKind::Error;
    };
    let Some(ty) = ascription.ty() else {
        return ExpressionKind::Error;
    };

    let inner = Expression::lower(&inner, file, diagnostics);
    let ty = Expression::lower(&ty, file, diagnostics);
    let ty_span = ty.span;
    let Ok(ty) = comptime::evaluate(ty, diagnostics) else {
        return ExpressionKind::Error;
    };
    let Value::Ty(ty) = ty else {
        diagnostics.error(
            "ascribed type must be a type",
            [primary(
                ty_span,
                format!("expected `Type`, got `{}`", ty.ty()),
            )],
        );
        return ExpressionKind::Error;
    };

    ExpressionKind::TypeAscription {
        inner: Box::new(inner),
        ty,
    }
}

fn lower_literal(lit: &ast::Literal) -> ExpressionKind {
    let token = lit.syntax().first_token().unwrap();
    match token.kind() {
        crate::parser::SyntaxKind::NUMBER => {
            token.text().parse().map_or(ExpressionKind::Error, |n| {
                ExpressionKind::Imm(Value::Num(n))
            })
        }
        crate::parser::SyntaxKind::STRING => parse_string_literal(token.text())
            .map_or(ExpressionKind::Error, |s| {
                ExpressionKind::Imm(Value::String(s))
            }),
        crate::parser::SyntaxKind::KW_FALSE => {
            ExpressionKind::Imm(Value::Bool(false))
        }
        crate::parser::SyntaxKind::KW_TRUE => {
            ExpressionKind::Imm(Value::Bool(true))
        }
        _ => unreachable!(),
    }
}

fn parse_string_literal(lit: &str) -> Result<String> {
    // Remove the quotes.
    Ok(lit[1..].strip_suffix('"').ok_or(())?.to_owned())
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
