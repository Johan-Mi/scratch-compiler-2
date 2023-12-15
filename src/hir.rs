use crate::{
    ast,
    diagnostics::{primary, secondary, span, Diagnostics},
    name::Name,
    parser::{SyntaxKind, SyntaxNode},
};
use codemap::{File, Span};
use rowan::ast::AstNode;
use std::collections::HashMap;

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
    sprites: Vec<Sprite>,
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

        Self {
            sprites: sprites.into_values().collect(),
        }
    }
}

#[derive(Debug)]
pub struct Sprite {
    name_span: Span,
    functions: Vec<Function>,
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

        let functions = ast
            .functions()
            .filter_map(|function| {
                Function::lower(&function, file, diagnostics).ok()
            })
            .collect();

        Ok((
            name.to_string(),
            Self {
                name_span,
                functions,
            },
        ))
    }
}

#[derive(Debug)]
pub struct Function {
    name: String,
    parameters: Vec<Parameter>,
    return_ty: Expression,
    body: Block,
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

        let name = ast
            .name()
            .ok_or_else(|| {
                diagnostics.error("function has no name", defined_here());
            })?
            .to_string();

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

        Ok(Self {
            name,
            parameters,
            return_ty: ast.return_ty().map_or(Expression::UnitType, |ty| {
                Expression::lower(&ty, file, diagnostics)
            }),
            body: Block::lower(&body, file, diagnostics),
        })
    }
}

#[derive(Debug)]
pub struct Parameter {
    external_name: String,
    internal_name: String,
    ty: Expression,
}

impl Parameter {
    fn lower(
        ast: &ast::Parameter,
        file: &File,
        diagnostics: &mut Diagnostics,
    ) -> Result<Self> {
        let external_name = ast.external_name().unwrap().identifier();
        let internal_name = ast
            .internal_name()
            .ok_or_else(|| {
                diagnostics.error(
                    "function parameter has no internal name",
                    [primary(span(file, external_name.text_range()), "")],
                );
            })?
            .to_string();
        let ty = ast.ty().ok_or_else(|| {
            diagnostics.error(
                "function parameter has no type",
                [primary(span(file, external_name.text_range()), "")],
            );
        })?;
        Ok(Self {
            external_name: external_name.to_string(),
            internal_name,
            ty: Expression::lower(&ty, file, diagnostics),
        })
    }
}

#[derive(Debug)]
pub struct Block {
    statements: Vec<Statement>,
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
}

#[derive(Debug)]
pub enum Statement {
    Let { variable: String, value: Expression },
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
                    Expression::Error
                };

                Self::Let {
                    variable: variable.to_string(),
                    value,
                }
            }
            ast::Statement::Expr(expr) => {
                Self::Expr(Expression::lower(expr, file, diagnostics))
            }
        }
    }
}

#[derive(Debug)]
pub enum Expression {
    Variable(Name),
    UnitType,
    BinaryOperation {
        lhs: Box<Expression>,
        operator: BinaryOperator,
        rhs: Box<Expression>,
    },
    FunctionCall {
        name: String,
        arguments: Vec<(Option<String>, Expression)>,
    },
    Error,
}

impl Expression {
    fn lower(
        ast: &ast::Expression,
        file: &File,
        diagnostics: &mut Diagnostics,
    ) -> Self {
        match ast {
            ast::Expression::Variable(var) => {
                let identifier = var.identifier();
                Name::resolve(&identifier).map_or_else(
                    || {
                        diagnostics.error(
                            "undefined variable",
                            [primary(span(file, identifier.text_range()), "")],
                        );
                        Self::Error
                    },
                    Self::Variable,
                )
            }
            ast::Expression::FunctionCall(call) => Self::FunctionCall {
                name: call.name().to_string(),
                arguments: call
                    .args()
                    .iter()
                    .map(|arg| {
                        if let ast::Expression::NamedArgument(named_arg) = arg {
                            (
                                Some(named_arg.name().to_string()),
                                Self::lower_opt(
                                    named_arg.value(),
                                    file,
                                    diagnostics,
                                ),
                            )
                        } else {
                            (None, Self::lower(&arg, file, diagnostics))
                        }
                    })
                    .collect(),
            },
            ast::Expression::BinaryOperation(op) => Self::BinaryOperation {
                lhs: Box::new(Self::lower_opt(op.lhs(), file, diagnostics)),
                operator: op.operator().kind().into(),
                rhs: Box::new(Self::lower_opt(op.rhs(), file, diagnostics)),
            },
            ast::Expression::Parenthesized(expr) => {
                Self::lower_opt(expr.inner(), file, diagnostics)
            }
            ast::Expression::NamedArgument(named_arg) => {
                diagnostics.error(
                    "unexpected named argument",
                    [primary(
                        span(file, named_arg.syntax().text_range()),
                        "expected expression",
                    )],
                );
                let _ = Self::lower_opt(named_arg.value(), file, diagnostics);
                Self::Error
            }
        }
    }

    fn lower_opt(
        ast: Option<ast::Expression>,
        file: &File,
        diagnostics: &mut Diagnostics,
    ) -> Self {
        ast.map_or(Self::Error, |expr| Self::lower(&expr, file, diagnostics))
    }
}

#[derive(Debug)]
pub enum BinaryOperator {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
}

impl From<SyntaxKind> for BinaryOperator {
    fn from(value: SyntaxKind) -> Self {
        use SyntaxKind::*;
        match value {
            PLUS => Self::Add,
            MINUS => Self::Sub,
            STAR => Self::Mul,
            SLASH => Self::Div,
            PERCENT => Self::Mod,
            _ => unreachable!(),
        }
    }
}
