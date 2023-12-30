use crate::{
    ast,
    comptime::{self, Value},
    diagnostics::{primary, secondary, span, Diagnostics},
    function,
    name::{self, Name},
    parser::{SyntaxKind, SyntaxNode, SyntaxToken},
    ty::{Context, Ty},
};
use codemap::{File, Span, Spanned};
use rowan::{ast::AstNode, TextRange};
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
    pub sprites: HashMap<String, Sprite>,
    pub functions: Vec<Function>,
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
            .collect();

        Self { sprites, functions }
    }
}

#[derive(Debug)]
pub struct Sprite {
    name_span: Span,
    pub costumes: Vec<Costume>,
    pub functions: Vec<Function>,
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
            name: parse_string_literal(ast.name().ok_or(())?.text()),
            path: parse_string_literal(ast.path().ok_or(())?.text()),
        })
    }
}

#[derive(Debug)]
pub struct Function {
    pub name: Spanned<String>,
    pub parameters: Vec<Parameter>,
    pub return_ty: Result<Ty>,
    pub body: Block,
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

        let return_ty = ast.return_ty().map_or(Ok(Ty::Unit), |ty| {
            let expr = Expression::lower(&ty, file, diagnostics);
            let span = expr.span;
            match comptime::evaluate(expr, diagnostics)? {
                Value::Ty(ty) => Ok(ty),
                value => {
                    diagnostics.error(
                        "function return type must be a type",
                        [primary(
                            span,
                            format!("expected `Type`, got `{}`", value.ty()),
                        )],
                    );
                    Err(())
                }
            }
        });

        Ok(Self {
            name,
            parameters,
            return_ty,
            body: Block::lower(&body, file, diagnostics),
        })
    }

    pub fn can_be_called_with(
        &self,
        arguments: &[Argument],
        tcx: &mut Context,
    ) -> bool {
        self.parameters.len() == arguments.len()
            && std::iter::zip(&self.parameters, arguments).all(
                |(parameter, argument)| {
                    parameter.is_compatible_with(argument, tcx)
                },
            )
    }
}

#[derive(Debug)]
pub struct Parameter {
    pub external_name: Option<String>,
    pub internal_name: SyntaxToken,
    pub ty: Result<Ty>,
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
            ty,
        })
    }

    fn is_compatible_with(
        &self,
        (argument_name, value): &Argument,
        tcx: &mut Context,
    ) -> bool {
        self.external_name.as_deref() == argument_name.as_deref()
            && match (&self.ty, value.ty(tcx)) {
                (Ok(parameter_ty), Ok(argument_ty)) => {
                    argument_ty.is_subtype_of(parameter_ty)
                }
                // A type error has already occured; don't let it cascade.
                _ => true,
            }
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
}

#[derive(Debug)]
pub enum Statement {
    Let {
        variable: SyntaxToken,
        value: Expression,
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
    BinaryOperation {
        lhs: Box<Expression>,
        operator: BinaryOperator,
        rhs: Box<Expression>,
    },
    FunctionCall {
        name: SyntaxToken,
        arguments: Vec<Argument>,
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
                ExpressionKind::FunctionCall {
                    name: call.name(),
                    arguments: call
                        .args()
                        .iter()
                        .map(|arg| {
                            if let ast::Expression::NamedArgument(named_arg) =
                                arg
                            {
                                (
                                    Some(named_arg.name().to_string()),
                                    Self::lower_opt(
                                        named_arg.value(),
                                        file,
                                        diagnostics,
                                        named_arg.syntax().text_range(),
                                    ),
                                )
                            } else {
                                (None, Self::lower(&arg, file, diagnostics))
                            }
                        })
                        .collect(),
                }
            }
            ast::Expression::BinaryOperation(op) => {
                let operator = &op.operator();
                ExpressionKind::BinaryOperation {
                    lhs: Box::new(Self::lower_opt(
                        op.lhs(),
                        file,
                        diagnostics,
                        operator.text_range(),
                    )),
                    operator: operator.kind().into(),
                    rhs: Box::new(Self::lower_opt(
                        op.rhs(),
                        file,
                        diagnostics,
                        operator.text_range(),
                    )),
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
            ExpressionKind::Variable(Name::User(variable)) => tcx
                .variable_types
                .get(&variable.text_range().start())
                .unwrap_or_else(|| panic!("variable `{variable}` has no type"))
                .clone(),
            ExpressionKind::Variable(Name::Builtin(builtin)) => match builtin {
                name::Builtin::Unit
                | name::Builtin::Num
                | name::Builtin::String
                | name::Builtin::Type => Ok(Ty::Ty),
            },
            ExpressionKind::Imm(value) => Ok(value.ty()),
            ExpressionKind::BinaryOperation { lhs, rhs, .. } => {
                if let Ok(ty) = lhs.ty(tcx) {
                    if !ty.is_subtype_of(&Ty::Num) {
                        tcx.diagnostics.error(
                            "left-hand side of binary operation must be a number",
                            [primary(
                                lhs.span,
                                format!("expected `Num`, got `{ty}`"),
                            )],
                        );
                    }
                }
                if let Ok(ty) = rhs.ty(tcx) {
                    if !ty.is_subtype_of(&Ty::Num) {
                        tcx.diagnostics.error(
                            "right-hand side of binary operation must be a number",
                            [primary(
                                rhs.span,
                                format!("expected `Num`, got `{ty}`"),
                            )],
                        );
                    }
                }

                Ok(Ty::Num)
            }
            ExpressionKind::FunctionCall { name, arguments } => {
                let resolved = function::resolve(
                    name.text(),
                    arguments,
                    span(tcx.file, name.text_range()),
                    tcx,
                );
                tcx.resolved_calls.insert(self.span.low(), resolved);
                let function = match resolved? {
                    function::Ref::SpriteLocal(index) => {
                        &tcx.sprite.functions[index]
                    }
                    function::Ref::TopLevel(index) => {
                        &tcx.top_level_functions[index]
                    }
                };
                function.return_ty.clone()
            }
            ExpressionKind::Error => Err(()),
        }
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
        crate::parser::SyntaxKind::STRING => ExpressionKind::Imm(
            Value::String(parse_string_literal(token.text())),
        ),
        _ => unreachable!(),
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

fn parse_string_literal(lit: &str) -> String {
    // Remove the quotes.
    lit[1..lit.len() - 1].to_owned()
}
