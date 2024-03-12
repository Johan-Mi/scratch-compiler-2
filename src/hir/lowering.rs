use super::{
    parse_string_literal, Block, Costume, Document, Expression, ExpressionKind,
    Function, GlobalVariable, Parameter, Result, Sprite, Statement,
};
use crate::{
    ast,
    comptime::{self, Value},
    diagnostics::{primary, span},
    name::Name,
    parser::{SyntaxKind, SyntaxNode},
    ty::{self, Context, Ty},
};
use codemap::{File, Spanned};
use rowan::{ast::AstNode, TextRange};
use std::collections::{hash_map::Entry, HashMap};

pub fn lower(document: SyntaxNode, file: &File, tcx: &mut Context) -> Document {
    Document::lower(&ast::Document::cast(document).unwrap(), file, tcx)
}

impl Document {
    fn lower(ast: &ast::Document, file: &File, tcx: &mut Context) -> Self {
        let mut sprites = HashMap::<_, Sprite>::new();

        for sprite in ast.sprites() {
            let Ok((name, sprite)) = Sprite::lower(&sprite, file, tcx) else {
                continue;
            };

            match sprites.entry(name) {
                Entry::Occupied(mut existing) => {
                    existing.get_mut().merge(sprite);
                }
                Entry::Vacant(new) => {
                    new.insert(sprite);
                }
            }
        }

        let functions = ast
            .functions()
            .filter_map(|function| Function::lower(&function, file, tcx).ok())
            .enumerate()
            .collect();

        let variables = ast
            .lets()
            .chain(ast.sprites().flat_map(|it| it.lets()))
            .filter_map(|it| {
                let token = it.variable()?;
                let text_range = token.text_range();
                let owning_sprite = token
                    .parent()
                    .and_then(|it| ast::Sprite::cast(it.parent()?)?.name())
                    .map_or_else(
                        || "Stage".to_owned(),
                        |it| it.text().to_owned(),
                    );
                Some(GlobalVariable {
                    token,
                    initializer: Expression::lower_opt(
                        it.value(),
                        file,
                        tcx,
                        text_range,
                    ),
                    owning_sprite,
                })
            })
            .collect();

        Self {
            sprites,
            functions,
            variables,
        }
    }
}

impl Sprite {
    fn lower(
        ast: &ast::Sprite,
        file: &File,
        tcx: &mut Context,
    ) -> Result<(String, Self)> {
        let name = ast.name().ok_or_else(|| {
            tcx.diagnostics.error(
                "sprite has no name",
                [primary(
                    span(file, ast.syntax().text_range()),
                    "defined here",
                )],
            );
        })?;

        let costumes = ast
            .costume_lists()
            .flat_map(|it| it.iter())
            .filter_map(|it| Costume::lower(&it).ok())
            .collect();

        let functions = ast
            .functions()
            .filter_map(|function| Function::lower(&function, file, tcx).ok())
            .enumerate()
            .collect();

        Ok((
            name.to_string(),
            Self {
                costumes,
                functions,
            },
        ))
    }
}

impl Costume {
    fn lower(ast: &ast::Costume) -> Result<Self> {
        Ok(Self {
            name: parse_string_literal(ast.name().ok_or(())?.text())?,
            path: parse_string_literal(ast.path().ok_or(())?.text())?,
        })
    }
}

impl Function {
    fn lower(
        ast: &ast::Function,
        file: &File,
        tcx: &mut Context,
    ) -> Result<Self> {
        let defined_here = || {
            [primary(
                span(file, ast.syntax().text_range()),
                "defined here",
            )]
        };

        let generics = ast
            .generics()
            .into_iter()
            .flat_map(|it| it.iter())
            .collect::<Vec<_>>();

        tcx.variable_types.extend(
            generics
                .iter()
                .map(|it| (it.text_range().start(), Ok(Ty::Ty))),
        );

        let name = ast.name().ok_or_else(|| {
            tcx.diagnostics
                .error("function has no name", defined_here());
        })?;
        let name = Spanned {
            node: name.text().to_owned(),
            span: span(file, name.text_range()),
        };

        let parameters = ast
            .parameters()
            .ok_or_else(|| {
                tcx.diagnostics
                    .error("function has no parameter list", defined_here());
            })
            .into_iter()
            .flat_map(|p| p.parameters())
            .map(|parameter| Parameter::lower(&parameter, file, tcx))
            .collect::<Result<_>>()?;

        let body = ast.body().ok_or_else(|| {
            tcx.diagnostics
                .error("function has no body", defined_here());
        })?;

        let return_ty =
            ast.return_ty().map(|it| Expression::lower(&it, file, tcx));
        let return_ty_span = return_ty.as_ref().map_or(name.span, |it| it.span);
        let return_ty = return_ty.map_or(Ok(Ty::Unit), |expr| {
            let expr_ty = expr.ty(None, tcx)?;
            if !matches!(expr_ty, Ty::Ty) {
                tcx.diagnostics.error(
                    "function return type must be a type",
                    [primary(
                        return_ty_span,
                        format!("expected `Type`, got `{expr_ty}`"),
                    )],
                );
            };
            match expr.kind {
                ExpressionKind::Imm(Value::Ty(ty)) => Ok(ty),
                ExpressionKind::Imm(_) => Err(()),
                _ => {
                    tcx.diagnostics.error(
                        "function return type must be comptime-known",
                        [primary(expr.span, "")],
                    );
                    Err(())
                }
            }
        });

        Ok(Self {
            name,
            generics,
            parameters,
            return_ty: Spanned {
                node: return_ty,
                span: return_ty_span,
            },
            body: Block::lower(&body, file, tcx),
            is_from_builtins: false,
            is_intrinsic: false,
            is_inline: ast.kw_inline().is_some(),
        })
    }
}

impl Parameter {
    fn lower(
        ast: &ast::Parameter,
        file: &File,
        tcx: &mut Context,
    ) -> Result<Self> {
        let external_name = ast.external_name().unwrap().identifier();
        let internal_name = ast.internal_name().ok_or_else(|| {
            tcx.diagnostics.error(
                "function parameter has no internal name",
                [primary(span(file, external_name.text_range()), "")],
            );
        })?;

        let ty = ast.ty().ok_or_else(|| {
            tcx.diagnostics.error(
                "function parameter has no type",
                [primary(span(file, external_name.text_range()), "")],
            );
        })?;
        let expr = Expression::lower(&ty, file, tcx);

        let expr_ty = expr.ty(None, tcx)?;
        if !matches!(expr_ty, Ty::Ty) {
            tcx.diagnostics.error(
                "function parameter type must be a type",
                [primary(
                    expr.span,
                    format!("expected `Type`, got `{expr_ty}`"),
                )],
            );
        };

        let ty = match expr.kind {
            ExpressionKind::Imm(Value::Ty(ty)) => Ok(ty),
            ExpressionKind::Imm(_) => Err(()),
            _ => {
                tcx.diagnostics.error(
                    "function parameter type must be comptime-known",
                    [primary(expr.span, "")],
                );
                Err(())
            }
        };

        Ok(Self {
            external_name: match external_name.text() {
                "_" => None,
                name => Some(name.to_owned()),
            },
            internal_name,
            ty: Spanned {
                node: ty,
                span: expr.span,
            },
            is_comptime: ast.is_comptime(),
            span: span(file, ast.syntax().text_range()),
        })
    }
}

impl Block {
    fn lower(ast: &ast::Block, file: &File, tcx: &mut Context) -> Self {
        Self {
            statements: ast
                .statements()
                .map(|statement| Statement::lower(&statement, file, tcx))
                .collect(),
        }
    }

    fn lower_opt(
        ast: Option<ast::Block>,
        file: &File,
        tcx: &mut Context,
    ) -> Result<Self> {
        ast.map(|block| Self::lower(&block, file, tcx)).ok_or(())
    }
}

impl Statement {
    fn lower(ast: &ast::Statement, file: &File, tcx: &mut Context) -> Self {
        match ast {
            ast::Statement::Let(let_) => {
                let Some(variable) = let_.variable() else {
                    tcx.diagnostics.error(
                        "expected variable name after `let`",
                        [primary(span(file, ast.syntax().text_range()), "")],
                    );
                    return Self::Error;
                };

                let value = if let Some(value) = let_.value() {
                    Expression::lower(&value, file, tcx)
                } else {
                    tcx.diagnostics.error(
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
                    tcx,
                    ast.syntax().text_range(),
                ),
                then: Block::lower_opt(if_.then(), file, tcx),
                else_: if_
                    .else_clause()
                    .and_then(|clause| {
                        clause
                            .block()
                            .map(|block| Block::lower(&block, file, tcx))
                            .or_else(|| {
                                clause.if_().map(|if_| Block {
                                    statements: vec![Self::lower(
                                        &ast::Statement::If(if_),
                                        file,
                                        tcx,
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
                    tcx,
                    ast.syntax().text_range(),
                ),
                body: Block::lower_opt(repeat_.body(), file, tcx),
            },
            ast::Statement::Forever(forever) => Self::Forever {
                body: Block::lower_opt(forever.body(), file, tcx),
                span: span(file, ast.syntax().text_range()),
            },
            ast::Statement::While(while_) => Self::While {
                body: Block::lower_opt(while_.body(), file, tcx),
                condition: Expression::lower_opt(
                    while_.condition(),
                    file,
                    tcx,
                    ast.syntax().text_range(),
                ),
            },
            ast::Statement::Until(until_) => Self::Until {
                body: Block::lower_opt(until_.body(), file, tcx),
                condition: Expression::lower_opt(
                    until_.condition(),
                    file,
                    tcx,
                    ast.syntax().text_range(),
                ),
            },
            ast::Statement::For(for_) => Self::For {
                variable: for_.variable().ok_or(()),
                times: Expression::lower_opt(
                    for_.times(),
                    file,
                    tcx,
                    ast.syntax().text_range(),
                ),
                body: Block::lower_opt(for_.body(), file, tcx),
            },
            ast::Statement::Expr(expr) => {
                Self::Expr(Expression::lower(expr, file, tcx))
            }
        }
    }
}

impl Expression {
    fn lower(ast: &ast::Expression, file: &File, tcx: &mut Context) -> Self {
        let mut expr = Self::lower_impl(ast, file, tcx);
        comptime::evaluate(&mut expr);
        expr
    }

    fn lower_impl(
        ast: &ast::Expression,
        file: &File,
        tcx: &mut Context,
    ) -> Self {
        let kind = match ast {
            ast::Expression::Variable(var) => {
                let identifier = var.identifier();
                Name::resolve(&identifier).map_or_else(
                    || {
                        tcx.diagnostics.error(
                            "undefined variable",
                            [primary(span(file, identifier.text_range()), "")],
                        );
                        ExpressionKind::Error
                    },
                    ExpressionKind::Variable,
                )
            }
            ast::Expression::FunctionCall(call) => {
                lower_function_call(call, file, tcx)
            }
            ast::Expression::BinaryOperation(op) => {
                let operator = op.operator();
                let operator_range = operator.text_range();
                ExpressionKind::FunctionCall {
                    name_or_operator: operator,
                    name_span: span(file, operator_range),
                    arguments: vec![
                        (
                            None,
                            Self::lower_opt(
                                op.lhs(),
                                file,
                                tcx,
                                operator_range,
                            ),
                        ),
                        (
                            None,
                            Self::lower_opt(
                                op.rhs(),
                                file,
                                tcx,
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
                    tcx,
                    expr.syntax().text_range(),
                );
            }
            ast::Expression::NamedArgument(named_arg) => {
                tcx.diagnostics.error(
                    "unexpected named argument",
                    [primary(
                        span(file, named_arg.syntax().text_range()),
                        "expected expression",
                    )],
                );
                let _ = Self::lower_opt(
                    named_arg.value(),
                    file,
                    tcx,
                    named_arg.syntax().text_range(),
                );
                ExpressionKind::Error
            }
            ast::Expression::Literal(lit) => lower_literal(lit),
            ast::Expression::Lvalue(lvalue) => {
                lower_lvalue(lvalue, ast.syntax().text_range(), file, tcx)
            }
            ast::Expression::GenericTypeInstantiation(instantiation) => {
                lower_generic_type_instantiation(instantiation, file, tcx)
            }
            ast::Expression::ListLiteral(list) => ExpressionKind::ListLiteral(
                list.iter().map(|it| Self::lower(&it, file, tcx)).collect(),
            ),
            ast::Expression::TypeAscription(ascription) => {
                lower_type_ascription(ascription, tcx, file)
            }
            ast::Expression::MethodCall(call) => {
                lower_method_call(call, file, tcx)
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
        tcx: &mut Context,
        fallback_text_range: TextRange,
    ) -> Self {
        ast.map_or_else(
            || Self {
                kind: ExpressionKind::Error,
                span: span(file, fallback_text_range),
            },
            |expr| Self::lower(&expr, file, tcx),
        )
    }
}

fn lower_method_call(
    call: &ast::MethodCall,
    file: &File,
    tcx: &mut Context,
) -> ExpressionKind {
    let caller = Expression::lower(&call.caller(), file, tcx);
    let Some(rhs) = call.rhs() else {
        return ExpressionKind::Error;
    };
    let rhs = Expression::lower(&rhs, file, tcx);
    let ExpressionKind::FunctionCall {
        name_or_operator,
        name_span,
        mut arguments,
    } = rhs.kind
    else {
        tcx.diagnostics
            .error("expected function call after `.`", [primary(rhs.span, "")]);
        return ExpressionKind::Error;
    };
    arguments.insert(0, (None, caller));
    ExpressionKind::FunctionCall {
        name_or_operator,
        name_span,
        arguments,
    }
}

fn lower_function_call(
    call: &ast::FunctionCall,
    file: &File,
    tcx: &mut Context,
) -> ExpressionKind {
    let name_or_operator = call.name();
    let name_span = span(file, name_or_operator.text_range());
    ExpressionKind::FunctionCall {
        name_or_operator,
        name_span,
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
                            tcx,
                            named_arg.syntax().text_range(),
                        ),
                    )
                } else {
                    (None, Expression::lower(&arg, file, tcx))
                }
            })
            .collect(),
    }
}

fn lower_lvalue(
    lvalue: &ast::Lvalue,
    text_range: TextRange,
    file: &File,
    tcx: &mut Context,
) -> ExpressionKind {
    let Some(inner) = lvalue.inner() else {
        return ExpressionKind::Error;
    };
    let inner = Expression::lower(&inner, file, tcx);
    if let ExpressionKind::Variable(Name::User(var)) = inner.kind {
        if var.parent().is_some_and(|it| it.kind() == SyntaxKind::LET) {
            return ExpressionKind::Lvalue(var.text_range().start());
        }
    }
    tcx.diagnostics.error(
        "`&` can only be applied to variables declared with `let`",
        [primary(span(file, text_range), "")],
    );
    ExpressionKind::Error
}

fn lower_generic_type_instantiation(
    instantiation: &ast::GenericTypeInstantiation,
    file: &File,
    tcx: &mut Context,
) -> ExpressionKind {
    let generic = Expression::lower(&instantiation.generic(), file, tcx);
    let span = generic.span;
    let Ok(generic) = ty::Generic::try_from(generic) else {
        tcx.diagnostics.error(
            "type parameters can only be applied to generic types",
            [primary(span, "this is not a generic type")],
        );
        return ExpressionKind::Error;
    };

    let arguments = instantiation
        .type_parameters()
        .iter()
        .map(|it| Expression::lower(&it, file, tcx))
        .collect::<Vec<_>>();

    ExpressionKind::GenericTypeInstantiation { generic, arguments }
}

fn lower_type_ascription(
    ascription: &ast::TypeAscription,
    tcx: &mut Context,
    file: &File,
) -> ExpressionKind {
    let Some(inner) = ascription.inner() else {
        return ExpressionKind::Error;
    };
    let Some(ty) = ascription.ty() else {
        return ExpressionKind::Error;
    };

    let inner = Expression::lower(&inner, file, tcx);
    let ty = Expression::lower(&ty, file, tcx);

    let Ok(ty_ty) = ty.ty(None, tcx) else {
        return ExpressionKind::Error;
    };
    if !matches!(ty_ty, Ty::Ty) {
        tcx.diagnostics.error(
            "ascribed type must be a type",
            [primary(ty.span, format!("expected `Type`, got `{ty_ty}`"))],
        );
    };

    let ty = match ty.kind {
        ExpressionKind::Imm(Value::Ty(ty)) => ty,
        ExpressionKind::Imm(_) => return ExpressionKind::Error,
        _ => {
            tcx.diagnostics.error(
                "ascribed type must be comptime-known",
                [primary(ty.span, "")],
            );
            return ExpressionKind::Error;
        }
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
