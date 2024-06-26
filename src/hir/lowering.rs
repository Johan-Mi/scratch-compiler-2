#![allow(clippy::similar_names)]

use super::{
    parse_string_literal, Block, Costume, Document, Expression, ExpressionKind,
    Field, Function, GlobalVariable, Parameter, Result, Sprite, Statement,
    StatementKind, Struct,
};
use crate::{
    ast,
    comptime::{self, Value},
    diagnostics::{primary, span},
    generator::Generator,
    name::Name,
    parser::SyntaxKind,
    ty::{self, Context, Ty},
};
use codemap::{File, Spanned};
use rowan::{ast::AstNode, TextRange};
use std::collections::{btree_map::Entry, BTreeMap};

impl Document {
    pub fn lower(
        ast: &ast::Document,
        generator: &mut Generator,
        file: &File,
        tcx: &mut Context,
    ) -> Self {
        let mut structs = BTreeMap::<_, Struct>::new();

        for struct_ in ast.structs() {
            let Ok((name, struct_)) = Struct::lower(&struct_, file, tcx) else {
                continue;
            };

            if let Some(old_struct) = structs.get(&name) {
                tcx.diagnostics.error(
                    format!("redefinition of struct `{name}`"),
                    [
                        primary(struct_.name_span, "defined here"),
                        primary(
                            old_struct.name_span,
                            "previously defined here",
                        ),
                    ],
                );
            } else {
                structs.insert(name, struct_);
            }
        }

        let mut sprites = BTreeMap::<_, Sprite>::new();

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

        let functions = ast.functions().map(|it| (it, None)).chain(
            ast.sprites()
                .filter_map(|it| it.name().zip(Some(it.functions())))
                .flat_map(|(name, functions)| {
                    functions.zip(std::iter::repeat_with(move || {
                        Some(name.to_string())
                    }))
                }),
        );
        let functions = std::iter::repeat_with(|| generator.new_u16().into())
            .zip(functions.filter_map(|(function, owning_sprite)| {
                Function::lower(&function, owning_sprite, file, tcx).ok()
            }))
            .collect();

        let variables = ast
            .lets()
            .chain(ast.sprites().flat_map(|it| it.lets()))
            .filter_map(|it| {
                let token = it.variable()?;
                let text_range = token.text_range();
                let belongs_to_stage = token
                    .parent()
                    .and_then(|it| ast::Sprite::cast(it.parent()?)?.name())
                    .is_none();
                let initializer =
                    Expression::lower_opt(it.value(), file, tcx, text_range);
                tcx.maybe_define_comptime_known_variable(
                    token.clone(),
                    &initializer,
                );
                Some(GlobalVariable {
                    token,
                    initializer,
                    belongs_to_stage,
                })
            })
            .collect();

        Self {
            structs,
            sprites,
            functions,
            variables,
        }
    }
}

impl Struct {
    fn lower(
        ast: &ast::Struct,
        file: &File,
        tcx: &mut Context,
    ) -> Result<(String, Self)> {
        let name = ast.name().ok_or_else(|| {
            tcx.diagnostics.error(
                "struct has no name",
                [primary(
                    span(file, ast.syntax().text_range()),
                    "defined here",
                )],
            );
        })?;

        let name_span = span(file, name.text_range());

        let fields = ast
            .fields()
            .map(|it| Self::lower_field(&it, file, tcx))
            .collect();

        Ok((
            name.to_string(),
            Self {
                name_span,
                name_token: name,
                fields,
            },
        ))
    }

    fn lower_field(
        ast: &ast::Field,
        file: &File,
        tcx: &mut Context,
    ) -> Spanned<Field> {
        Spanned {
            node: Field {
                name: ast.name(),
                ty: Expression::lower_opt(
                    ast.ty(),
                    file,
                    tcx,
                    ast.syntax().text_range(),
                ),
            },
            span: span(file, ast.syntax().text_range()),
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

        Ok((name.to_string(), Self { costumes }))
    }
}

impl Costume {
    fn lower(ast: &ast::Costume) -> Result<Self> {
        Ok(Self {
            name: parse_string_literal(&ast.name().ok_or(())?)?,
            path: parse_string_literal(&ast.path().ok_or(())?)?,
        })
    }
}

impl Function {
    fn lower(
        ast: &ast::Function,
        owning_sprite: Option<String>,
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
            generics.iter().cloned().zip(std::iter::repeat(Ok(Ty::Ty))),
        );

        let name = ast.name().ok_or_else(|| {
            tcx.diagnostics
                .error("function has no name", defined_here());
        })?;
        let name_text_range = name.text_range();
        let name = Spanned {
            node: name.text().to_owned(),
            span: span(file, name_text_range),
        };

        let tag = ast.tag().map(|it| parse_string_literal(&it)).transpose()?;

        if let Some(parameters) = ast.parameters() {
            if parameters.parameters().next().is_none() {
                tcx.diagnostics.warning(
                    "empty function parameter list is redundant",
                    [primary(
                        span(file, parameters.syntax().text_range()),
                        "remove these parentheses",
                    )],
                );
            }
        }

        let parameters = ast
            .parameters()
            .into_iter()
            .flat_map(|p| p.parameters())
            .map(|parameter| Parameter::lower(&parameter, file, tcx))
            .collect::<Result<_>>()?;

        let body = ast.body().ok_or_else(|| {
            tcx.diagnostics
                .error("function has no body", defined_here());
        })?;

        let return_ty = ast.return_ty().map_or(
            Expression {
                kind: ExpressionKind::Imm(Value::Ty(Ty::Unit)),
                span: name.span,
            },
            |it| Expression::lower(&it, file, tcx),
        );

        Ok(Self {
            owning_sprite,
            name,
            tag,
            generics,
            parameters,
            return_ty,
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
        let ty = Expression::lower(&ty, file, tcx);

        Ok(Self {
            external_name: match external_name.text() {
                "_" => None,
                name => Some(name.to_owned()),
            },
            internal_name,
            ty,
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
        let kind = match ast {
            ast::Statement::Let(let_) => lower_let(let_, file, tcx),
            ast::Statement::If(if_) => StatementKind::If {
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
            ast::Statement::Repeat(repeat_) => StatementKind::Repeat {
                times: Expression::lower_opt(
                    repeat_.times(),
                    file,
                    tcx,
                    ast.syntax().text_range(),
                ),
                body: Block::lower_opt(repeat_.body(), file, tcx),
            },
            ast::Statement::Forever(forever) => StatementKind::Forever {
                body: Block::lower_opt(forever.body(), file, tcx),
            },
            ast::Statement::While(while_) => StatementKind::While {
                body: Block::lower_opt(while_.body(), file, tcx),
                condition: Expression::lower_opt(
                    while_.condition(),
                    file,
                    tcx,
                    ast.syntax().text_range(),
                ),
            },
            ast::Statement::Until(until_) => StatementKind::Until {
                body: Block::lower_opt(until_.body(), file, tcx),
                condition: Expression::lower_opt(
                    until_.condition(),
                    file,
                    tcx,
                    ast.syntax().text_range(),
                ),
            },
            ast::Statement::For(for_) => StatementKind::For {
                variable: for_.variable().ok_or(()),
                times: Expression::lower_opt(
                    for_.times(),
                    file,
                    tcx,
                    ast.syntax().text_range(),
                ),
                body: Block::lower_opt(for_.body(), file, tcx),
            },
            ast::Statement::Return(return_) => {
                StatementKind::Return(Expression::lower_opt(
                    return_.expression(),
                    file,
                    tcx,
                    return_.syntax().text_range(),
                ))
            }
            ast::Statement::Expr(expr) => {
                StatementKind::Expr(Expression::lower(expr, file, tcx))
            }
        };

        Self {
            kind,
            span: span(file, ast.syntax().text_range()),
        }
    }
}

fn lower_let(
    let_: &ast::Let,
    file: &File,
    tcx: &mut Context<'_>,
) -> StatementKind {
    let Some(variable) = let_.variable() else {
        tcx.diagnostics.error(
            "expected variable name after `let`",
            [primary(span(file, let_.syntax().text_range()), "")],
        );
        return StatementKind::Error;
    };

    let value = if let Some(value) = let_.value() {
        Expression::lower(&value, file, tcx)
    } else {
        tcx.diagnostics.error(
            "expected expression after `=` in variable definition",
            [primary(span(file, let_.syntax().text_range()), "")],
        );
        Expression {
            kind: ExpressionKind::Error,
            span: span(file, variable.text_range()),
        }
    };

    tcx.maybe_define_comptime_known_variable(variable.clone(), &value);

    StatementKind::Let { variable, value }
}

impl Expression {
    fn lower(ast: &ast::Expression, file: &File, tcx: &mut Context) -> Self {
        let mut expr = Self::lower_impl(ast, file, tcx);
        comptime::evaluate(&mut expr, tcx);
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
            return ExpressionKind::Lvalue(var);
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

    ExpressionKind::TypeAscription {
        inner: Box::new(inner),
        ty: Box::new(ty),
    }
}

fn lower_literal(lit: &ast::Literal) -> ExpressionKind {
    let token = lit.syntax().first_token().unwrap();
    match token.kind() {
        crate::parser::SyntaxKind::DECIMAL_NUMBER => {
            ExpressionKind::Imm(Value::Num(token.text().parse().unwrap()))
        }
        crate::parser::SyntaxKind::BINARY_NUMBER => {
            let text = token.text();
            let (is_negative, text) = match text.as_bytes() {
                [b'+', b'0', b'b' | b'B', ..] => (false, &text[3..]),
                [b'-', b'0', b'b' | b'B', ..] => (true, &text[3..]),
                _ => (false, &text[2..]),
            };
            #[allow(clippy::cast_precision_loss)]
            ExpressionKind::Imm(Value::Num(
                u64::from_str_radix(text, 2).unwrap() as f64
                    * if is_negative { -1.0 } else { 1.0 },
            ))
        }
        crate::parser::SyntaxKind::OCTAL_NUMBER => {
            let text = token.text();
            let (is_negative, text) = match text.as_bytes() {
                [b'+', b'0', b'o' | b'O', ..] => (false, &text[3..]),
                [b'-', b'0', b'o' | b'O', ..] => (true, &text[3..]),
                _ => (false, &text[2..]),
            };
            #[allow(clippy::cast_precision_loss)]
            ExpressionKind::Imm(Value::Num(
                u64::from_str_radix(text, 8).unwrap() as f64
                    * if is_negative { -1.0 } else { 1.0 },
            ))
        }
        crate::parser::SyntaxKind::HEXADECIMAL_NUMBER => {
            let text = token.text();
            let (is_negative, text) = match text.as_bytes() {
                [b'+', b'0', b'x' | b'X', ..] => (false, &text[3..]),
                [b'-', b'0', b'x' | b'X', ..] => (true, &text[3..]),
                _ => (false, &text[2..]),
            };
            #[allow(clippy::cast_precision_loss)]
            ExpressionKind::Imm(Value::Num(
                u64::from_str_radix(text, 16).unwrap() as f64
                    * if is_negative { -1.0 } else { 1.0 },
            ))
        }
        crate::parser::SyntaxKind::STRING => parse_string_literal(&token)
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
