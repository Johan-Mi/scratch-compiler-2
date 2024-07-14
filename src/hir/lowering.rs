#![allow(clippy::similar_names)]

use super::{
    Block, Costume, Document, Expression, ExpressionKind, Field, Function,
    GlobalVariable, Parameter, Result, Sprite, Statement, StatementKind,
    Struct,
};
use crate::{
    ast,
    comptime::Value,
    diagnostics::{primary, span, Diagnostics},
    generator::Generator,
    name::Name,
    parser::{parse_string_literal, SyntaxKind},
    ty::{self, Ty},
};
use codemap::{File, Spanned};
use rowan::{ast::AstNode, TextRange};
use std::collections::{btree_map::Entry, BTreeMap};

impl Document {
    pub fn lower(
        ast: &ast::Document,
        generator: &mut Generator,
        file: &File,
        diagnostics: &mut Diagnostics,
    ) -> Self {
        let mut structs = BTreeMap::<_, Struct>::new();

        for struct_ in ast.structs() {
            let Ok((name, struct_)) =
                Struct::lower(&struct_, file, diagnostics)
            else {
                continue;
            };

            if let Some(old_struct) = structs.get(&name) {
                diagnostics.error(
                    format!("redefinition of struct `{name}`"),
                    [
                        primary(struct_.name.span, "defined here"),
                        primary(
                            old_struct.name.span,
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
            let Ok((name, sprite)) = Sprite::lower(&sprite, file, diagnostics)
            else {
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
                Function::lower(&function, owning_sprite, file, diagnostics)
                    .ok()
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
                let initializer = Expression::lower_opt(
                    it.value(),
                    file,
                    diagnostics,
                    text_range,
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
        diagnostics: &mut Diagnostics,
    ) -> Result<(String, Self)> {
        let name = ast.name().ok_or_else(|| {
            diagnostics.error(
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
            .map(|it| Self::lower_field(&it, file, diagnostics))
            .collect();

        Ok((
            name.to_string(),
            Self {
                name: Spanned {
                    node: name,
                    span: name_span,
                },
                fields,
            },
        ))
    }

    fn lower_field(
        ast: &ast::Field,
        file: &File,
        diagnostics: &mut Diagnostics,
    ) -> Spanned<Field> {
        Spanned {
            node: Field {
                name: ast.name(),
                ty: Expression::lower_opt(
                    ast.ty(),
                    file,
                    diagnostics,
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
        diagnostics: &mut Diagnostics,
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

        let name = ast.name().ok_or_else(|| {
            diagnostics.error("function has no name", defined_here());
        })?;
        let name_text_range = name.text_range();
        let name = Spanned {
            node: name.text().to_owned(),
            span: span(file, name_text_range),
        };

        let tag = ast.tag().map(|it| parse_string_literal(&it)).transpose()?;

        if let Some(parameters) = ast.parameters() {
            if parameters.parameters().next().is_none() {
                diagnostics.warning(
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
            .map(|parameter| Parameter::lower(&parameter, file, diagnostics))
            .collect::<Result<_>>()?;

        let body = ast.body().ok_or_else(|| {
            diagnostics.error("function has no body", defined_here());
        })?;

        let return_ty = ast.return_ty().map_or(
            Expression {
                kind: ExpressionKind::Imm(Value::Ty(Ty::Unit)),
                span: name.span,
            },
            |it| Expression::lower(&it, file, diagnostics),
        );

        Ok(Self {
            owning_sprite,
            name,
            tag,
            generics,
            parameters,
            return_ty,
            body: Block::lower(&body, file, diagnostics),
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

impl Statement {
    fn lower(
        ast: &ast::Statement,
        file: &File,
        diagnostics: &mut Diagnostics,
    ) -> Self {
        let kind = match ast {
            ast::Statement::Let(let_) => lower_let(let_, file, diagnostics),
            ast::Statement::If(if_) => StatementKind::If {
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
            ast::Statement::Repeat(repeat_) => StatementKind::Repeat {
                times: Expression::lower_opt(
                    repeat_.times(),
                    file,
                    diagnostics,
                    ast.syntax().text_range(),
                ),
                body: Block::lower_opt(repeat_.body(), file, diagnostics),
            },
            ast::Statement::Forever(forever) => StatementKind::Forever {
                body: Block::lower_opt(forever.body(), file, diagnostics),
            },
            ast::Statement::While(while_) => StatementKind::While {
                body: Block::lower_opt(while_.body(), file, diagnostics),
                condition: Expression::lower_opt(
                    while_.condition(),
                    file,
                    diagnostics,
                    ast.syntax().text_range(),
                ),
            },
            ast::Statement::Until(until_) => StatementKind::Until {
                body: Block::lower_opt(until_.body(), file, diagnostics),
                condition: Expression::lower_opt(
                    until_.condition(),
                    file,
                    diagnostics,
                    ast.syntax().text_range(),
                ),
            },
            ast::Statement::For(for_) => StatementKind::For {
                variable: for_.variable().ok_or(()),
                times: Expression::lower_opt(
                    for_.times(),
                    file,
                    diagnostics,
                    ast.syntax().text_range(),
                ),
                body: Block::lower_opt(for_.body(), file, diagnostics),
            },
            ast::Statement::Return(return_) => {
                StatementKind::Return(Expression::lower_opt(
                    return_.expression(),
                    file,
                    diagnostics,
                    return_.syntax().text_range(),
                ))
            }
            ast::Statement::Expr(expr) => {
                StatementKind::Expr(Expression::lower(expr, file, diagnostics))
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
    diagnostics: &mut Diagnostics,
) -> StatementKind {
    let Some(variable) = let_.variable() else {
        diagnostics.error(
            "expected variable name after `let`",
            [primary(span(file, let_.syntax().text_range()), "")],
        );
        return StatementKind::Error;
    };

    let value = if let Some(value) = let_.value() {
        Expression::lower(&value, file, diagnostics)
    } else {
        diagnostics.error(
            "expected expression after `=` in variable definition",
            [primary(span(file, let_.syntax().text_range()), "")],
        );
        Expression {
            kind: ExpressionKind::Error,
            span: span(file, variable.text_range()),
        }
    };

    StatementKind::Let { variable, value }
}

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
                    name_span: span(file, operator_range),
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
            ast::Expression::MethodCall(call) => {
                lower_method_call(call, file, diagnostics)
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
}

fn lower_method_call(
    call: &ast::MethodCall,
    file: &File,
    diagnostics: &mut Diagnostics,
) -> ExpressionKind {
    let caller = Expression::lower(&call.caller(), file, diagnostics);
    let Some(rhs) = call.rhs() else {
        return ExpressionKind::Error;
    };
    let rhs = Expression::lower(&rhs, file, diagnostics);
    let ExpressionKind::FunctionCall {
        name_or_operator,
        name_span,
        mut arguments,
    } = rhs.kind
    else {
        diagnostics
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
    diagnostics: &mut Diagnostics,
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
            return ExpressionKind::Lvalue(var);
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
