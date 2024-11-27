use super::{Block, ExpressionKind, FunctionKind, Result};
use crate::{
    comptime::{self, Value},
    diagnostics::primary,
    generator::Generator,
    ty::{self, Context, Ty},
};
use codemap::Spanned;

pub type Document = super::Document<Spanned<Result<Ty>>>;
pub type Struct = super::Struct<Spanned<Result<Ty>>>;
pub type Field = super::Field<Spanned<Result<Ty>>>;
pub type Function = super::Function<Spanned<Result<Ty>>>;
pub type Parameter = super::Parameter<Spanned<Result<Ty>>>;

pub fn lower(mut it: super::Document, tcx: &mut Context, generator: &mut Generator) -> Document {
    comptime::evaluate_all(&mut it, tcx);

    let mut document = Document {
        structs: it
            .structs
            .into_iter()
            .map(|(name, it)| (name, lower_struct(it, tcx)))
            .collect(),
        sprites: it.sprites,
        functions: it
            .functions
            .into_iter()
            .map(|(id, function)| (id, lower_function(function, tcx)))
            .collect(),
        variables: it.variables,
    };

    for (name, struct_) in &document.structs {
        let function = constructor(name.clone(), struct_);
        assert!(document
            .functions
            .insert(usize::from(generator.new_u16()), function)
            .is_none());
    }

    document
}

fn constructor(name: String, struct_: &Struct) -> Function {
    Function {
        owning_sprite: None,
        name: Spanned {
            node: name.clone(),
            span: struct_.name.span,
        },
        tag: None,
        generics: Vec::new(),
        parameters: struct_
            .fields
            .iter()
            .map(|field| Parameter {
                external_name: Some(field.name.to_string()),
                internal_name: field.name.clone(),
                ty: field.ty.clone(),
                is_comptime: false,
                span: field.span,
            })
            .collect(),
        return_ty: Spanned {
            node: Ok(Ty::Struct {
                name: Spanned {
                    node: name,
                    span: struct_.name.span,
                },
            }),
            span: struct_.name.span,
        },
        body: Block::default(),
        kind: FunctionKind::Constructor,
    }
}

pub fn lower_struct(it: super::Struct, tcx: &mut Context) -> Struct {
    Struct {
        name: it.name,
        fields: it
            .fields
            .into_iter()
            .map(|field| {
                let name = field.node.name.clone();
                let ty_span = field.ty.span;
                let ty = ty::of_expression(&field.ty, None, tcx).and_then(|ty_ty| {
                    if !matches!(ty_ty, Ty::Ty) {
                        tcx.diagnostics.error(
                            "struct field type must be a type",
                            [primary(
                                field.node.ty.span,
                                format!("expected `Type`, got `{ty_ty}`"),
                            )],
                        );
                    };
                    match field.node.ty.kind {
                        ExpressionKind::Imm(Value::Ty(ty)) => Ok(ty),
                        ExpressionKind::Imm(_) => Err(()),
                        _ => {
                            tcx.diagnostics.error(
                                "struct field type must be comptime-known",
                                [primary(field.node.ty.span, "")],
                            );
                            Err(())
                        }
                    }
                });
                Spanned {
                    node: Field {
                        name,
                        ty: Spanned {
                            node: ty,
                            span: ty_span,
                        },
                    },
                    span: field.span,
                }
            })
            .collect(),
    }
}

pub fn lower_function(it: super::Function, tcx: &mut Context) -> Function {
    let return_ty = ty::of_expression(&it.return_ty, None, tcx).and_then(|ty_ty| {
        if !matches!(ty_ty, Ty::Ty) {
            tcx.diagnostics.error(
                "function return type must be a type",
                [primary(
                    it.return_ty.span,
                    format!("expected `Type`, got `{ty_ty}`"),
                )],
            );
        };
        match it.return_ty.kind {
            ExpressionKind::Imm(Value::Ty(ty)) => Ok(ty),
            ExpressionKind::Imm(_) => Err(()),
            _ => {
                tcx.diagnostics.error(
                    "function return type must be comptime-known",
                    [primary(it.return_ty.span, "")],
                );
                Err(())
            }
        }
    });

    Function {
        owning_sprite: it.owning_sprite,
        name: it.name,
        tag: it.tag,
        generics: it.generics,
        parameters: it
            .parameters
            .into_iter()
            .map(|parameter| lower_parameter(parameter, tcx))
            .collect(),
        return_ty: Spanned {
            node: return_ty,
            span: it.return_ty.span,
        },
        body: it.body,
        kind: it.kind,
    }
}

pub fn lower_parameter(it: super::Parameter, tcx: &mut Context) -> Parameter {
    let ty = ty::of_expression(&it.ty, None, tcx).and_then(|ty_ty| {
        if !matches!(ty_ty, Ty::Ty) {
            tcx.diagnostics.error(
                "function parameter type must be a type",
                [primary(
                    it.ty.span,
                    format!("expected `Type`, got `{ty_ty}`"),
                )],
            );
        };

        match it.ty.kind {
            ExpressionKind::Imm(Value::Ty(ty)) => Ok(ty),
            ExpressionKind::Imm(_) => Err(()),
            _ => {
                tcx.diagnostics.error(
                    "function parameter type must be comptime-known",
                    [primary(it.ty.span, "")],
                );
                Err(())
            }
        }
    });

    Parameter {
        external_name: it.external_name,
        internal_name: it.internal_name,
        ty: Spanned {
            node: ty,
            span: it.ty.span,
        },
        is_comptime: it.is_comptime,
        span: it.span,
    }
}
