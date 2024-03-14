use super::{Block, ExpressionKind, Result};
use crate::{
    comptime::Value,
    diagnostics::primary,
    parser::SyntaxToken,
    ty::{Context, Ty},
};
use codemap::{Span, Spanned};

pub type Document = super::Document<Function>;
pub type Sprite = super::Sprite<Function>;

#[derive(Debug)]
pub struct Function {
    pub name: Spanned<String>,
    pub generics: Vec<SyntaxToken>,
    pub parameters: Vec<Parameter>,
    pub return_ty: Spanned<Result<Ty>>,
    pub body: Block,
    pub is_from_builtins: bool,
    pub is_intrinsic: bool,
    pub is_inline: bool,
}

#[derive(Debug)]
pub struct Parameter {
    pub external_name: Option<String>,
    pub internal_name: SyntaxToken,
    pub ty: Spanned<Result<Ty>>,
    pub is_comptime: bool,
    pub span: Span,
}

pub fn lower(it: super::Document, tcx: &mut Context) -> Document {
    Document {
        sprites: it
            .sprites
            .into_iter()
            .map(|(id, sprite)| (id, lower_sprite(sprite, tcx)))
            .collect(),
        functions: it
            .functions
            .into_iter()
            .map(|(id, function)| (id, lower_function(function, tcx)))
            .collect(),
        variables: it.variables,
    }
}

pub fn lower_sprite(it: super::Sprite, tcx: &mut Context) -> Sprite {
    Sprite {
        costumes: it.costumes,
        functions: it
            .functions
            .into_iter()
            .map(|(id, function)| (id, lower_function(function, tcx)))
            .collect(),
    }
}

pub fn lower_function(it: super::Function, tcx: &mut Context) -> Function {
    let return_ty = it.return_ty.ty(None, tcx).and_then(|ty_ty| {
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
        name: it.name,
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
        is_from_builtins: it.is_from_builtins,
        is_intrinsic: it.is_intrinsic,
        is_inline: it.is_inline,
    }
}

pub fn lower_parameter(it: super::Parameter, tcx: &mut Context) -> Parameter {
    let ty = it.ty.ty(None, tcx).and_then(|ty_ty| {
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
