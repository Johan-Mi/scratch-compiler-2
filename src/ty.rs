use crate::{
    diagnostics::{primary, Diagnostics},
    function::ResolvedCalls,
    hir,
    parser::SyntaxToken,
};
use rowan::TextSize;
use std::{
    collections::{BTreeMap, HashMap},
    fmt,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Ty {
    Unit,
    Num,
    String,
    Bool,
    Sprite,
    #[allow(clippy::enum_variant_names)]
    Ty,
    Var(Box<Self>),
    Generic(SyntaxToken),
}

impl fmt::Display for Ty {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Unit => write!(f, "Unit"),
            Self::Num => write!(f, "Num"),
            Self::String => write!(f, "String"),
            Self::Bool => write!(f, "Bool"),
            Self::Sprite => write!(f, "Sprite"),
            Self::Ty => write!(f, "Type"),
            Self::Var(inner) => write!(f, "Var[{inner}]"),
            Self::Generic(token) => write!(f, "{token}"),
        }
    }
}

impl Ty {
    pub fn is_subtype_of(&self, other: &Self) -> bool {
        //          v <: t
        // ------   -----------
        // t <: t   Var[v] <: t

        self == other
            || matches!(self, Self::Var(inner) if inner.is_subtype_of(other))
    }

    pub fn is_zero_sized(&self) -> bool {
        *self == Self::Unit
    }

    pub fn has_runtime_repr(&self) -> bool {
        matches!(
            self,
            Self::Num | Self::String | Self::Bool
            // Generics are limited to runtime types for now.
            | Self::Generic(_)
        ) || self.is_zero_sized()
    }
}

pub fn check(
    document: &hir::Document,
    diagnostics: &mut Diagnostics,
) -> ResolvedCalls {
    let mut resolved_calls = HashMap::new();

    let mut tcx = Context {
        sprite: None,
        top_level_functions: &document.functions,
        diagnostics,
        variable_types: HashMap::new(),
        resolved_calls: &mut resolved_calls,
    };

    for sprite in document.sprites.values() {
        tcx.sprite = Some(sprite);
        for function in sprite.functions.values() {
            check_function(function, &mut tcx);
        }
    }

    tcx.sprite = None;
    for function in document.functions.values() {
        if !function.is_builtin {
            check_function(function, &mut tcx);
        }
    }

    resolved_calls
}

fn check_function(function: &hir::Function, tcx: &mut Context) {
    tcx.variable_types
        .extend(function.parameters.iter().map(|parameter| {
            (
                parameter.internal_name.text_range().start(),
                parameter.ty.node.clone(),
            )
        }));

    let actual_return_ty = check_block(&function.body, tcx);
    if let (Ok(return_ty), Ok(actual_return_ty)) =
        (&function.return_ty.node, actual_return_ty)
    {
        if !actual_return_ty.is_subtype_of(return_ty) {
            tcx.diagnostics.error(
                "function has wrong return type",
                [primary(
                    function.name.span,
                    format!("according to the signature, this function should return `{return_ty}` but it actually returns `{actual_return_ty}`"),
                )],
            );
        }
    }
}

fn check_statement(
    statement: &hir::Statement,
    tcx: &mut Context<'_>,
) -> Result<Ty, ()> {
    match statement {
        hir::Statement::Let { variable, value } => {
            let ty = value.ty(tcx).map(Box::new).map(Ty::Var);
            tcx.variable_types.insert(variable.text_range().start(), ty);
            Ok(Ty::Unit)
        }
        hir::Statement::If {
            condition,
            then,
            else_,
        } => {
            if let Ok(condition_ty) = condition.ty(tcx) {
                if !condition_ty.is_subtype_of(&Ty::Bool) {
                    tcx.diagnostics.error(
                        "`if` condition must be a `Bool`",
                        [primary(
                            condition.span,
                            format!("expected `Bool`, got `{condition_ty}`"),
                        )],
                    );
                }
            }
            if let Ok(then) = then {
                let _ = check_block(then, tcx);
            }
            if let Ok(else_) = else_ {
                let _ = check_block(else_, tcx);
            }
            Ok(Ty::Unit)
        }
        hir::Statement::Repeat { times, body } => {
            if let Ok(times_ty) = times.ty(tcx) {
                if !times_ty.is_subtype_of(&Ty::Num) {
                    tcx.diagnostics.error(
                        "repetition count must be a number",
                        [primary(
                            times.span,
                            format!("expected `Num`, got `{times_ty}`"),
                        )],
                    );
                }
            }
            if let Ok(body) = body {
                let _ = check_block(body, tcx);
            }
            Ok(Ty::Unit)
        }
        hir::Statement::Forever { body, .. } => {
            if let Ok(body) = body {
                let _ = check_block(body, tcx);
            }
            Ok(Ty::Unit)
        }
        hir::Statement::While { condition, body }
        | hir::Statement::Until { condition, body } => {
            if let Ok(condition_ty) = condition.ty(tcx) {
                if !condition_ty.is_subtype_of(&Ty::Bool) {
                    let message =
                        if matches!(statement, hir::Statement::While { .. }) {
                            "`while` condition must be a `Bool`"
                        } else {
                            "`until` condition must be a `Bool`"
                        };
                    tcx.diagnostics.error(
                        message,
                        [primary(
                            condition.span,
                            format!("expected `Bool`, got `{condition_ty}`"),
                        )],
                    );
                }
            }
            if let Ok(body) = body {
                let _ = check_block(body, tcx);
            }
            Ok(Ty::Unit)
        }
        hir::Statement::For {
            variable,
            times,
            body,
        } => Ok(check_for(variable, times, body, tcx)),
        hir::Statement::Expr(expr) => expr.ty(tcx),
        hir::Statement::Error => Err(()),
    }
}

fn check_for(
    variable: &Result<SyntaxToken, ()>,
    times: &hir::Expression,
    body: &Result<hir::Block, ()>,
    tcx: &mut Context,
) -> Ty {
    if let Ok(times_ty) = times.ty(tcx) {
        if !times_ty.is_subtype_of(&Ty::Num) {
            tcx.diagnostics.error(
                "repetition count must be a number",
                [primary(
                    times.span,
                    format!("expected `Num`, got `{times_ty}`"),
                )],
            );
        }
    }
    if let Ok(variable) = variable {
        tcx.variable_types
            .insert(variable.text_range().start(), Ok(Ty::Num));
    }
    if let Ok(body) = body {
        let _ = check_block(body, tcx);
    }
    Ty::Unit
}

fn check_block(body: &hir::Block, tcx: &mut Context<'_>) -> Result<Ty, ()> {
    body.statements
        .iter()
        .map(|statement| check_statement(statement, tcx))
        .last()
        .unwrap_or(Ok(Ty::Unit))
}

impl hir::Function {
    pub fn can_be_called_with(
        &self,
        arguments: &[hir::Argument],
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

impl hir::Parameter {
    fn is_compatible_with(
        &self,
        (argument_name, value): &hir::Argument,
        tcx: &mut Context,
    ) -> bool {
        self.external_name.as_deref() == argument_name.as_deref()
            && match (&self.ty.node, value.ty(tcx)) {
                (Ok(parameter_ty), Ok(argument_ty)) => {
                    argument_ty.is_subtype_of(parameter_ty)
                }
                // A type error has already occured; don't let it cascade.
                _ => true,
            }
    }
}

pub struct Context<'a> {
    pub sprite: Option<&'a hir::Sprite>,
    pub top_level_functions: &'a BTreeMap<usize, hir::Function>,
    pub diagnostics: &'a mut Diagnostics,
    pub variable_types: HashMap<TextSize, Result<Ty, ()>>,
    pub resolved_calls: &'a mut ResolvedCalls,
}
