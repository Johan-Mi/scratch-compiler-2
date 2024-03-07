use crate::{
    diagnostics::{primary, Diagnostics},
    function::ResolvedCalls,
    hir,
    name::{self, Name},
    parser::SyntaxToken,
};
use codemap::Span;
use rowan::TextSize;
use std::{
    collections::{BTreeMap, HashMap, HashSet},
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
    List(Box<Self>),
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
            Self::List(inner) => write!(f, "List[{inner}]"),
            Self::Generic(token) => write!(f, "{token}"),
        }
    }
}

impl TryFrom<name::Builtin> for Ty {
    type Error = ();

    fn try_from(builtin: name::Builtin) -> Result<Self, ()> {
        match builtin {
            name::Builtin::Unit => Ok(Self::Unit),
            name::Builtin::Num => Ok(Self::Num),
            name::Builtin::String => Ok(Self::String),
            name::Builtin::Bool => Ok(Self::Bool),
            name::Builtin::Type => Ok(Self::Ty),
            name::Builtin::Var | name::Builtin::List => Err(()),
        }
    }
}

impl Ty {
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

    fn apply_constraints(&mut self, constraints: &Constraints) {
        match self {
            Self::Generic(generic) => {
                if let Some(ty) = constraints.get(generic) {
                    *self = ty.clone();
                }
            }
            Self::Var(inner) => inner.apply_constraints(constraints),
            _ => {}
        }
    }

    fn pattern_match(
        &self,
        other: Self,
        constraints: &mut Constraints,
    ) -> bool {
        *self == other
            || match (self, other) {
                (_, Self::Generic(name)) => {
                    constraints.insert(name, self.clone());
                    true
                }
                (Self::Var(this), Self::Var(other))
                | (Self::List(this), Self::List(other)) => {
                    this.pattern_match(*other, constraints)
                }
                _ => false,
            }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Generic {
    Var,
    List,
}

impl fmt::Display for Generic {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Self::Var => "Var",
            Self::List => "List",
        })
    }
}

impl TryFrom<hir::Expression> for Generic {
    type Error = ();

    fn try_from(expr: hir::Expression) -> Result<Self, ()> {
        match expr.kind {
            hir::ExpressionKind::Variable(Name::Builtin(
                name::Builtin::Var,
            )) => Ok(Self::Var),
            hir::ExpressionKind::Variable(Name::Builtin(
                name::Builtin::List,
            )) => Ok(Self::List),
            _ => Err(()),
        }
    }
}

pub fn check<'tcx>(document: &'tcx hir::Document, tcx: &mut Context<'tcx>) {
    tcx.top_level_functions = &document.functions;

    for (token, variable) in &document.variables {
        let ty = variable.initializer.ty(None, tcx);
        tcx.variable_types.insert(token.text_range().start(), ty);
    }

    for sprite in document.sprites.values() {
        tcx.sprite = Some(sprite);
        for function in sprite.functions.values() {
            check_function(function, tcx);
        }
    }

    tcx.sprite = None;
    for function in document.functions.values() {
        if !function.is_intrinsic {
            check_function(function, tcx);
        }
    }
}

fn check_function(function: &hir::Function, tcx: &mut Context) {
    tcx.variable_types
        .extend(function.parameters.iter().map(|parameter| {
            (
                parameter.internal_name.text_range().start(),
                parameter.ty.node.clone(),
            )
        }));

    tcx.comptime_known_variables.extend(
        function
            .parameters
            .iter()
            .filter(|it| it.is_comptime)
            .map(|it| it.internal_name.text_range().start()),
    );

    let actual_return_ty = check_block(&function.body, tcx);
    if let (Ok(return_ty), Ok(actual_return_ty)) =
        (&function.return_ty.node, actual_return_ty)
    {
        if actual_return_ty != *return_ty {
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
            let ty = value.ty(None, tcx);
            let pos = variable.text_range().start();
            if matches!(ty, Ok(Ty::List(_))) {
                tcx.comptime_known_variables.insert(pos);
            }
            tcx.variable_types.insert(pos, ty);
            Ok(Ty::Unit)
        }
        hir::Statement::If {
            condition,
            then,
            else_,
        } => {
            if let Ok(condition_ty) = condition.ty(None, tcx) {
                if condition_ty != Ty::Bool {
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
            if let Ok(times_ty) = times.ty(None, tcx) {
                if times_ty != Ty::Num {
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
            if let Ok(condition_ty) = condition.ty(None, tcx) {
                if condition_ty != Ty::Bool {
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
        hir::Statement::Expr(expr) => expr.ty(None, tcx),
        hir::Statement::Error => Err(()),
    }
}

fn check_for(
    variable: &Result<SyntaxToken, ()>,
    times: &hir::Expression,
    body: &Result<hir::Block, ()>,
    tcx: &mut Context,
) -> Ty {
    if let Ok(times_ty) = times.ty(None, tcx) {
        if times_ty != Ty::Num {
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
    pub fn call_with(
        &self,
        typed_arguments: &[(Option<&str>, Result<Ty, ()>)],
    ) -> Option<Result<Ty, ()>> {
        if self.parameters.len() != typed_arguments.len() {
            return None;
        }
        let mut constraints = Constraints::new();
        if !std::iter::zip(&self.parameters, typed_arguments).all(
            |(parameter, argument)| {
                parameter.is_compatible_with(
                    argument.0,
                    &argument.1,
                    &mut constraints,
                )
            },
        ) {
            return None;
        }

        Some(self.return_ty.node.clone().map(|mut return_ty| {
            return_ty.apply_constraints(&constraints);
            return_ty
        }))
    }
}

impl hir::Parameter {
    fn is_compatible_with(
        &self,
        name: Option<&str>,
        ty: &Result<Ty, ()>,
        constraints: &mut Constraints,
    ) -> bool {
        self.external_name.as_deref() == name
            && match (&self.ty.node, ty) {
                (Ok(parameter_ty), Ok(argument_ty)) => {
                    let mut parameter_ty = parameter_ty.clone();
                    parameter_ty.apply_constraints(constraints);
                    (self.is_comptime || parameter_ty.has_runtime_repr())
                        && argument_ty.pattern_match(parameter_ty, constraints)
                }
                // A type error has already occured; don't let it cascade.
                _ => true,
            }
    }
}

pub fn of_builtin_name(
    builtin: name::Builtin,
    span: Span,
    diagnostics: &mut Diagnostics,
) -> Result<Ty, ()> {
    match builtin {
        name::Builtin::Unit
        | name::Builtin::Num
        | name::Builtin::String
        | name::Builtin::Bool
        | name::Builtin::Type => Ok(Ty::Ty),
        name::Builtin::Var => {
            diagnostics.error(
                "generic type `Var` must have one type parameter applied",
                [primary(span, "")],
            );
            Err(())
        }
        name::Builtin::List => {
            diagnostics.error(
                "generic type `List` must have one type parameter applied",
                [primary(span, "")],
            );
            Err(())
        }
    }
}

pub fn of_list_literal(
    list: &[hir::Expression],
    span: Span,
    ascribed: Option<&Ty>,
    tcx: &mut Context,
) -> Result<Ty, ()> {
    let ascribed_element_ty = match ascribed {
        Some(Ty::List(ty)) => Some(&**ty),
        _ => None,
    };
    let [first, rest @ ..] = list else {
        if let Some(ascribed_element_ty) = ascribed_element_ty {
            return Ok(Ty::List(Box::new(ascribed_element_ty.clone())));
        }

        tcx.diagnostics.error(
            "cannot infer type of empty list literal",
            [primary(span, "")],
        );
        tcx.diagnostics.help(
            "use type ascription to specify the element type: `[] as List[T]`",
            [],
        );
        return Err(());
    };
    let first_ty = first.ty(ascribed_element_ty, tcx)?;
    for element in rest {
        let ty = element.ty(ascribed_element_ty, tcx)?;
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

type Constraints = HashMap<SyntaxToken, Ty>;

pub struct Context<'a> {
    pub sprite: Option<&'a hir::Sprite>,
    pub top_level_functions: &'a BTreeMap<usize, hir::Function>,
    pub diagnostics: &'a mut Diagnostics,
    pub variable_types: HashMap<TextSize, Result<Ty, ()>>,
    pub comptime_known_variables: HashSet<TextSize>,
    pub resolved_calls: &'a mut ResolvedCalls,
}

pub fn check_generic_type_instantiation(
    generic: Generic,
    arguments: &[hir::Expression],
    span: Span,
    tcx: &mut Context,
) {
    let [arg] = arguments else {
        tcx.diagnostics.error(
            format!("wrong number of arguments for generic type `{generic}`"),
            [primary(
                span,
                format!("expected 1 argument, got {}", arguments.len()),
            )],
        );
        return;
    };
    let Ok(arg_ty) = arg.ty(None, tcx) else {
        return;
    };
    if !matches!(arg_ty, Ty::Ty) {
        tcx.diagnostics.error(
            format!("type mismatch for argument of generic type `{generic}`"),
            [primary(
                arg.span,
                format!("expected `Type`, got `{arg_ty}`"),
            )],
        );
    };
}
