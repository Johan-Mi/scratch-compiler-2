use crate::{
    comptime,
    diagnostics::{primary, secondary, Diagnostics},
    function::ResolvedCalls,
    hir,
    name::{self, Name},
    parser::SyntaxToken,
};
use codemap::{Span, Spanned};
use std::{
    collections::{BTreeMap, HashMap},
    fmt,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Ty {
    Never,
    Unit,
    Num,
    String,
    Bool,
    Sprite,
    #[expect(clippy::enum_variant_names, reason = "`Type` is a type")]
    Ty,
    Var(Box<Self>),
    List(Box<Self>),
    Generic(SyntaxToken),
    // TODO: which struct is it?
    Struct {
        name: Spanned<String>,
    },
}

impl fmt::Display for Ty {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Never => write!(f, "Never"),
            Self::Unit => write!(f, "Unit"),
            Self::Num => write!(f, "Num"),
            Self::String => write!(f, "String"),
            Self::Bool => write!(f, "Bool"),
            Self::Sprite => write!(f, "Sprite"),
            Self::Ty => write!(f, "Type"),
            Self::Var(inner) => write!(f, "Var[{inner}]"),
            Self::List(inner) => write!(f, "List[{inner}]"),
            Self::Generic(token) => write!(f, "{token}"),
            Self::Struct { name } => write!(f, "{}", name.node),
        }
    }
}

impl TryFrom<name::Builtin> for Ty {
    type Error = ();

    fn try_from(builtin: name::Builtin) -> Result<Self, ()> {
        match builtin {
            name::Builtin::Never => Ok(Self::Never),
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
    pub const fn is_zero_sized(&self) -> bool {
        matches!(self, Self::Never | Self::Unit)
    }

    pub const fn has_runtime_repr(&self) -> bool {
        matches!(
            self,
            Self::Num | Self::String | Self::Bool | Self::Struct { .. }
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

    fn pattern_match(&self, other: Self, constraints: &mut Constraints) -> bool {
        *self == other
            || match (self, other) {
                (_, Self::Generic(name)) => {
                    assert!(constraints.insert(name, self.clone()).is_none());
                    true
                }
                (Self::Var(this), Self::Var(other)) | (Self::List(this), Self::List(other)) => {
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
            hir::ExpressionKind::Variable(Name::Builtin(name::Builtin::Var)) => Ok(Self::Var),
            hir::ExpressionKind::Variable(Name::Builtin(name::Builtin::List)) => Ok(Self::List),
            _ => Err(()),
        }
    }
}

pub fn check<'tcx>(document: &'tcx hir::typed::Document, tcx: &mut Context<'tcx>) {
    tcx.functions = &document.functions;

    tcx.variable_types.extend(
        document
            .structs
            .values()
            .map(|struct_| (struct_.name.node.clone(), Ok(Ty::Ty))),
    );

    tcx.comptime_known_variables
        .extend(document.structs.iter().map(|(name, struct_)| {
            (
                struct_.name.node.clone(),
                Some(comptime::Value::Ty(Ty::Struct {
                    name: Spanned {
                        node: name.clone(),
                        span: struct_.name.span,
                    },
                })),
            )
        }));

    for variable in &document.variables {
        check_global_variable(variable, tcx);
    }
    for function in document.functions.values() {
        if matches!(function.kind, hir::FunctionKind::Regular { .. }) {
            check_function(function, tcx);
        }
    }
}

fn check_global_variable(variable: &hir::GlobalVariable, tcx: &mut Context<'_>) {
    let ty = of_expression(&variable.initializer, None, tcx);
    if matches!(ty, Ok(Ty::List(_))) {
        assert!(tcx
            .comptime_known_variables
            .insert(variable.token.clone(), None)
            .is_none());
    }
    assert!(tcx
        .variable_types
        .insert(variable.token.clone(), ty)
        .is_none());
    if !comptime::is_known(&variable.initializer, tcx) {
        tcx.diagnostics.error(
            "global variable initializer is not comptime-known",
            [primary(variable.initializer.span, "")],
        );
    }
}

fn check_function(function: &hir::typed::Function, tcx: &mut Context) {
    tcx.sprite.clone_from(&function.owning_sprite);
    tcx.function_return_ty.clone_from(&function.return_ty.node);

    tcx.variable_types.extend(
        function
            .parameters
            .iter()
            .map(|parameter| (parameter.internal_name.clone(), parameter.ty.node.clone())),
    );

    tcx.comptime_known_variables.extend(
        function
            .parameters
            .iter()
            .filter(|it| it.is_comptime)
            .map(|it| (it.internal_name.clone(), None)),
    );

    let actual_return_ty = check_block(&function.body, tcx);
    let return_span = function
        .body
        .statements
        .last()
        .map_or(function.name.span, |it| it.span);
    check_return(&actual_return_ty, return_span, tcx);
}

fn check_return(ty: &Result<Ty, ()>, span: Span, tcx: &mut Context) {
    if let (Ok(return_ty), Ok(ty)) = (&tcx.function_return_ty, ty) {
        if !(*ty == Ty::Never || ty == return_ty) {
            tcx.diagnostics.error(
                "return type mismatch",
                [primary(
                    span,
                    format!(
                        "expected `{return_ty}` because of function \
                        return type, got `{ty}`"
                    ),
                )],
            );
        }
    }
}

fn check_statement(statement: &hir::Statement, tcx: &mut Context<'_>) -> Result<Ty, ()> {
    match &statement.kind {
        hir::StatementKind::Let { variable, value } => {
            let ty = of_expression(value, None, tcx);
            if matches!(ty, Ok(Ty::List(_))) {
                assert!(tcx
                    .comptime_known_variables
                    .insert(variable.clone(), None)
                    .is_none());
            }
            assert!(tcx.variable_types.insert(variable.clone(), ty).is_none());
            Ok(Ty::Unit)
        }
        hir::StatementKind::If {
            condition,
            then,
            else_,
        } => {
            if let Ok(condition_ty) = of_expression(condition, None, tcx) {
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
                let _: Result<Ty, ()> = check_block(then, tcx);
            }
            if let Ok(else_) = else_ {
                let _: Result<Ty, ()> = check_block(else_, tcx);
            }
            Ok(Ty::Unit)
        }
        hir::StatementKind::Repeat { times, body } => {
            if let Ok(times_ty) = of_expression(times, None, tcx) {
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
                let _: Result<Ty, ()> = check_block(body, tcx);
            }
            Ok(Ty::Unit)
        }
        hir::StatementKind::Forever { body, .. } => {
            if let Ok(body) = body {
                let _: Result<Ty, ()> = check_block(body, tcx);
            }
            Ok(Ty::Never)
        }
        hir::StatementKind::While { condition, body }
        | hir::StatementKind::Until { condition, body } => {
            if let Ok(condition_ty) = of_expression(condition, None, tcx) {
                if condition_ty != Ty::Bool {
                    let message = if matches!(statement.kind, hir::StatementKind::While { .. }) {
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
                let _: Result<Ty, ()> = check_block(body, tcx);
            }
            Ok(Ty::Unit)
        }
        hir::StatementKind::For {
            variable,
            times,
            body,
        } => Ok(check_for(variable, times, body, tcx)),
        hir::StatementKind::Return(value) => {
            let ascribed = tcx.function_return_ty.clone().ok();
            let ty = of_expression(value, ascribed.as_ref(), tcx);
            check_return(&ty, statement.span, tcx);
            Ok(Ty::Unit)
        }
        hir::StatementKind::Expr(expr) => of_expression(expr, None, tcx),
        hir::StatementKind::Error => Err(()),
    }
}

fn check_for(
    variable: &Result<SyntaxToken, ()>,
    times: &hir::Expression,
    body: &Result<hir::Block, ()>,
    tcx: &mut Context,
) -> Ty {
    if let Ok(times_ty) = of_expression(times, None, tcx) {
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
        assert!(tcx
            .variable_types
            .insert(variable.clone(), Ok(Ty::Num))
            .is_none());
    }
    if let Ok(body) = body {
        let _: Result<Ty, ()> = check_block(body, tcx);
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

impl hir::typed::Function {
    pub fn call_with(
        &self,
        typed_arguments: &[(Option<&str>, Result<Ty, ()>)],
    ) -> Option<Result<Ty, ()>> {
        if self.parameters.len() != typed_arguments.len() {
            return None;
        }
        let mut constraints = Constraints::new();
        if !std::iter::zip(&self.parameters, typed_arguments).all(|(parameter, argument)| {
            parameter.is_compatible_with(argument.0, &argument.1, &mut constraints)
        }) {
            return None;
        }

        Some(self.return_ty.node.clone().map(|mut return_ty| {
            return_ty.apply_constraints(&constraints);
            return_ty
        }))
    }
}

impl hir::typed::Parameter {
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

pub fn of_expression(
    expression: &hir::Expression,
    ascribed: Option<&Ty>,
    tcx: &mut Context,
) -> Result<Ty, ()> {
    use hir::ExpressionKind;

    match &expression.kind {
        ExpressionKind::Variable(Name::User(variable)) => tcx
            .variable_types
            .get(variable)
            .unwrap_or_else(|| panic!("variable `{variable:?}` has no type"))
            .clone(),
        ExpressionKind::Variable(Name::Builtin(builtin)) => {
            of_builtin_name(*builtin, expression.span, tcx.diagnostics)
        }
        ExpressionKind::Imm(comptime::Value::ListRef {
            token,
            initializer: Some(initializer),
        }) => {
            let ty = of_list_literal(initializer, expression.span, ascribed, tcx);
            assert!(tcx
                .variable_types
                .insert(token.clone(), ty.clone())
                .is_none());
            ty
        }
        ExpressionKind::Imm(value) => value.ty(tcx),
        ExpressionKind::FunctionCall {
            name_or_operator,
            name_span,
            arguments,
        } => {
            let name = hir::desugar_function_call_name(name_or_operator);
            let (resolved, return_ty) = crate::function::resolve(name, arguments, *name_span, tcx)?;
            assert!(tcx
                .resolved_calls
                .insert(name_span.low(), resolved)
                .is_none());

            for (param, (_, arg)) in std::iter::zip(&tcx.functions[&resolved].parameters, arguments)
            {
                if param.is_comptime && !comptime::is_known(arg, tcx) {
                    tcx.diagnostics.error(
                        "function argument is not comptime-known",
                        [
                            primary(arg.span, ""),
                            secondary(param.span, "comptime parameter declared here"),
                        ],
                    );
                }
            }

            return_ty
        }
        ExpressionKind::GenericTypeInstantiation { generic, arguments } => {
            check_generic_type_instantiation(*generic, arguments, expression.span, tcx);
            Ok(Ty::Ty)
        }
        ExpressionKind::TypeAscription { inner, ty } => {
            let Ok(ty_ty) = of_expression(ty, None, tcx) else {
                return Err(());
            };
            if !matches!(ty_ty, Ty::Ty) {
                tcx.diagnostics.error(
                    "ascribed type must be a type",
                    [primary(ty.span, format!("expected `Type`, got `{ty_ty}`"))],
                );
            }

            let ty = match &ty.kind {
                ExpressionKind::Imm(comptime::Value::Ty(ty)) => ty,
                ExpressionKind::Imm(_) => return Err(()),
                _ => {
                    tcx.diagnostics.error(
                        "ascribed type must be comptime-known",
                        [primary(ty.span, "")],
                    );
                    return Err(());
                }
            };

            if let Ok(inner_ty) = of_expression(inner, Some(ty), tcx) {
                if !(inner_ty == Ty::Never || inner_ty == *ty) {
                    tcx.diagnostics.error(
                        "type ascription mismatch",
                        [primary(
                            inner.span,
                            format!("expected type `{ty}`, got `{inner_ty}`"),
                        )],
                    );
                }
            }

            Ok(ty.clone())
        }
        ExpressionKind::Error => Err(()),
    }
}

pub fn of_builtin_name(
    builtin: name::Builtin,
    span: Span,
    diagnostics: &mut Diagnostics,
) -> Result<Ty, ()> {
    match builtin {
        name::Builtin::Never
        | name::Builtin::Unit
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
    let first_ty = of_expression(first, ascribed_element_ty, tcx)?;
    for element in rest {
        let ty = of_expression(element, ascribed_element_ty, tcx)?;
        if ty != first_ty {
            tcx.diagnostics.error(
                "conflicting types in list literal",
                [
                    primary(
                        first.span,
                        format!("expected element type `{first_ty}` because of the first item..."),
                    ),
                    primary(element.span, format!("...but this has type `{ty}`")),
                ],
            );
            return Err(());
        }
    }
    Ok(Ty::List(Box::new(first_ty)))
}

type Constraints = HashMap<SyntaxToken, Ty>;

pub struct Context<'a> {
    pub sprite: Option<String>,
    pub functions: &'a BTreeMap<usize, hir::typed::Function>,
    pub diagnostics: &'a mut Diagnostics,
    pub variable_types: HashMap<SyntaxToken, Result<Ty, ()>>,
    pub comptime_known_variables: HashMap<SyntaxToken, Option<comptime::Value>>,
    pub resolved_calls: &'a mut ResolvedCalls,
    pub function_return_ty: Result<Ty, ()>,
}

impl Context<'_> {
    pub fn maybe_define_comptime_known_variable(
        &mut self,
        variable: SyntaxToken,
        value: &hir::Expression,
    ) {
        if let hir::Expression {
            kind: hir::ExpressionKind::Imm(value @ comptime::Value::Ty(_)),
            ..
        } = value
        {
            assert!(self
                .comptime_known_variables
                .insert(variable, Some(value.clone()))
                .is_none());
        }
    }
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
    let Ok(arg_ty) = of_expression(arg, None, tcx) else {
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
    }
}
