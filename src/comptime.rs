use crate::{
    hir::{
        Document, Expression, ExpressionKind, GlobalVariable, Statement, StatementKind,
        VisitorPostorderMut,
    },
    name::Name,
    parser::{SyntaxKind, SyntaxToken},
    ty::{self, Ty},
};
use std::fmt;

#[derive(Clone)]
pub enum Value<L = SyntaxToken> {
    Ty(Ty),
    Num(f64),
    String(String),
    Bool(bool),
    Sprite { name: String },
    VariableRef(L),
}

impl<L: fmt::Debug> fmt::Debug for Value<L> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Ty(ty) => fmt::Debug::fmt(ty, f),
            Self::Num(n) => fmt::Debug::fmt(n, f),
            Self::String(s) => fmt::Debug::fmt(s, f),
            Self::Bool(b) => fmt::Debug::fmt(b, f),
            Self::Sprite { name } => f.debug_struct("Sprite").field("name", name).finish(),
            Self::VariableRef(var) => write!(f, "&{var:?}"),
        }
    }
}

impl Value {
    pub fn ty(&self, tcx: &ty::Context) -> Result<Ty, ()> {
        match self {
            Self::Ty(_) => Ok(Ty::Ty),
            Self::Num(_) => Ok(Ty::Num),
            Self::String(_) => Ok(Ty::String),
            Self::Bool(_) => Ok(Ty::Bool),
            Self::Sprite { .. } => Ok(Ty::Sprite),
            Self::VariableRef(var) => tcx.variable_types[var].clone().map(Box::new).map(Ty::Var),
        }
    }
}

pub fn evaluate(expr: &mut Expression, tcx: &ty::Context) {
    match &mut expr.kind {
        ExpressionKind::Variable(Name::User(token)) => {
            expr.kind = ExpressionKind::Imm(
                if let Some(Some(value)) = tcx.comptime_known_variables.get(token) {
                    value.clone()
                } else {
                    match token.parent().map(|it| it.kind()) {
                        Some(SyntaxKind::SPRITE) => Value::Sprite {
                            name: token.to_string(),
                        },
                        Some(SyntaxKind::GENERICS) => Value::Ty(Ty::Generic(token.clone())),
                        _ => return,
                    }
                },
            );
        }
        ExpressionKind::Variable(Name::Builtin(builtin)) => {
            if let Ok(ty) = (*builtin).try_into() {
                expr.kind = ExpressionKind::Imm(Value::Ty(ty));
            }
        }
        ExpressionKind::GenericTypeInstantiation { generic, arguments } => {
            if let [Expression {
                kind: ExpressionKind::Imm(Value::Ty(ty)),
                ..
            }] = &mut **arguments
            {
                expr.kind = ExpressionKind::Imm(Value::Ty(match generic {
                    ty::Generic::Var => Ty::Var(Box::new(ty.clone())),
                    ty::Generic::List => Ty::List(Box::new(ty.clone())),
                }));
            }
        }
        _ => {}
    }
}

// FIXME: get rid of this and use actual compile-time evaluation instead
pub fn is_known(expr: &Expression, tcx: &ty::Context) -> bool {
    matches!(expr.kind, ExpressionKind::Imm(_))
        || matches!(
            &expr.kind,
            ExpressionKind::Variable(Name::User(var))
                if tcx.comptime_known_variables.contains_key(var)
        )
        || matches!(
            &expr.kind,
            ExpressionKind::ListLiteral(items)
                if items.iter().all(|item| is_known(item, tcx))
        )
        || matches!(
            &expr.kind,
            ExpressionKind::TypeAscription { inner, .. }
                if is_known(inner, tcx)
        )
}

pub fn evaluate_all(document: &mut Document, tcx: &mut ty::Context) {
    struct Visitor<'a, 'b> {
        tcx: &'a mut ty::Context<'b>,
    }

    impl VisitorPostorderMut for Visitor<'_, '_> {
        fn visit_global_variable(&mut self, variable: &mut GlobalVariable) {
            self.tcx.maybe_define_comptime_known_variable(
                variable.token.clone(),
                &variable.initializer,
            );
        }

        fn visit_statement(&mut self, statement: &mut Statement) {
            if let StatementKind::Let { variable, value } = &statement.kind {
                self.tcx
                    .maybe_define_comptime_known_variable(variable.clone(), value);
            }
        }

        fn visit_expression(&mut self, expr: &mut Expression) {
            evaluate(expr, self.tcx);
        }
    }

    for function in document.functions.values_mut() {
        tcx.variable_types.extend(
            function
                .generics
                .iter()
                .cloned()
                .zip(std::iter::repeat(Ok(Ty::Ty))),
        );
    }

    Visitor { tcx }.traverse_document(document);
}
