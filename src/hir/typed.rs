use super::{Result, Visitor};
use crate::{
    comptime::Value,
    diagnostics::{primary, Diagnostics},
    name::{self, Name},
    parser::SyntaxKind,
    ty::{self, Ty},
};
use codemap::{Pos, Spanned};
use std::collections::{BTreeMap, HashMap};

pub type Document = super::Document<Spanned<Result<Ty>>>;
type Struct = super::Struct<Spanned<Result<Ty>>>;
type Field = super::Field<Spanned<Result<Ty>>>;
pub type Function = super::Function<Spanned<Result<Ty>>>;
type Parameter = super::Parameter<Spanned<Result<Ty>>>;

pub fn lower(
    mut u: super::Document,
    diagnostics: &mut Diagnostics,
) -> (Document, HashMap<Pos, usize>) {
    let mut t = Document {
        structs: BTreeMap::new(),
        sprites: std::mem::take(&mut u.sprites),
        functions: BTreeMap::new(),
        variables: Vec::new(),
    };

    // Repeatedly find something that doesn't depend on things that haven't been lowered yet.
    loop {
        if let Some((name, _)) = u.structs.iter().find(|(_, it)| it.can_be_lowered(&u)) {
            let name = name.clone();
            let r#struct = lower_struct(u.structs.remove(&name).unwrap());
            assert!(t.structs.insert(name, r#struct).is_none());
        } else if let Some((&id, _)) = u.functions.iter().find(|(_, it)| it.can_be_lowered(&u)) {
            let function = lower_function(u.functions.remove(&id).unwrap());
            assert!(t.functions.insert(id, r#function).is_none());
        } else if let Some(id) = u.variables.iter().position(|it| it.can_be_lowered(&u)) {
            let variable = u.variables.swap_remove(id);
            let span = variable.initializer.span;
            let initializer = evaluate(variable.initializer);
            t.variables.push(super::GlobalVariable {
                token: variable.token,
                name_span: variable.name_span,
                initializer: super::Expression {
                    kind: initializer
                        .map_or(super::ExpressionKind::Error, super::ExpressionKind::Imm),
                    span,
                },
                belongs_to_stage: variable.belongs_to_stage,
            });
        } else {
            break;
        }
    }

    for r#struct in u.structs.into_values() {
        diagnostics.error(
            "FIXME lower struct to THIR",
            [primary(r#struct.name.span, "")],
        );
    }
    for function in u.functions.into_values() {
        diagnostics.error(
            "FIXME lower function to THIR",
            [primary(r#function.name.span, "")],
        );
    }
    for variable in u.variables {
        diagnostics.error(
            "FIXME lower variable to THIR",
            [primary(r#variable.name_span, "")],
        );
    }

    (t, HashMap::new())
}

impl super::Struct {
    fn can_be_lowered(&self, u: &super::Document) -> bool {
        let mut visitor = CanBeLoweredVisitor { it_can: true, u };
        visitor.traverse_struct(self);
        visitor.it_can
    }
}

impl super::Function {
    fn can_be_lowered(&self, u: &super::Document) -> bool {
        let mut visitor = CanBeLoweredVisitor { it_can: true, u };
        for parameter in &self.parameters {
            visitor.traverse_expression(&parameter.ty);
        }
        visitor.traverse_expression(&self.return_ty);
        visitor.it_can
    }
}

impl super::GlobalVariable {
    fn can_be_lowered(&self, u: &super::Document) -> bool {
        let mut visitor = CanBeLoweredVisitor { it_can: true, u };
        visitor.traverse_expression(&self.initializer);
        visitor.it_can
    }
}

fn lower_struct(r#struct: super::Struct) -> Struct {
    Struct {
        name: r#struct.name,
        fields: r#struct
            .fields
            .into_iter()
            .map(|it| it.map_node(lower_field))
            .collect(),
    }
}

fn lower_field(field: super::Field) -> Field {
    Field {
        name: field.name,
        ty: require_type(field.ty),
    }
}

fn lower_function(function: super::Function) -> Function {
    Function {
        owning_sprite: function.owning_sprite,
        name: function.name,
        tag: function.tag,
        generics: function.generics,
        parameters: function
            .parameters
            .into_iter()
            .map(lower_parameter)
            .collect(),
        return_ty: require_type(function.return_ty),
        body: function.body,
        kind: function.kind,
    }
}

fn lower_parameter(parameter: super::Parameter) -> Parameter {
    Parameter {
        external_name: parameter.external_name,
        internal_name: parameter.internal_name,
        ty: require_type(parameter.ty),
        is_comptime: parameter.is_comptime,
        span: parameter.span,
    }
}

fn require_type(expr: super::Expression) -> Spanned<Result<Ty>> {
    let span = expr.span;
    Spanned {
        node: if let Ok(Value::Ty(ty)) = evaluate(expr) {
            Ok(ty)
        } else {
            Err(())
        },
        span,
    }
}

struct CanBeLoweredVisitor<'a> {
    it_can: bool,
    u: &'a super::Document,
}

impl Visitor for CanBeLoweredVisitor<'_> {
    fn visit_expression(&mut self, expr: &super::Expression) {
        match &expr.kind {
            super::ExpressionKind::Variable(Name::User(token))
                if self.u.variables.iter().any(|it| it.token == *token) =>
            {
                self.it_can = false;
            }
            super::ExpressionKind::FunctionCall { .. } => self.it_can = false, // TODO
            _ => {}
        }
    }
}

fn evaluate(expr: super::Expression) -> Result<Value> {
    Ok(match expr.kind {
        super::ExpressionKind::Variable(Name::User(token)) => {
            match token.parent().map(|it| it.kind()) {
                Some(SyntaxKind::GENERICS) => Value::Ty(Ty::Generic(token)),
                Some(SyntaxKind::SPRITE) => Value::Sprite {
                    name: token.text().to_owned(),
                },
                _ => todo!(),
            }
        }
        super::ExpressionKind::Variable(Name::Builtin(builtin)) => Value::Ty(match builtin {
            name::Builtin::Never => Ty::Never,
            name::Builtin::Unit => Ty::Unit,
            name::Builtin::Num => Ty::Num,
            name::Builtin::String => Ty::String,
            name::Builtin::Bool => Ty::Bool,
            name::Builtin::Var => todo!(),
            name::Builtin::List => todo!(),
            name::Builtin::Type => Ty::Ty,
        }),
        // Only initialize a list the first time it's referred to.
        super::ExpressionKind::Imm(Value::ListRef { token, .. }) => Value::ListRef {
            token,
            initializer: None,
        },
        super::ExpressionKind::Imm(value) => value,
        super::ExpressionKind::FunctionCall { .. } => todo!(),
        super::ExpressionKind::GenericTypeInstantiation { generic, arguments } => {
            let [ty] = arguments.try_into().unwrap_or_else(|_| todo!());
            let ty = require_type(ty).node?;
            Value::Ty(match generic {
                ty::Generic::Var => Ty::Var(Box::new(ty)),
                ty::Generic::List => Ty::List(Box::new(ty)),
            })
        }
        // TODO: check type
        super::ExpressionKind::TypeAscription { inner, .. } => evaluate(*inner)?,
        super::ExpressionKind::Error => todo!(),
    })
}
