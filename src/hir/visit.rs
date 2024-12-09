use super::{
    Block, Document, Expression, ExpressionKind, Function, GlobalVariable, Statement,
    StatementKind, Struct,
};
use crate::{comptime::Value, hir};
use std::any::Any;

/// Define a struct, implement this trait, override some `visit_*` methods and
/// traverse the HIR.
pub trait Visitor<T: 'static = Expression> {
    fn visit_global_variable(&mut self, _variable: &GlobalVariable) {}

    fn visit_function(&mut self, _function: &Function<T>) {}

    fn visit_block(&mut self, _block: &Block) {}

    fn visit_statement(&mut self, _statement: &Statement) {}

    fn visit_expression(&mut self, _expr: &Expression) {}

    fn traverse_document(&mut self, document: &Document<T>) {
        for struct_ in document.structs.values() {
            self.traverse_struct(struct_);
        }
        for variable in &document.variables {
            self.visit_global_variable(variable);
            self.traverse_expression(&variable.initializer);
        }
        for function in document.functions.values() {
            self.traverse_function(function);
        }
    }

    fn traverse_struct(&mut self, r#struct: &Struct<T>) {
        if let Some(struct_) = <dyn Any>::downcast_ref::<hir::Struct>(r#struct) {
            for field in &struct_.fields {
                self.traverse_expression(&field.ty);
            }
        }
    }

    fn traverse_function(&mut self, function: &Function<T>) {
        self.visit_function(function);
        if let Some(function) = <dyn Any>::downcast_ref::<hir::Function>(function) {
            for parameter in &function.parameters {
                self.traverse_expression(&parameter.ty);
            }
            self.traverse_expression(&function.return_ty);
            self.traverse_block(&function.body);
        } else if let Some(function) = <dyn Any>::downcast_ref::<super::typed::Function>(function) {
            self.traverse_block(&function.body);
        }
    }

    fn traverse_block(&mut self, block: &Block) {
        self.visit_block(block);
        for statement in &block.statements {
            self.traverse_statement(statement);
        }
    }

    fn traverse_statement(&mut self, statement: &Statement) {
        self.visit_statement(statement);
        match &statement.kind {
            StatementKind::Let { value, .. }
            | StatementKind::Return(value)
            | StatementKind::Expr(value) => {
                self.traverse_expression(value);
            }
            StatementKind::If {
                condition,
                then,
                else_,
            } => {
                self.traverse_expression(condition);
                if let Ok(then) = then {
                    self.traverse_block(then);
                }
                if let Ok(else_) = else_ {
                    self.traverse_block(else_);
                }
            }
            StatementKind::Forever { body, .. } => {
                if let Ok(body) = body {
                    self.traverse_block(body);
                }
            }
            StatementKind::Repeat { times: value, body }
            | StatementKind::While {
                condition: value,
                body,
            }
            | StatementKind::Until {
                condition: value,
                body,
            }
            | StatementKind::For {
                times: value, body, ..
            } => {
                self.traverse_expression(value);
                if let Ok(body) = body {
                    self.traverse_block(body);
                }
            }
            StatementKind::Error => {}
        }
    }

    fn traverse_expression(&mut self, expr: &Expression) {
        self.visit_expression(expr);
        match &expr.kind {
            ExpressionKind::Imm(Value::ListRef {
                initializer: Some(arguments),
                ..
            })
            | ExpressionKind::GenericTypeInstantiation { arguments, .. } => {
                for arg in arguments {
                    self.traverse_expression(arg);
                }
            }
            ExpressionKind::Variable(_) | ExpressionKind::Imm(_) | ExpressionKind::Error => {}
            ExpressionKind::FunctionCall { arguments, .. } => {
                for (_, arg) in arguments {
                    self.traverse_expression(arg);
                }
            }
            ExpressionKind::TypeAscription { inner, ty } => {
                self.traverse_expression(inner);
                self.traverse_expression(ty);
            }
        }
    }
}
