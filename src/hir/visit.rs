use super::{
    Document, Expression, ExpressionKind, Function, Sprite, Statement,
};

/// Define a struct, implement this trait, override some `visit_*` methods and
/// traverse the HIR.
pub trait Visitor {
    fn visit_statement(&mut self, _statement: &Statement) {}

    fn visit_expression(&mut self, _expr: &Expression) {}

    fn traverse_document(&mut self, document: &Document) {
        for sprite in document.sprites.values() {
            self.traverse_sprite(sprite);
        }
        for function in document.functions.values() {
            self.traverse_function(function);
        }
    }

    fn traverse_sprite(&mut self, sprite: &Sprite) {
        for function in sprite.functions.values() {
            self.traverse_function(function);
        }
    }

    fn traverse_function(&mut self, function: &Function) {
        for statement in &function.body.statements {
            self.traverse_statement(statement);
        }
    }

    fn traverse_statement(&mut self, statement: &Statement) {
        self.visit_statement(statement);
        match statement {
            Statement::Let { value, .. } | Statement::Expr(value) => {
                self.traverse_expression(value);
            }
            Statement::If { condition, then } => {
                self.traverse_expression(condition);
                if let Ok(then) = then {
                    for statement in &then.statements {
                        self.traverse_statement(statement);
                    }
                }
            }
            Statement::Error => {}
        }
    }

    fn traverse_expression(&mut self, expr: &Expression) {
        self.visit_expression(expr);
        match &expr.kind {
            ExpressionKind::Variable(_)
            | ExpressionKind::Imm(_)
            | ExpressionKind::Error => {}
            ExpressionKind::FunctionCall { arguments, .. } => {
                for (_, arg) in arguments {
                    self.traverse_expression(arg);
                }
            }
        }
    }
}
