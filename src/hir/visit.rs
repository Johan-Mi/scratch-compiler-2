use super::{
    Block, Document, Expression, ExpressionKind, Function, Sprite, Statement,
};

/// Define a struct, implement this trait, override some `visit_*` methods and
/// traverse the HIR.
pub trait Visitor {
    fn visit_function(&mut self, _function: &Function, _is_top_level: bool) {}

    fn visit_block(&mut self, _block: &Block) {}

    fn visit_statement(&mut self, _statement: &Statement) {}

    fn visit_expression(&mut self, _expr: &Expression) {}

    fn traverse_document(&mut self, document: &Document) {
        for sprite in document.sprites.values() {
            self.traverse_sprite(sprite);
        }
        for function in document.functions.values() {
            self.traverse_function(function, true);
        }
        for variable in document.variables.values() {
            self.traverse_expression(variable);
        }
    }

    fn traverse_sprite(&mut self, sprite: &Sprite) {
        for function in sprite.functions.values() {
            self.traverse_function(function, false);
        }
    }

    fn traverse_function(&mut self, function: &Function, is_top_level: bool) {
        self.visit_function(function, is_top_level);
        self.traverse_block(&function.body);
    }

    fn traverse_block(&mut self, block: &Block) {
        self.visit_block(block);
        for statement in &block.statements {
            self.traverse_statement(statement);
        }
    }

    fn traverse_statement(&mut self, statement: &Statement) {
        self.visit_statement(statement);
        match statement {
            Statement::Let { value, .. } | Statement::Expr(value) => {
                self.traverse_expression(value);
            }
            Statement::If {
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
            Statement::Forever { body, .. } => {
                if let Ok(body) = body {
                    self.traverse_block(body);
                }
            }
            Statement::Repeat { times: value, body }
            | Statement::While {
                condition: value,
                body,
            }
            | Statement::Until {
                condition: value,
                body,
            }
            | Statement::For {
                times: value, body, ..
            } => {
                self.traverse_expression(value);
                if let Ok(body) = body {
                    self.traverse_block(body);
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
            | ExpressionKind::Lvalue(_)
            | ExpressionKind::Error => {}
            ExpressionKind::FunctionCall { arguments, .. } => {
                for (_, arg) in arguments {
                    self.traverse_expression(arg);
                }
            }
            ExpressionKind::GenericTypeInstantiation { arguments, .. }
            | ExpressionKind::ListLiteral(arguments) => {
                for arg in arguments {
                    self.traverse_expression(arg);
                }
            }
            ExpressionKind::TypeAscription { inner, .. } => {
                self.traverse_expression(inner);
            }
        }
    }
}
