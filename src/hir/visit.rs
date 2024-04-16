use super::{
    Block, Document, Expression, ExpressionKind, GlobalVariable, Statement,
    StatementKind,
};

/// Define a struct, implement this trait, override some `visit_*` methods and
/// traverse the HIR.
pub trait Visitor<Func: HasBody = super::typed::Function> {
    fn visit_global_variable(&mut self, _variable: &GlobalVariable) {}

    fn visit_function(&mut self, _function: &Func) {}

    fn visit_block(&mut self, _block: &Block) {}

    fn visit_statement(&mut self, _statement: &Statement) {}

    fn visit_expression(&mut self, _expr: &Expression) {}

    fn traverse_document<Struc>(&mut self, document: &Document<Func, Struc>) {
        for variable in &document.variables {
            self.visit_global_variable(variable);
            self.traverse_expression(&variable.initializer);
        }
        for function in document.functions.values() {
            self.traverse_function(function);
        }
    }

    fn traverse_function(&mut self, function: &Func) {
        self.visit_function(function);
        self.traverse_block(function.body());
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

pub trait HasBody {
    fn body(&self) -> &Block;
}

impl HasBody for super::Function {
    fn body(&self) -> &Block {
        &self.body
    }
}

impl HasBody for super::typed::Function {
    fn body(&self) -> &Block {
        &self.body
    }
}
