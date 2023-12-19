use crate::{
    ast,
    parser::{SyntaxKind::*, SyntaxNode, SyntaxToken},
};
use rowan::{ast::AstNode, TextSize};

#[derive(Debug)]
pub enum Name {
    User(SyntaxToken),
    Builtin(Builtin),
}

#[derive(Debug)]
pub enum Builtin {
    Num,
}

impl Builtin {
    fn resolve(identifier: &str) -> Option<Self> {
        match identifier {
            "Num" => Some(Self::Num),
            _ => None,
        }
    }
}

impl Name {
    pub fn resolve(identifier: &SyntaxToken) -> Option<Self> {
        let position = identifier.text_range().start();
        identifier
            .parent_ancestors()
            .flat_map(|scope| all_in_exact_scope_at(scope, position))
            .find(|name| name.text() == identifier.text())
            .map(Self::User)
            .or_else(|| Builtin::resolve(identifier.text()).map(Self::Builtin))
    }
}

fn all_in_exact_scope_at(
    scope: SyntaxNode,
    position: TextSize,
) -> Box<dyn Iterator<Item = SyntaxToken>> {
    match scope.kind() {
        DOCUMENT => Box::new(
            ast::Document::cast(scope)
                .unwrap()
                .sprites()
                .filter_map(|sprite| sprite.name()),
        ),
        SPRITE => Box::new(
            ast::Sprite::cast(scope)
                .unwrap()
                .functions()
                .filter_map(|func| func.name()),
        ),
        FN => Box::new(
            ast::Function::cast(scope)
                .unwrap()
                .parameters()
                .into_iter()
                .flat_map(|p| p.parameters())
                .filter_map(|parameter| parameter.internal_name()),
        ),
        BLOCK => Box::new(
            ast::Block::cast(scope)
                .unwrap()
                .statements()
                .take_while(move |statement| {
                    // You can't refer to a variable before its definition.
                    statement.syntax().text_range().end() <= position
                })
                .filter_map(|statement| match statement {
                    ast::Statement::Let(let_) => Some(let_.variable()?),
                    _ => None,
                })
                .collect::<Vec<_>>()
                .into_iter()
                .rev(),
        ),
        _ => Box::new(std::iter::empty()),
    }
}
