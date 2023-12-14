use crate::{
    ast,
    parser::{SyntaxKind::*, SyntaxNode, SyntaxToken},
};
use rowan::ast::AstNode;

pub enum Name {
    User(SyntaxToken),
    Builtin(Builtin),
}

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
        identifier
            .parent_ancestors()
            .find_map(|scope| resolve_in_scope(scope, identifier))
            .map(Self::User)
            .or_else(|| Builtin::resolve(identifier.text()).map(Self::Builtin))
    }
}

fn resolve_in_scope(
    scope: SyntaxNode,
    identifier: &SyntaxToken,
) -> Option<SyntaxToken> {
    let start = identifier.text_range().start();
    let identifier = identifier.text();
    match scope.kind() {
        DOCUMENT => ast::Document::cast(scope)?
            .sprites()
            .filter_map(|sprite| sprite.name())
            .find(|name| name.text() == identifier),
        SPRITE => ast::Sprite::cast(scope)?
            .functions()
            .filter_map(|func| func.name())
            .find(|name| name.text() == identifier),
        BLOCK => ast::Block::cast(scope)?
            .statements()
            .take_while(|statement| {
                // You can't refer to a variable before its definition.
                statement.syntax().text_range().end() <= start
            })
            .find_map(|statement| match statement {
                ast::Statement::Let(let_) => Some(let_.variable()?),
                _ => None,
            }),
        _ => None,
    }
}
