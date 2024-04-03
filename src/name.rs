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

#[derive(Debug, Clone, Copy)]
pub enum Builtin {
    Never,
    Unit,
    Num,
    String,
    Bool,
    Var,
    List,
    Type,
}

impl Builtin {
    fn resolve(identifier: &str) -> Option<Self> {
        match identifier {
            "Never" => Some(Self::Never),
            "Unit" => Some(Self::Unit),
            "Num" => Some(Self::Num),
            "String" => Some(Self::String),
            "Bool" => Some(Self::Bool),
            "Var" => Some(Self::Var),
            "List" => Some(Self::List),
            "Type" => Some(Self::Type),
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
        DOCUMENT => {
            let document = ast::Document::cast(scope).unwrap();
            let sprites = document.sprites().filter_map(|sprite| sprite.name());
            let lets = document.lets().filter_map(|it| it.variable());
            Box::new(sprites.chain(lets))
        }
        SPRITE => Box::new(
            ast::Sprite::cast(scope)
                .unwrap()
                .lets()
                .filter_map(|it| it.variable()),
        ),
        FN => {
            let function = ast::Function::cast(scope).unwrap();
            let generics =
                function.generics().into_iter().flat_map(|it| it.iter());
            let parameters = function
                .parameters()
                .into_iter()
                .flat_map(|p| p.parameters())
                .filter_map(|parameter| parameter.internal_name());
            Box::new(generics.chain(parameters))
        }
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
        FOR => {
            let for_ = ast::For::cast(scope).unwrap();
            Box::new(
                for_.variable()
                    // The counter can only be used inside the loop body.
                    .filter(|_| {
                        for_.body().is_some_and(|body| {
                            body.syntax().text_range().contains(position)
                        })
                    })
                    .into_iter(),
            )
        }
        _ => Box::new(std::iter::empty()),
    }
}
