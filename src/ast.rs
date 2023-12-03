use crate::parser::{
    SyntaxKind::{self, *},
    SyntaxNode,
};
use rowan::ast::AstNode;

macro_rules! ast_node {
    ($Name:ident: $kind:expr) => {
        pub struct $Name {
            syntax: SyntaxNode,
        }

        impl AstNode for $Name {
            type Language = crate::parser::Lang;

            fn can_cast(kind: SyntaxKind) -> bool {
                kind == $kind
            }

            fn cast(syntax: SyntaxNode) -> Option<Self> {
                if Self::can_cast(syntax.kind()) {
                    Some(Self { syntax })
                } else {
                    None
                }
            }

            fn syntax(&self) -> &SyntaxNode {
                &self.syntax
            }
        }
    };
}

ast_node!(Document: DOCUMENT);

impl Document {
    pub fn sprites(&self) -> impl Iterator<Item = Sprite> {
        rowan::ast::support::children(&self.syntax)
    }
}

ast_node!(Sprite: SPRITE);

impl Sprite {
    pub fn functions(&self) -> impl Iterator<Item = Function> {
        rowan::ast::support::children(&self.syntax)
    }
}

ast_node!(Function: FN);
