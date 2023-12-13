use crate::parser::{
    SyntaxKind::{self, *},
    SyntaxNode, SyntaxToken,
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
    pub fn name(&self) -> Option<SyntaxToken> {
        rowan::ast::support::token(&self.syntax, IDENTIFIER)
    }

    pub fn functions(&self) -> impl Iterator<Item = Function> {
        rowan::ast::support::children(&self.syntax)
    }
}

ast_node!(Function: FN);

impl Function {
    pub fn name(&self) -> Option<SyntaxToken> {
        rowan::ast::support::token(&self.syntax, IDENTIFIER)
    }
}

ast_node!(Block: BLOCK);

impl Block {
    pub fn statements(&self) -> impl Iterator<Item = Statement> {
        rowan::ast::support::children(&self.syntax)
    }
}

pub enum Statement {
    Let(Let),
}

impl AstNode for Statement {
    type Language = crate::parser::Lang;

    fn can_cast(kind: SyntaxKind) -> bool {
        Let::can_cast(kind)
    }

    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            LET => AstNode::cast(node).map(Self::Let),
            _ => None,
        }
    }

    fn syntax(&self) -> &SyntaxNode {
        match self {
            Self::Let(inner) => &inner.syntax,
        }
    }
}

ast_node!(Let: LET);

impl Let {
    pub fn variable(&self) -> Option<Variable> {
        rowan::ast::support::child(&self.syntax)
    }
}

ast_node!(Variable: VARIABLE);

impl Variable {
    pub fn identifier(&self) -> SyntaxToken {
        rowan::ast::support::token(&self.syntax, IDENTIFIER).unwrap()
    }
}
