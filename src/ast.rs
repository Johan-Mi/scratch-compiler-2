use crate::parser::{
    SyntaxKind::{self, *},
    SyntaxNode, SyntaxToken,
};
use rowan::{ast::AstNode, NodeOrToken};

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

    pub fn functions(&self) -> impl Iterator<Item = Function> {
        rowan::ast::support::children(&self.syntax)
    }
}

ast_node!(Sprite: SPRITE);

impl Sprite {
    pub fn name(&self) -> Option<SyntaxToken> {
        rowan::ast::support::token(&self.syntax, IDENTIFIER)
    }

    pub fn costume_lists(&self) -> impl Iterator<Item = CostumeList> {
        rowan::ast::support::children(&self.syntax)
    }

    pub fn functions(&self) -> impl Iterator<Item = Function> {
        rowan::ast::support::children(&self.syntax)
    }
}

ast_node!(CostumeList: COSTUME_LIST);

impl CostumeList {
    pub fn iter(&self) -> impl Iterator<Item = Costume> {
        rowan::ast::support::children(&self.syntax)
    }
}

ast_node!(Costume: COSTUME);

impl Costume {
    pub fn name(&self) -> Option<SyntaxToken> {
        self.syntax
            .children_with_tokens()
            .filter_map(NodeOrToken::into_token)
            .take_while(|it| it.kind() != COLON)
            .find(|it| it.kind() == STRING)
    }

    pub fn path(&self) -> Option<SyntaxToken> {
        self.syntax
            .children_with_tokens()
            .filter_map(NodeOrToken::into_token)
            .skip_while(|it| it.kind() == COLON)
            .find(|it| it.kind() == STRING)
    }
}

ast_node!(Function: FN);

impl Function {
    pub fn name(&self) -> Option<SyntaxToken> {
        rowan::ast::support::token(&self.syntax, IDENTIFIER)
    }

    pub fn parameters(&self) -> Option<FunctionParameters> {
        rowan::ast::support::child(&self.syntax)
    }

    pub fn return_ty(&self) -> Option<Expression> {
        rowan::ast::support::child(&self.syntax)
    }

    pub fn body(&self) -> Option<Block> {
        rowan::ast::support::child(&self.syntax)
    }
}

ast_node!(FunctionParameters: FUNCTION_PARAMETERS);

impl FunctionParameters {
    pub fn parameters(&self) -> impl Iterator<Item = Parameter> {
        rowan::ast::support::children(&self.syntax)
    }
}

ast_node!(Parameter: PARAMETER);

impl Parameter {
    pub fn external_name(&self) -> Option<ExternalParameterName> {
        rowan::ast::support::child(&self.syntax)
    }

    pub fn internal_name(&self) -> Option<SyntaxToken> {
        rowan::ast::support::token(&self.syntax, IDENTIFIER)
    }

    pub fn ty(&self) -> Option<Expression> {
        rowan::ast::support::child(&self.syntax)
    }
}

ast_node!(ExternalParameterName: EXTERNAL_PARAMETER_NAME);

impl ExternalParameterName {
    pub fn identifier(&self) -> SyntaxToken {
        rowan::ast::support::token(&self.syntax, IDENTIFIER).unwrap()
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
    Expr(Expression),
}

impl AstNode for Statement {
    type Language = crate::parser::Lang;

    fn can_cast(kind: SyntaxKind) -> bool {
        Let::can_cast(kind) || Expression::can_cast(kind)
    }

    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            LET => AstNode::cast(node).map(Self::Let),
            _ => Expression::cast(node).map(Self::Expr),
        }
    }

    fn syntax(&self) -> &SyntaxNode {
        match self {
            Self::Let(inner) => &inner.syntax,
            Self::Expr(inner) => inner.syntax(),
        }
    }
}

ast_node!(Let: LET);

impl Let {
    pub fn variable(&self) -> Option<SyntaxToken> {
        rowan::ast::support::token(&self.syntax, IDENTIFIER)
    }

    pub fn value(&self) -> Option<Expression> {
        rowan::ast::support::child(&self.syntax)
    }
}

pub enum Expression {
    Parenthesized(ParenthesizedExpression),
    Variable(Variable),
    FunctionCall(FunctionCall),
    BinaryOperation(BinaryOperation),
    NamedArgument(NamedArgument),
    Literal(Literal),
}

impl AstNode for Expression {
    type Language = crate::parser::Lang;

    fn can_cast(kind: SyntaxKind) -> bool {
        ParenthesizedExpression::can_cast(kind)
            || Variable::can_cast(kind)
            || FunctionCall::can_cast(kind)
            || BinaryOperation::can_cast(kind)
            || NamedArgument::can_cast(kind)
            || Literal::can_cast(kind)
    }

    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            PARENTHESIZED_EXPRESSION => {
                AstNode::cast(node).map(Self::Parenthesized)
            }
            VARIABLE => AstNode::cast(node).map(Self::Variable),
            FUNCTION_CALL => AstNode::cast(node).map(Self::FunctionCall),
            BINARY_EXPRESSION => AstNode::cast(node).map(Self::BinaryOperation),
            NAMED_ARGUMENT => AstNode::cast(node).map(Self::NamedArgument),
            LITERAL => AstNode::cast(node).map(Self::Literal),
            _ => None,
        }
    }

    fn syntax(&self) -> &SyntaxNode {
        match self {
            Self::Variable(inner) => &inner.syntax,
            Self::FunctionCall(inner) => &inner.syntax,
            Self::BinaryOperation(inner) => &inner.syntax,
            Self::Parenthesized(inner) => &inner.syntax,
            Self::NamedArgument(inner) => &inner.syntax,
            Self::Literal(inner) => &inner.syntax,
        }
    }
}

ast_node!(ParenthesizedExpression: PARENTHESIZED_EXPRESSION);

impl ParenthesizedExpression {
    pub fn inner(&self) -> Option<Expression> {
        rowan::ast::support::child(&self.syntax)
    }
}

ast_node!(Variable: VARIABLE);

impl Variable {
    pub fn identifier(&self) -> SyntaxToken {
        rowan::ast::support::token(&self.syntax, IDENTIFIER).unwrap()
    }
}

ast_node!(FunctionCall: FUNCTION_CALL);

impl FunctionCall {
    pub fn name(&self) -> SyntaxToken {
        rowan::ast::support::token(&self.syntax, IDENTIFIER).unwrap()
    }

    pub fn args(&self) -> Arguments {
        rowan::ast::support::child(&self.syntax).unwrap()
    }
}

ast_node!(Arguments: ARGUMENTS);

impl Arguments {
    pub fn iter(&self) -> impl Iterator<Item = Expression> {
        rowan::ast::support::children(&self.syntax)
    }
}

ast_node!(BinaryOperation: BINARY_EXPRESSION);

impl BinaryOperation {
    pub fn operator(&self) -> SyntaxToken {
        self.syntax
            .children_with_tokens()
            .filter_map(rowan::NodeOrToken::into_token)
            .find(|token| token.kind().is_binary_operator())
            .unwrap()
    }

    pub fn lhs(&self) -> Option<Expression> {
        let operator = self.operator().text_range().start();
        self.syntax
            .children()
            .take_while(|child| child.text_range().end() <= operator)
            .find_map(AstNode::cast)
    }

    pub fn rhs(&self) -> Option<Expression> {
        let operator = self.operator().text_range().end();
        self.syntax
            .children()
            .skip_while(|child| child.text_range().start() < operator)
            .find_map(AstNode::cast)
    }
}

ast_node!(NamedArgument: NAMED_ARGUMENT);

impl NamedArgument {
    pub fn name(&self) -> SyntaxToken {
        rowan::ast::support::token(&self.syntax, IDENTIFIER).unwrap()
    }

    pub fn value(&self) -> Option<Expression> {
        rowan::ast::support::child(&self.syntax)
    }
}

ast_node!(Literal: LITERAL);
