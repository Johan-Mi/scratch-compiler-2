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

    pub fn lets(&self) -> impl Iterator<Item = Let> {
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

    pub fn lets(&self) -> impl Iterator<Item = Let> {
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
        let colon = rowan::ast::support::token(&self.syntax, COLON)?;
        self.syntax
            .children_with_tokens()
            .filter_map(NodeOrToken::into_token)
            .find(|it| {
                it.text_range().start() >= colon.text_range().end()
                    && it.kind() == STRING
            })
    }
}

ast_node!(Function: FN);

impl Function {
    pub fn kw_inline(&self) -> Option<SyntaxToken> {
        rowan::ast::support::token(&self.syntax, KW_INLINE)
    }

    pub fn name(&self) -> Option<SyntaxToken> {
        rowan::ast::support::token(&self.syntax, IDENTIFIER)
    }

    pub fn generics(&self) -> Option<Generics> {
        rowan::ast::support::child(&self.syntax)
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

ast_node!(Generics: GENERICS);

impl Generics {
    pub fn iter(&self) -> impl Iterator<Item = SyntaxToken> {
        self.syntax
            .children_with_tokens()
            .filter_map(NodeOrToken::into_token)
            .filter(|it| it.kind() == IDENTIFIER)
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

    pub fn is_comptime(&self) -> bool {
        rowan::ast::support::token(&self.syntax, KW_COMPTIME).is_some()
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
    If(If),
    Repeat(Repeat),
    Forever(Forever),
    While(While),
    Until(Until),
    For(For),
    Expr(Expression),
}

impl AstNode for Statement {
    type Language = crate::parser::Lang;

    fn can_cast(kind: SyntaxKind) -> bool {
        Let::can_cast(kind) || If::can_cast(kind) || Expression::can_cast(kind)
    }

    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            LET => AstNode::cast(node).map(Self::Let),
            IF => AstNode::cast(node).map(Self::If),
            REPEAT => AstNode::cast(node).map(Self::Repeat),
            FOREVER => AstNode::cast(node).map(Self::Forever),
            WHILE => AstNode::cast(node).map(Self::While),
            UNTIL => AstNode::cast(node).map(Self::Until),
            FOR => AstNode::cast(node).map(Self::For),
            _ => Expression::cast(node).map(Self::Expr),
        }
    }

    fn syntax(&self) -> &SyntaxNode {
        match self {
            Self::Let(inner) => &inner.syntax,
            Self::If(inner) => &inner.syntax,
            Self::Repeat(inner) => &inner.syntax,
            Self::Forever(inner) => &inner.syntax,
            Self::While(inner) => &inner.syntax,
            Self::Until(inner) => &inner.syntax,
            Self::For(inner) => &inner.syntax,
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

ast_node!(If: IF);

impl If {
    pub fn condition(&self) -> Option<Expression> {
        rowan::ast::support::child(&self.syntax)
    }

    pub fn then(&self) -> Option<Block> {
        rowan::ast::support::child(&self.syntax)
    }

    pub fn else_clause(&self) -> Option<ElseClause> {
        rowan::ast::support::child(&self.syntax)
    }
}

ast_node!(ElseClause: ELSE_CLAUSE);

impl ElseClause {
    pub fn block(&self) -> Option<Block> {
        rowan::ast::support::child(&self.syntax)
    }

    pub fn if_(&self) -> Option<If> {
        rowan::ast::support::child(&self.syntax)
    }
}

ast_node!(Repeat: REPEAT);

impl Repeat {
    pub fn times(&self) -> Option<Expression> {
        rowan::ast::support::child(&self.syntax)
    }

    pub fn body(&self) -> Option<Block> {
        rowan::ast::support::child(&self.syntax)
    }
}

ast_node!(Forever: FOREVER);

impl Forever {
    pub fn body(&self) -> Option<Block> {
        rowan::ast::support::child(&self.syntax)
    }
}

ast_node!(While: WHILE);

impl While {
    pub fn condition(&self) -> Option<Expression> {
        rowan::ast::support::child(&self.syntax)
    }

    pub fn body(&self) -> Option<Block> {
        rowan::ast::support::child(&self.syntax)
    }
}

ast_node!(Until: UNTIL);

impl Until {
    pub fn condition(&self) -> Option<Expression> {
        rowan::ast::support::child(&self.syntax)
    }

    pub fn body(&self) -> Option<Block> {
        rowan::ast::support::child(&self.syntax)
    }
}

ast_node!(For: FOR);

impl For {
    pub fn variable(&self) -> Option<SyntaxToken> {
        rowan::ast::support::token(&self.syntax, IDENTIFIER)
    }

    pub fn times(&self) -> Option<Expression> {
        rowan::ast::support::child(&self.syntax)
    }

    pub fn body(&self) -> Option<Block> {
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
    Lvalue(Lvalue),
    GenericTypeInstantiation(GenericTypeInstantiation),
    ListLiteral(ListLiteral),
    TypeAscription(TypeAscription),
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
            || Lvalue::can_cast(kind)
            || GenericTypeInstantiation::can_cast(kind)
            || ListLiteral::can_cast(kind)
            || TypeAscription::can_cast(kind)
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
            LVALUE => AstNode::cast(node).map(Self::Lvalue),
            GENERIC_TYPE_INSTANTIATION => {
                AstNode::cast(node).map(Self::GenericTypeInstantiation)
            }
            LIST_LITERAL => AstNode::cast(node).map(Self::ListLiteral),
            TYPE_ASCRIPTION => AstNode::cast(node).map(Self::TypeAscription),
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
            Self::Lvalue(inner) => &inner.syntax,
            Self::GenericTypeInstantiation(inner) => &inner.syntax,
            Self::ListLiteral(inner) => &inner.syntax,
            Self::TypeAscription(inner) => &inner.syntax,
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

ast_node!(Lvalue: LVALUE);

impl Lvalue {
    pub fn inner(&self) -> Option<Expression> {
        rowan::ast::support::child(&self.syntax)
    }
}

ast_node!(GenericTypeInstantiation: GENERIC_TYPE_INSTANTIATION);

impl GenericTypeInstantiation {
    pub fn generic(&self) -> Expression {
        rowan::ast::support::child(&self.syntax).unwrap()
    }

    pub fn type_parameters(&self) -> TypeParameters {
        rowan::ast::support::child(&self.syntax).unwrap()
    }
}

ast_node!(TypeParameters: TYPE_PARAMETERS);

impl TypeParameters {
    pub fn iter(&self) -> impl Iterator<Item = Expression> {
        rowan::ast::support::children(&self.syntax)
    }
}

ast_node!(ListLiteral: LIST_LITERAL);

impl ListLiteral {
    pub fn iter(&self) -> impl Iterator<Item = Expression> {
        rowan::ast::support::children(&self.syntax)
    }
}

ast_node!(TypeAscription: TYPE_ASCRIPTION);

impl TypeAscription {
    pub fn operator(&self) -> SyntaxToken {
        self.syntax
            .children_with_tokens()
            .filter_map(rowan::NodeOrToken::into_token)
            .find(|token| token.kind() == KW_AS)
            .unwrap()
    }

    pub fn inner(&self) -> Option<Expression> {
        let operator = self.operator().text_range().start();
        self.syntax
            .children()
            .take_while(|child| child.text_range().end() <= operator)
            .find_map(AstNode::cast)
    }

    pub fn ty(&self) -> Option<Expression> {
        let operator = self.operator().text_range().end();
        self.syntax
            .children()
            .skip_while(|child| child.text_range().start() < operator)
            .find_map(AstNode::cast)
    }
}
