use crate::parser::{SyntaxNode, SyntaxToken, K};
use rowan::{ast::AstNode, NodeOrToken};

macro_rules! node {
    ($Name:ident) => {
        pub struct $Name {
            syntax: SyntaxNode,
        }

        impl AstNode for $Name {
            type Language = crate::parser::Lang;

            fn can_cast(kind: K) -> bool {
                kind == K::$Name
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

node!(Document);

impl Document {
    pub fn imports(&self) -> impl Iterator<Item = Import> {
        rowan::ast::support::children(&self.syntax)
    }

    pub fn structs(&self) -> impl Iterator<Item = Struct> {
        rowan::ast::support::children(&self.syntax)
    }

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

node!(Import);

impl Import {
    pub fn path(&self) -> Option<SyntaxToken> {
        rowan::ast::support::token(&self.syntax, K::String)
    }
}

node!(Struct);

impl Struct {
    pub fn name(&self) -> Option<SyntaxToken> {
        rowan::ast::support::token(&self.syntax, K::Identifier)
    }

    pub fn fields(&self) -> impl Iterator<Item = FieldDefinition> {
        rowan::ast::support::children(&self.syntax)
    }
}

node!(FieldDefinition);

impl FieldDefinition {
    pub fn name(&self) -> SyntaxToken {
        rowan::ast::support::token(&self.syntax, K::Identifier).unwrap()
    }

    pub fn ty(&self) -> Option<Expression> {
        rowan::ast::support::child(&self.syntax)
    }
}

node!(Sprite);

impl Sprite {
    pub fn name(&self) -> Option<SyntaxToken> {
        rowan::ast::support::token(&self.syntax, K::Identifier)
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

node!(CostumeList);

impl CostumeList {
    pub fn iter(&self) -> impl Iterator<Item = Costume> {
        rowan::ast::support::children(&self.syntax)
    }
}

node!(Costume);

impl Costume {
    pub fn name(&self) -> Option<SyntaxToken> {
        self.syntax
            .children_with_tokens()
            .filter_map(NodeOrToken::into_token)
            .take_while(|it| it.kind() != K::Colon)
            .find(|it| it.kind() == K::String)
    }

    pub fn path(&self) -> Option<SyntaxToken> {
        let colon = rowan::ast::support::token(&self.syntax, K::Colon)?;
        self.syntax
            .children_with_tokens()
            .filter_map(NodeOrToken::into_token)
            .find(|it| {
                it.text_range().start() >= colon.text_range().end() && it.kind() == K::String
            })
    }
}

node!(Function);

impl Function {
    pub fn kw_inline(&self) -> Option<SyntaxToken> {
        rowan::ast::support::token(&self.syntax, K::KwInline)
    }

    pub fn name(&self) -> Option<SyntaxToken> {
        rowan::ast::support::token(&self.syntax, K::Identifier)
    }

    pub fn tag(&self) -> Option<SyntaxToken> {
        rowan::ast::support::token(&self.syntax, K::String)
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

node!(Generics);

impl Generics {
    pub fn iter(&self) -> impl Iterator<Item = SyntaxToken> {
        self.syntax
            .children_with_tokens()
            .filter_map(NodeOrToken::into_token)
            .filter(|it| it.kind() == K::Identifier)
    }
}

node!(FunctionParameters);

impl FunctionParameters {
    pub fn parameters(&self) -> impl Iterator<Item = Parameter> {
        rowan::ast::support::children(&self.syntax)
    }
}

node!(Parameter);

impl Parameter {
    pub fn external_name(&self) -> Option<ExternalParameterName> {
        rowan::ast::support::child(&self.syntax)
    }

    pub fn internal_name(&self) -> Option<SyntaxToken> {
        rowan::ast::support::token(&self.syntax, K::Identifier)
    }

    pub fn ty(&self) -> Option<Expression> {
        rowan::ast::support::child(&self.syntax)
    }

    pub fn is_comptime(&self) -> bool {
        rowan::ast::support::token(&self.syntax, K::KwComptime).is_some()
    }
}

node!(ExternalParameterName);

impl ExternalParameterName {
    pub fn identifier(&self) -> SyntaxToken {
        rowan::ast::support::token(&self.syntax, K::Identifier).unwrap()
    }
}

node!(Block);

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
    Return(Return),
    Expr(Expression),
}

impl AstNode for Statement {
    type Language = crate::parser::Lang;

    fn can_cast(kind: K) -> bool {
        Let::can_cast(kind)
            || If::can_cast(kind)
            || Repeat::can_cast(kind)
            || Forever::can_cast(kind)
            || While::can_cast(kind)
            || Until::can_cast(kind)
            || For::can_cast(kind)
            || Return::can_cast(kind)
            || Expression::can_cast(kind)
    }

    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            K::Let => AstNode::cast(node).map(Self::Let),
            K::If => AstNode::cast(node).map(Self::If),
            K::Repeat => AstNode::cast(node).map(Self::Repeat),
            K::Forever => AstNode::cast(node).map(Self::Forever),
            K::While => AstNode::cast(node).map(Self::While),
            K::Until => AstNode::cast(node).map(Self::Until),
            K::For => AstNode::cast(node).map(Self::For),
            K::Return => AstNode::cast(node).map(Self::Return),
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
            Self::Return(inner) => &inner.syntax,
            Self::Expr(inner) => inner.syntax(),
        }
    }
}

node!(Let);

impl Let {
    pub fn variable(&self) -> Option<SyntaxToken> {
        rowan::ast::support::token(&self.syntax, K::Identifier)
    }

    pub fn value(&self) -> Option<Expression> {
        rowan::ast::support::child(&self.syntax)
    }
}

node!(If);

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

node!(ElseClause);

impl ElseClause {
    pub fn block(&self) -> Option<Block> {
        rowan::ast::support::child(&self.syntax)
    }

    pub fn if_(&self) -> Option<If> {
        rowan::ast::support::child(&self.syntax)
    }
}

node!(Repeat);

impl Repeat {
    pub fn times(&self) -> Option<Expression> {
        rowan::ast::support::child(&self.syntax)
    }

    pub fn body(&self) -> Option<Block> {
        rowan::ast::support::child(&self.syntax)
    }
}

node!(Forever);

impl Forever {
    pub fn body(&self) -> Option<Block> {
        rowan::ast::support::child(&self.syntax)
    }
}

node!(While);

impl While {
    pub fn condition(&self) -> Option<Expression> {
        rowan::ast::support::child(&self.syntax)
    }

    pub fn body(&self) -> Option<Block> {
        rowan::ast::support::child(&self.syntax)
    }
}

node!(Until);

impl Until {
    pub fn condition(&self) -> Option<Expression> {
        rowan::ast::support::child(&self.syntax)
    }

    pub fn body(&self) -> Option<Block> {
        rowan::ast::support::child(&self.syntax)
    }
}

node!(For);

impl For {
    pub fn variable(&self) -> Option<SyntaxToken> {
        rowan::ast::support::token(&self.syntax, K::Identifier)
    }

    pub fn times(&self) -> Option<Expression> {
        rowan::ast::support::child(&self.syntax)
    }

    pub fn body(&self) -> Option<Block> {
        rowan::ast::support::child(&self.syntax)
    }
}

node!(Return);

impl Return {
    pub fn expression(&self) -> Option<Expression> {
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
    MethodCall(MethodCall),
}

impl AstNode for Expression {
    type Language = crate::parser::Lang;

    fn can_cast(kind: K) -> bool {
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
            || MethodCall::can_cast(kind)
    }

    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            K::ParenthesizedExpression => AstNode::cast(node).map(Self::Parenthesized),
            K::Variable => AstNode::cast(node).map(Self::Variable),
            K::FunctionCall => AstNode::cast(node).map(Self::FunctionCall),
            K::BinaryOperation => AstNode::cast(node).map(Self::BinaryOperation),
            K::NamedArgument => AstNode::cast(node).map(Self::NamedArgument),
            K::Literal => AstNode::cast(node).map(Self::Literal),
            K::Lvalue => AstNode::cast(node).map(Self::Lvalue),
            K::GenericTypeInstantiation => AstNode::cast(node).map(Self::GenericTypeInstantiation),
            K::ListLiteral => AstNode::cast(node).map(Self::ListLiteral),
            K::TypeAscription => AstNode::cast(node).map(Self::TypeAscription),
            K::MethodCall => AstNode::cast(node).map(Self::MethodCall),
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
            Self::MethodCall(inner) => &inner.syntax,
        }
    }
}

node!(ParenthesizedExpression);

impl ParenthesizedExpression {
    pub fn inner(&self) -> Option<Expression> {
        rowan::ast::support::child(&self.syntax)
    }
}

node!(Variable);

impl Variable {
    pub fn identifier(&self) -> SyntaxToken {
        rowan::ast::support::token(&self.syntax, K::Identifier).unwrap()
    }
}

node!(FunctionCall);

impl FunctionCall {
    pub fn name(&self) -> SyntaxToken {
        rowan::ast::support::token(&self.syntax, K::Identifier).unwrap()
    }

    pub fn args(&self) -> Arguments {
        rowan::ast::support::child(&self.syntax).unwrap()
    }
}

node!(Arguments);

impl Arguments {
    pub fn iter(&self) -> impl Iterator<Item = Expression> {
        rowan::ast::support::children(&self.syntax)
    }
}

node!(BinaryOperation);

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

node!(NamedArgument);

impl NamedArgument {
    pub fn name(&self) -> SyntaxToken {
        rowan::ast::support::token(&self.syntax, K::Identifier).unwrap()
    }

    pub fn value(&self) -> Option<Expression> {
        rowan::ast::support::child(&self.syntax)
    }
}

node!(Literal);

node!(Lvalue);

impl Lvalue {
    pub fn inner(&self) -> Option<Expression> {
        rowan::ast::support::child(&self.syntax)
    }
}

node!(GenericTypeInstantiation);

impl GenericTypeInstantiation {
    pub fn generic(&self) -> Expression {
        rowan::ast::support::child(&self.syntax).unwrap()
    }

    pub fn type_parameters(&self) -> TypeParameters {
        rowan::ast::support::child(&self.syntax).unwrap()
    }
}

node!(TypeParameters);

impl TypeParameters {
    pub fn iter(&self) -> impl Iterator<Item = Expression> {
        rowan::ast::support::children(&self.syntax)
    }
}

node!(ListLiteral);

impl ListLiteral {
    pub fn iter(&self) -> impl Iterator<Item = Expression> {
        rowan::ast::support::children(&self.syntax)
    }

    pub fn lbracket(&self) -> SyntaxToken {
        rowan::ast::support::token(&self.syntax, K::Lbracket).unwrap()
    }
}

node!(TypeAscription);

impl TypeAscription {
    pub fn operator(&self) -> SyntaxToken {
        self.syntax
            .children_with_tokens()
            .filter_map(rowan::NodeOrToken::into_token)
            .find(|token| token.kind() == K::KwAs)
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

node!(MethodCall);

impl MethodCall {
    pub fn dot(&self) -> SyntaxToken {
        self.syntax
            .children_with_tokens()
            .filter_map(rowan::NodeOrToken::into_token)
            .find(|token| token.kind() == K::Dot)
            .unwrap()
    }

    pub fn caller(&self) -> Expression {
        let operator = self.dot().text_range().start();
        self.syntax
            .children()
            .take_while(|child| child.text_range().end() <= operator)
            .find_map(AstNode::cast)
            .unwrap()
    }

    pub fn rhs(&self) -> Option<Expression> {
        let operator = self.dot().text_range().end();
        self.syntax
            .children()
            .skip_while(|child| child.text_range().start() < operator)
            .find_map(AstNode::cast)
    }
}
