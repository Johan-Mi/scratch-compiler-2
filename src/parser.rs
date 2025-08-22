use crate::diagnostics::{primary, secondary, Diagnostics};
use codemap::Span;
use logos::Logos;
use rowan::{Checkpoint, GreenNodeBuilder};

pub fn parse(file: &codemap::File, diagnostics: &mut Diagnostics) -> SyntaxNode {
    let source_code = file.source();
    let tokens = &K::lexer(source_code)
        .spanned()
        .map(|(token, span)| Token {
            kind: token.unwrap_or(K::Error),
            text: &source_code[span.clone()],
            span: file.span.subspan(span.start as u64, span.end as u64),
        })
        .collect::<Vec<_>>();
    Parser {
        builder: GreenNodeBuilder::new(),
        tokens,
        span: file.span,
        diagnostics,
    }
    .parse()
}

pub fn parse_string_literal(token: &SyntaxToken) -> Result<String, ()> {
    let mut res = String::new();
    let mut chars = token.text().chars().skip(1);
    loop {
        match chars.next() {
            Some('"') => return Ok(res),
            Some('\\') => match chars.next() {
                Some(c @ ('"' | '\\')) => res.push(c),
                Some('n') => res.push('\n'),
                Some(_) => todo!("invalid escape sequence"),
                None => todo!("unfinished escape sequence"),
            },
            Some(c) => res.push(c),
            // Unterminated string literal. This is already checked for in
            // `syntax_errors`.
            None => return Err(()),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Logos)]
#[repr(u16)]
pub enum K {
    Eof = 0,
    #[regex(r"(\p{Whitespace}|#.*)+")]
    Trivia,

    Document,
    Import,
    Struct,
    FieldDefinition,
    Sprite,
    CostumeList,
    Costume,
    Fn,
    Generics,
    FunctionParameters,
    Parameter,
    ExternalParameterName,
    Block,
    Variable,
    FunctionCall,
    Arguments,
    NamedArgument,
    Let,
    If,
    ElseClause,
    Repeat,
    Forever,
    While,
    Until,
    For,
    ParenthesizedExpression,
    BinaryExpression,
    Literal,
    Lvalue,
    GenericTypeInstantiation,
    TypeParameters,
    ListLiteral,
    TypeAscription,
    MethodCall,
    Return,

    #[token("(")]
    Lparen,
    #[token(")")]
    Rparen,
    #[token("{")]
    Lbrace,
    #[token("}")]
    Rbrace,
    #[token("[")]
    Lbracket,
    #[token("]")]
    Rbracket,
    #[token("->")]
    Arrow,
    #[token(":")]
    Colon,
    #[token(",")]
    Comma,
    #[token("=")]
    Eq,
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Star,
    #[token("/")]
    Slash,
    #[token("%")]
    Percent,
    #[token("<")]
    Lt,
    #[token("==")]
    EqEq,
    #[token(">")]
    Gt,
    #[token("&")]
    Ampersand,
    #[token(".")]
    Dot,

    #[token("import")]
    KwImport,
    #[token("struct")]
    KwStruct,
    #[token("sprite")]
    KwSprite,
    #[token("inline")]
    KwInline,
    #[token("fn")]
    KwFn,
    #[token("let")]
    KwLet,
    #[token("costumes")]
    KwCostumes,
    #[token("false")]
    KwFalse,
    #[token("true")]
    KwTrue,
    #[token("if")]
    KwIf,
    #[token("else")]
    KwElse,
    #[token("repeat")]
    KwRepeat,
    #[token("forever")]
    KwForever,
    #[token("while")]
    KwWhile,
    #[token("until")]
    KwUntil,
    #[token("for")]
    KwFor,
    #[token("comptime")]
    KwComptime,
    #[token("as")]
    KwAs,
    #[token("return")]
    KwReturn,

    #[regex(r"[\p{XID_Start}_][\p{XID_Continue}-]*")]
    Identifier,

    #[regex(r"[+-]?[0-9]+(\.[0-9]+)?([eE][+-]?[0-9]+)?")]
    DecimalNumber,
    #[regex(r"[+-]?0[bB][01]+")]
    BinaryNumber,
    #[regex(r"[+-]?0[oO][0-7]+")]
    OctalNumber,
    #[regex(r"[+-]?0[xX][0-9a-fA-F]+")]
    HexadecimalNumber,

    #[regex(r#""([^"\n\\]|\\[^\n])*[\\"]?"#)]
    String,

    Error,
}

impl K {
    pub const fn is_binary_operator(self) -> bool {
        matches!(
            self,
            Self::Plus
                | Self::Minus
                | Self::Star
                | Self::Slash
                | Self::Percent
                | Self::Lt
                | Self::EqEq
                | Self::Gt
                | Self::Eq
        )
    }
}

impl From<K> for rowan::SyntaxKind {
    fn from(kind: K) -> Self {
        Self(kind as u16)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Lang {}

impl rowan::Language for Lang {
    type Kind = K;

    fn kind_from_raw(raw: rowan::SyntaxKind) -> Self::Kind {
        assert!(raw.0 <= K::Error as u16);
        // SAFETY: `SyntaxKind` is `repr(u16)` and the assertion ensures that
        // `raw` is within range.
        unsafe { std::mem::transmute(raw.0) }
    }

    fn kind_to_raw(kind: Self::Kind) -> rowan::SyntaxKind {
        kind.into()
    }
}

pub type SyntaxNode = rowan::SyntaxNode<Lang>;

pub type SyntaxToken = rowan::SyntaxToken<Lang>;

struct Token<'src> {
    kind: K,
    text: &'src str,
    span: Span,
}

struct Parser<'src> {
    builder: GreenNodeBuilder<'static>,
    tokens: &'src [Token<'src>],
    span: Span,
    diagnostics: &'src mut Diagnostics,
}

impl Parser<'_> {
    fn skip_trivia(&mut self) {
        while let [token, rest @ ..] = self.tokens {
            if token.kind != K::Trivia {
                break;
            }
            self.tokens = rest;
            self.builder.token(token.kind.into(), token.text);
        }
    }

    fn peek(&self) -> K {
        self.tokens
            .iter()
            .map(|token| token.kind)
            .find(|&it| it != K::Trivia)
            .unwrap_or(K::Eof)
    }

    fn peek_span(&self) -> Span {
        self.tokens
            .iter()
            .find(|token| token.kind != K::Trivia)
            .map_or_else(
                || {
                    let len = self.span.len();
                    self.span.subspan(len, len)
                },
                |token| token.span,
            )
    }

    fn at(&self, kind: K) -> bool {
        self.peek() == kind
    }

    fn immediately_at(&self, kind: K) -> bool {
        self.tokens.first().is_some_and(|token| token.kind == kind)
    }

    fn bump(&mut self) {
        while let [token, rest @ ..] = self.tokens {
            self.tokens = rest;
            self.builder.token(token.kind.into(), token.text);
            if token.kind != K::Trivia {
                break;
            }
        }
    }

    fn eat(&mut self, kind: K) -> bool {
        if self.at(kind) {
            self.bump();
            true
        } else {
            false
        }
    }

    fn start_node(&mut self, kind: K) {
        self.skip_trivia();
        self.builder.start_node(kind.into());
    }

    fn checkpoint(&mut self) -> Checkpoint {
        self.skip_trivia();
        self.builder.checkpoint()
    }

    fn parse_anything(&mut self) {
        match self.peek() {
            K::KwImport => self.parse_import(),
            K::KwStruct => self.parse_struct(),
            K::KwSprite => self.parse_sprite(),
            K::KwInline | K::KwFn => self.parse_function(),
            K::KwCostumes => self.parse_costume_list(),
            K::KwLet => self.parse_let(),
            K::KwIf => self.parse_if(),
            K::KwRepeat => self.parse_repeat(),
            K::KwForever => self.parse_forever(),
            K::KwWhile => self.parse_while(),
            K::KwUntil => self.parse_until(),
            K::KwFor => self.parse_for(),
            K::KwReturn => self.parse_return(),
            K::Lparen => {
                self.bump();
                while !self.at(K::Eof) && !self.eat(K::Rparen) {
                    self.parse_anything();
                }
            }
            K::Lbrace => {
                self.bump();
                while !self.at(K::Eof) && !self.eat(K::Rbrace) {
                    self.parse_anything();
                }
            }
            K::Lbracket => {
                self.bump();
                while !self.at(K::Eof) && !self.eat(K::Rbracket) {
                    self.parse_anything();
                }
            }
            _ => self.bump(),
        }
    }

    fn error(&mut self) {
        self.start_node(K::Error);
        self.parse_anything();
        self.builder.finish_node();
    }

    fn expect(&mut self, kind: K) -> Option<Span> {
        if self.at(kind) {
            let span = self.peek_span();
            self.bump();
            Some(span)
        } else {
            self.error();
            None
        }
    }

    fn parse_import(&mut self) {
        self.start_node(K::Import);
        self.bump(); // K::KwImport
        let _: Option<Span> = self.expect(K::String);
        self.builder.finish_node();
    }

    fn parse_struct(&mut self) {
        self.start_node(K::Struct);
        self.bump(); // K::KwStruct
        if !self.at(K::Lbrace) {
            let _: Option<Span> = self.expect(K::Identifier);
        }
        if self.eat(K::Lbrace) {
            while !self.at(K::Eof) && !self.eat(K::Rbrace) {
                if self.at(K::Identifier) {
                    self.parse_field_definition();
                } else {
                    self.error();
                }
            }
        }
        self.builder.finish_node();
    }

    fn parse_field_definition(&mut self) {
        self.start_node(K::FieldDefinition);
        self.bump(); // K::Identifier
        let _: Option<Span> = self.expect(K::Colon);
        self.parse_expression();
        let _: bool = self.eat(K::Comma);
        self.builder.finish_node();
    }

    fn parse_arguments(&mut self) {
        self.start_node(K::Arguments);
        self.bump(); // K::Lparen
        while !self.at(K::Eof) && !self.eat(K::Rparen) {
            self.parse_expression();
            let _: bool = self.eat(K::Comma);
        }
        self.builder.finish_node();
    }

    fn parse_list_literal(&mut self) {
        self.start_node(K::ListLiteral);
        self.bump(); // K::Lbracket
        while !self.at(K::Eof) && !self.eat(K::Rbracket) {
            self.parse_expression();
            let _: bool = self.eat(K::Comma);
        }
        self.builder.finish_node();
    }

    fn parse_atom(&mut self) {
        match self.peek() {
            K::Identifier => {
                let checkpoint = self.checkpoint();
                self.bump();
                if self.immediately_at(K::Lparen) {
                    self.builder
                        .start_node_at(checkpoint, K::FunctionCall.into());
                    self.parse_arguments();
                } else if self.immediately_at(K::Colon) {
                    self.builder
                        .start_node_at(checkpoint, K::NamedArgument.into());
                    self.bump();
                    self.parse_expression();
                } else {
                    self.builder.start_node_at(checkpoint, K::Variable.into());
                }
                self.builder.finish_node();
            }
            K::Lparen => {
                self.start_node(K::ParenthesizedExpression);
                self.bump();
                self.parse_expression();
                let _: Option<Span> = self.expect(K::Rparen);
                self.builder.finish_node();
            }
            K::DecimalNumber
            | K::BinaryNumber
            | K::OctalNumber
            | K::HexadecimalNumber
            | K::String
            | K::KwFalse
            | K::KwTrue => {
                self.start_node(K::Literal);
                self.bump();
                self.builder.finish_node();
            }
            K::Ampersand => {
                self.start_node(K::Lvalue);
                self.bump();
                self.parse_atom();
                self.builder.finish_node();
            }
            K::Lbracket => self.parse_list_literal(),
            _ => self.error(),
        }
    }

    fn parse_expression(&mut self) {
        self.parse_recursive_expression(K::Eof);
    }

    fn parse_recursive_expression(&mut self, left: K) {
        let checkpoint = self.checkpoint();
        self.parse_atom();
        while self.at(K::Lbracket) {
            self.builder
                .start_node_at(checkpoint, K::GenericTypeInstantiation.into());
            self.parse_type_parameters();
            self.builder.finish_node();
        }

        loop {
            let right = self.peek();
            if binding_power(right) <= binding_power(left) {
                break;
            }
            let node_kind = match right {
                K::KwAs => K::TypeAscription,
                K::Dot => K::MethodCall,
                _ => K::BinaryExpression,
            };
            self.builder.start_node_at(checkpoint, node_kind.into());
            self.bump(); // operator
            self.parse_recursive_expression(right);
            self.builder.finish_node();
        }
    }

    fn parse_type_parameters(&mut self) {
        self.start_node(K::TypeParameters);
        self.bump(); // K::Lbracket
        while !self.at(K::Eof) && !self.eat(K::Rbracket) {
            self.parse_expression();
            let _: bool = self.eat(K::Comma);
        }
        self.builder.finish_node();
    }

    fn parse_function_parameters(&mut self) {
        self.start_node(K::FunctionParameters);
        self.bump(); // K::Lparen
        while !self.at(K::Eof) && !self.eat(K::Rparen) {
            if self.at(K::Comma) {
                let span = self.peek_span();
                self.diagnostics
                    .error("unexpected `,`", [primary(span, "expected parameter")]);
                self.bump();
                continue;
            }
            if self.at(K::Arrow) || self.at(K::Lbrace) {
                let span = self.peek_span();
                self.diagnostics.error(
                    "unterminated parameter list",
                    [primary(span, "expected parameter or `)`")],
                );
                break;
            }
            if !self.at(K::Identifier) {
                self.error();
                continue;
            }

            self.start_node(K::Parameter);
            self.start_node(K::ExternalParameterName);
            self.bump();
            self.builder.finish_node();
            let _: bool = self.eat(K::KwComptime);
            if !self.at(K::Colon) {
                let _: Option<Span> = self.expect(K::Identifier);
            }
            let _: Option<Span> = self.expect(K::Colon);
            if self.at(K::Comma) {
                let span = self.peek_span();
                self.diagnostics
                    .error("unexpected `,`", [primary(span, "expected expression")]);
            } else if self.at(K::Rparen) {
                let span = self.peek_span();
                self.diagnostics.error(
                    "unexpected end of parameter list",
                    [primary(span, "expected expression")],
                );
            } else {
                self.parse_expression();
            }
            let _: bool = self.eat(K::Comma);
            self.builder.finish_node();
        }
        self.builder.finish_node();
    }

    fn parse_let(&mut self) {
        self.start_node(K::Let);
        self.bump(); // K::KwLet
        if !self.at(K::Eq) {
            let _: Option<Span> = self.expect(K::Identifier);
        }
        let _: Option<Span> = self.expect(K::Eq);
        self.parse_expression();
        self.builder.finish_node();
    }

    fn parse_if(&mut self) {
        self.start_node(K::If);
        self.bump(); // K::KwIf
        if self.at(K::Lbrace) {
            let label = primary(self.peek_span(), "");
            self.diagnostics
                .error("expected expression after `if`", [label]);
        } else {
            self.parse_expression();
        }
        self.parse_block();
        if self.eat(K::KwElse) {
            self.start_node(K::ElseClause);
            if self.at(K::KwIf) {
                self.parse_if();
            } else {
                self.parse_block();
            }
            self.builder.finish_node();
        }
        self.builder.finish_node();
    }

    fn parse_repeat(&mut self) {
        self.start_node(K::Repeat);
        self.bump(); // K::KwRepeat
        if self.at(K::Lbrace) {
            let label = primary(self.peek_span(), "");
            self.diagnostics
                .error("expected expression after `repeat`", [label]);
        } else {
            self.parse_expression();
        }
        self.parse_block();
        self.builder.finish_node();
    }

    fn parse_forever(&mut self) {
        self.start_node(K::Forever);
        self.bump(); // K::KwForever
        self.parse_block();
        self.builder.finish_node();
    }

    fn parse_while(&mut self) {
        self.start_node(K::While);
        self.bump(); // K::KwWhile
        if self.at(K::Lbrace) {
            let label = primary(self.peek_span(), "");
            self.diagnostics
                .error("expected expression after `while`", [label]);
        } else {
            self.parse_expression();
        }
        self.parse_block();
        self.builder.finish_node();
    }

    fn parse_until(&mut self) {
        self.start_node(K::Until);
        self.bump(); // K::KwUntil
        if self.at(K::Lbrace) {
            let label = primary(self.peek_span(), "");
            self.diagnostics
                .error("expected expression after `until`", [label]);
        } else {
            self.parse_expression();
        }
        self.parse_block();
        self.builder.finish_node();
    }

    fn parse_for(&mut self) {
        self.start_node(K::For);
        self.bump(); // K::KwFor
        if self.at(K::Lbrace) {
            let label = primary(self.peek_span(), "");
            self.diagnostics
                .error("expected identifier after `for`", [label]);
        } else {
            let _: Option<Span> = self.expect(K::Identifier);
            if self.at(K::Lbrace) {
                let label = primary(self.peek_span(), "");
                self.diagnostics
                    .error("expected expression after variable in `for` loop", [label]);
            } else {
                self.parse_expression();
            }
        }
        self.parse_block();
        self.builder.finish_node();
    }

    fn parse_return(&mut self) {
        self.start_node(K::Return);
        self.bump(); // K::KwReturn
        self.parse_expression();
        self.builder.finish_node();
    }

    fn parse_statement(&mut self) {
        match self.peek() {
            K::KwLet => self.parse_let(),
            K::KwIf => self.parse_if(),
            K::KwRepeat => self.parse_repeat(),
            K::KwForever => self.parse_forever(),
            K::KwWhile => self.parse_while(),
            K::KwUntil => self.parse_until(),
            K::KwFor => self.parse_for(),
            K::KwReturn => self.parse_return(),
            _ => self.parse_expression(),
        }
    }

    fn parse_block(&mut self) {
        if !self.at(K::Lbrace) {
            self.error();
            return;
        }
        self.start_node(K::Block);
        self.bump();
        while !self.at(K::Eof) && !self.eat(K::Rbrace) {
            if self.at(K::KwSprite) {
                break;
            }

            self.parse_statement();
        }
        self.builder.finish_node();
    }

    fn parse_function(&mut self) {
        let checkpoint = self.checkpoint();
        let _: bool = self.eat(K::KwInline);
        if !self.eat(K::KwFn) {
            let span = self.peek_span();
            self.diagnostics
                .error("expected `fn` after `inline`", [primary(span, "")]);
            return;
        }
        self.builder.start_node_at(checkpoint, K::Fn.into());
        let _: Option<Span> = self.expect(K::Identifier);
        let _: bool = self.eat(K::String);
        if self.at(K::Lbracket) {
            self.parse_generics();
        }
        if self.at(K::Lparen) {
            self.parse_function_parameters();
        }
        if self.eat(K::Arrow) {
            self.parse_expression();
        }
        self.parse_block();
        self.builder.finish_node();
    }

    fn parse_generics(&mut self) {
        self.start_node(K::Generics);
        self.bump(); // K::Lbracket
        while !self.at(K::Eof) && !self.eat(K::Rbracket) {
            let _: Option<Span> = self.expect(K::Identifier);
            let _: bool = self.eat(K::Comma);
        }
        self.builder.finish_node();
    }

    fn parse_costume_list(&mut self) {
        self.start_node(K::CostumeList);
        self.bump(); // K::KwCostumes
        let _: Option<Span> = self.expect(K::Lbrace);
        while !self.at(K::Eof) && !self.eat(K::Rbrace) {
            if !self.at(K::String) {
                self.error();
                continue;
            }
            self.start_node(K::Costume);
            self.bump();
            let _: Option<Span> = self.expect(K::Colon);
            let _: Option<Span> = self.expect(K::String);
            let _: bool = self.eat(K::Comma);
            self.builder.finish_node();
        }
        self.builder.finish_node();
    }

    fn parse_sprite(&mut self) {
        self.start_node(K::Sprite);
        let kw_sprite_span = self.peek_span();
        self.bump(); // K::KwSprite
        if !self.at(K::Lbrace) {
            let _: Option<Span> = self.expect(K::Identifier);
        }
        let lbrace_span = self.expect(K::Lbrace);
        while !self.eat(K::Rbrace) {
            match self.peek() {
                K::KwInline | K::KwFn => self.parse_function(),
                K::KwCostumes => self.parse_costume_list(),
                K::KwLet => self.parse_let(),
                K::Eof | K::KwSprite => {
                    let mut labels = vec![primary(kw_sprite_span, "")];
                    if let Some(lbrace_span) = lbrace_span {
                        labels.push(primary(lbrace_span, "unclosed brace"));
                    }
                    labels.push(secondary(self.peek_span(), "expected `}`"));
                    self.diagnostics
                        .error("unfinished sprite definition", labels);
                    break;
                }
                _ => self.error(),
            }
        }
        self.builder.finish_node();
    }

    fn parse_top_level_item(&mut self) {
        match self.peek() {
            K::KwImport => self.parse_import(),
            K::KwStruct => self.parse_struct(),
            K::KwSprite => self.parse_sprite(),
            K::KwInline | K::KwFn => self.parse_function(),
            K::KwLet => self.parse_let(),
            _ => self.error(),
        }
    }

    fn parse(mut self) -> SyntaxNode {
        self.builder.start_node(K::Document.into());
        while !self.at(K::Eof) {
            self.parse_top_level_item();
        }
        self.builder.finish_node();
        SyntaxNode::new_root(self.builder.finish())
    }
}

const PRECEDENCE_TABLE: &[&[K]] = &[
    &[K::Eq],
    &[K::Lt, K::EqEq, K::Gt],
    &[K::Plus, K::Minus],
    &[K::Star, K::Slash, K::Percent],
    &[K::KwAs],
    &[K::Dot],
];

fn binding_power(kind: K) -> Option<usize> {
    PRECEDENCE_TABLE
        .iter()
        .position(|level| level.contains(&kind))
}
