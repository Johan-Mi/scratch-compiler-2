use crate::diagnostics::{primary, secondary, Diagnostics};
use codemap::Span;
use logos::Logos;
use rowan::GreenNodeBuilder;

pub fn parse(
    file: &codemap::File,
    diagnostics: &mut Diagnostics,
) -> SyntaxNode {
    let source_code = file.source();
    let tokens = &SyntaxKind::lexer(source_code)
        .spanned()
        .map(|(token, span)| Token {
            kind: token.unwrap_or(ERROR),
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Logos)]
#[allow(non_camel_case_types, clippy::upper_case_acronyms)]
#[repr(u16)]
pub enum SyntaxKind {
    EOF = 0,
    #[regex(r"(\p{Whitespace}|#.*)+")]
    TRIVIA,

    DOCUMENT,
    SPRITE,
    COSTUME_LIST,
    COSTUME,
    FN,
    GENERICS,
    FUNCTION_PARAMETERS,
    PARAMETER,
    EXTERNAL_PARAMETER_NAME,
    BLOCK,
    VARIABLE,
    FUNCTION_CALL,
    ARGUMENTS,
    NAMED_ARGUMENT,
    LET,
    IF,
    ELSE_CLAUSE,
    REPEAT,
    FOREVER,
    WHILE,
    UNTIL,
    FOR,
    PARENTHESIZED_EXPRESSION,
    BINARY_EXPRESSION,
    LITERAL,
    LVALUE,
    GENERIC_TYPE_INSTANTIATION,
    TYPE_PARAMETERS,
    LIST_LITERAL,

    #[token("(")]
    LPAREN,
    #[token(")")]
    RPAREN,
    #[token("{")]
    LBRACE,
    #[token("}")]
    RBRACE,
    #[token("[")]
    LBRACKET,
    #[token("]")]
    RBRACKET,
    #[token("->")]
    ARROW,
    #[token(":")]
    COLON,
    #[token(",")]
    COMMA,
    #[token("=")]
    EQ,
    #[token("+")]
    PLUS,
    #[token("-")]
    MINUS,
    #[token("*")]
    STAR,
    #[token("/")]
    SLASH,
    #[token("%")]
    PERCENT,
    #[token("<")]
    LT,
    #[token("==")]
    EQ_EQ,
    #[token(">")]
    GT,
    #[token("&")]
    AMPERSAND,

    #[token("sprite")]
    KW_SPRITE,
    #[token("inline")]
    KW_INLINE,
    #[token("fn")]
    KW_FN,
    #[token("let")]
    KW_LET,
    #[token("costumes")]
    KW_COSTUMES,
    #[token("false")]
    KW_FALSE,
    #[token("true")]
    KW_TRUE,
    #[token("if")]
    KW_IF,
    #[token("else")]
    KW_ELSE,
    #[token("repeat")]
    KW_REPEAT,
    #[token("forever")]
    KW_FOREVER,
    #[token("while")]
    KW_WHILE,
    #[token("until")]
    KW_UNTIL,
    #[token("for")]
    KW_FOR,
    #[token("comptime")]
    KW_COMPTIME,

    #[regex(r"[\p{XID_Start}_][\p{XID_Continue}-]*")]
    IDENTIFIER,

    // TODO: binary, hex, octal and decimals
    #[regex(r"[+-]?[0-9]+")]
    NUMBER,

    // TODO: escape sequences
    #[regex(r#""[^"\n]*"?"#)]
    STRING,

    ERROR,
}

use SyntaxKind::*;

impl SyntaxKind {
    pub const fn is_binary_operator(self) -> bool {
        matches!(
            self,
            PLUS | MINUS | STAR | SLASH | PERCENT | LT | EQ_EQ | GT | EQ
        )
    }
}

impl From<SyntaxKind> for rowan::SyntaxKind {
    fn from(kind: SyntaxKind) -> Self {
        Self(kind as u16)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Lang {}

impl rowan::Language for Lang {
    type Kind = SyntaxKind;

    fn kind_from_raw(raw: rowan::SyntaxKind) -> Self::Kind {
        assert!(raw.0 <= ERROR as u16);
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
    kind: SyntaxKind,
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
            if token.kind != TRIVIA {
                break;
            }
            self.tokens = rest;
            self.builder.token(token.kind.into(), token.text);
        }
    }

    fn peek(&mut self) -> SyntaxKind {
        self.tokens
            .iter()
            .map(|token| token.kind)
            .find(|&it| it != TRIVIA)
            .unwrap_or(EOF)
    }

    fn peek_span(&mut self) -> Span {
        self.tokens
            .iter()
            .find(|token| token.kind != TRIVIA)
            .map_or_else(
                || {
                    let len = self.span.len();
                    self.span.subspan(len, len)
                },
                |token| token.span,
            )
    }

    fn at(&mut self, kind: SyntaxKind) -> bool {
        self.peek() == kind
    }

    fn immediately_at(&mut self, kind: SyntaxKind) -> bool {
        self.tokens.first().is_some_and(|token| token.kind == kind)
    }

    fn bump(&mut self) {
        while let [token, rest @ ..] = self.tokens {
            self.tokens = rest;
            self.builder.token(token.kind.into(), token.text);
            if token.kind != TRIVIA {
                break;
            }
        }
    }

    fn eat(&mut self, kind: SyntaxKind) -> bool {
        if self.at(kind) {
            self.bump();
            true
        } else {
            false
        }
    }

    fn parse_anything(&mut self) {
        match self.peek() {
            KW_SPRITE => self.parse_sprite(),
            KW_INLINE | KW_FN => self.parse_function(),
            KW_COSTUMES => self.parse_costume_list(),
            KW_LET => self.parse_let(),
            KW_IF => self.parse_if(),
            KW_REPEAT => self.parse_repeat(),
            KW_FOREVER => self.parse_forever(),
            KW_WHILE => self.parse_while(),
            KW_UNTIL => self.parse_until(),
            KW_FOR => self.parse_for(),
            LPAREN => {
                self.bump();
                while !self.at(EOF) && !self.eat(RPAREN) {
                    self.parse_anything();
                }
            }
            LBRACE => {
                self.bump();
                while !self.at(EOF) && !self.eat(RBRACE) {
                    self.parse_anything();
                }
            }
            LBRACKET => {
                self.bump();
                while !self.at(EOF) && !self.eat(RBRACKET) {
                    self.parse_anything();
                }
            }
            _ => self.bump(),
        }
    }

    fn error(&mut self) {
        self.builder.start_node(ERROR.into());
        self.parse_anything();
        self.builder.finish_node();
    }

    fn expect(&mut self, kind: SyntaxKind) -> Option<Span> {
        if self.at(kind) {
            let span = self.peek_span();
            self.bump();
            Some(span)
        } else {
            self.error();
            None
        }
    }

    fn parse_arguments(&mut self) {
        self.builder.start_node(ARGUMENTS.into());
        self.bump(); // LPAREN
        while !self.at(EOF) && !self.eat(RPAREN) {
            self.parse_expression();
            self.eat(COMMA);
        }
        self.builder.finish_node();
    }

    fn parse_list_literal(&mut self) {
        self.builder.start_node(LIST_LITERAL.into());
        self.bump(); // LBRACKET
        while !self.at(EOF) && !self.eat(RBRACKET) {
            self.parse_expression();
            self.eat(COMMA);
        }
        self.builder.finish_node();
    }

    fn parse_atom(&mut self) {
        match self.peek() {
            IDENTIFIER => {
                let checkpoint = self.builder.checkpoint();
                self.bump();
                if self.immediately_at(LPAREN) {
                    self.builder
                        .start_node_at(checkpoint, FUNCTION_CALL.into());
                    self.parse_arguments();
                } else if self.immediately_at(COLON) {
                    self.builder
                        .start_node_at(checkpoint, NAMED_ARGUMENT.into());
                    self.bump();
                    self.parse_expression();
                } else {
                    self.builder.start_node_at(checkpoint, VARIABLE.into());
                }
                self.builder.finish_node();
            }
            LPAREN => {
                self.builder.start_node(PARENTHESIZED_EXPRESSION.into());
                self.bump();
                self.parse_expression();
                self.expect(RPAREN);
                self.builder.finish_node();
            }
            NUMBER | STRING | KW_FALSE | KW_TRUE => {
                self.skip_trivia();
                self.builder.start_node(LITERAL.into());
                self.bump();
                self.builder.finish_node();
            }
            AMPERSAND => {
                self.builder.start_node(LVALUE.into());
                self.bump();
                self.parse_atom();
                self.builder.finish_node();
            }
            LBRACKET => self.parse_list_literal(),
            _ => self.error(),
        }
    }

    fn parse_expression(&mut self) {
        self.parse_recursive_expression(EOF);
    }

    fn parse_recursive_expression(&mut self, left: SyntaxKind) {
        let checkpoint = self.builder.checkpoint();
        self.parse_atom();
        while self.at(LBRACKET) {
            self.builder
                .start_node_at(checkpoint, GENERIC_TYPE_INSTANTIATION.into());
            self.parse_type_parameters();
            self.builder.finish_node();
        }

        loop {
            let right = self.peek();
            if binding_power(right) <= binding_power(left) {
                break;
            }
            self.builder
                .start_node_at(checkpoint, BINARY_EXPRESSION.into());
            self.bump(); // operator
            self.parse_recursive_expression(right);
            self.builder.finish_node();
        }
    }

    fn parse_type_parameters(&mut self) {
        self.builder.start_node(TYPE_PARAMETERS.into());
        self.bump(); // LBRACKET
        while !self.at(EOF) && !self.eat(RBRACKET) {
            self.parse_expression();
            self.eat(COMMA);
        }
        self.builder.finish_node();
    }

    fn parse_function_parameters(&mut self) {
        self.builder.start_node(FUNCTION_PARAMETERS.into());
        self.bump(); // LPAREN
        while !self.at(EOF) && !self.eat(RPAREN) {
            if self.at(COMMA) {
                let span = self.peek_span();
                self.diagnostics.error(
                    "unexpected `,`",
                    [primary(span, "expected parameter")],
                );
                self.bump();
                continue;
            }
            if self.at(ARROW) || self.at(LBRACE) {
                let span = self.peek_span();
                self.diagnostics.error(
                    "unterminated parameter list",
                    [primary(span, "expected parameter or `)`")],
                );
                break;
            }
            if !self.at(IDENTIFIER) {
                self.error();
                continue;
            }

            self.builder.start_node(PARAMETER.into());
            self.builder.start_node(EXTERNAL_PARAMETER_NAME.into());
            self.bump();
            self.builder.finish_node();
            self.eat(KW_COMPTIME);
            if !self.at(COLON) {
                self.expect(IDENTIFIER);
            }
            self.expect(COLON);
            if self.at(COMMA) {
                let span = self.peek_span();
                self.diagnostics.error(
                    "unexpected `,`",
                    [primary(span, "expected expression")],
                );
            } else if self.at(RPAREN) {
                let span = self.peek_span();
                self.diagnostics.error(
                    "unexpected end of parameter list",
                    [primary(span, "expected expression")],
                );
            } else {
                self.parse_expression();
            }
            self.eat(COMMA);
            self.builder.finish_node();
        }
        self.builder.finish_node();
    }

    fn parse_let(&mut self) {
        self.builder.start_node(LET.into());
        self.bump(); // KW_LET
        if !self.at(EQ) {
            self.expect(IDENTIFIER);
        }
        self.expect(EQ);
        self.parse_expression();
        self.builder.finish_node();
    }

    fn parse_if(&mut self) {
        self.builder.start_node(IF.into());
        self.bump(); // KW_IF
        if self.at(LBRACE) {
            let label = primary(self.peek_span(), "");
            self.diagnostics
                .error("expected expression after `if`", [label]);
        } else {
            self.parse_expression();
        }
        self.parse_block();
        if self.eat(KW_ELSE) {
            self.builder.start_node(ELSE_CLAUSE.into());
            if self.at(KW_IF) {
                self.parse_if();
            } else {
                self.parse_block();
            }
            self.builder.finish_node();
        }
        self.builder.finish_node();
    }

    fn parse_repeat(&mut self) {
        self.builder.start_node(REPEAT.into());
        self.bump(); // KW_REPEAT
        if self.at(LBRACE) {
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
        self.builder.start_node(FOREVER.into());
        self.bump(); // KW_FOREVER
        self.parse_block();
        self.builder.finish_node();
    }

    fn parse_while(&mut self) {
        self.builder.start_node(WHILE.into());
        self.bump(); // KW_WHILE
        if self.at(LBRACE) {
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
        self.builder.start_node(UNTIL.into());
        self.bump(); // KW_UNTIL
        if self.at(LBRACE) {
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
        self.builder.start_node(FOR.into());
        self.bump(); // KW_FOR
        if self.at(LBRACE) {
            let label = primary(self.peek_span(), "");
            self.diagnostics
                .error("expected identifier after `for`", [label]);
        } else {
            self.expect(IDENTIFIER);
            if self.at(LBRACE) {
                let label = primary(self.peek_span(), "");
                self.diagnostics.error(
                    "expected expression after variable in `for` loop",
                    [label],
                );
            } else {
                self.parse_expression();
            }
        }
        self.parse_block();
        self.builder.finish_node();
    }

    fn parse_statement(&mut self) {
        match self.peek() {
            KW_LET => self.parse_let(),
            KW_IF => self.parse_if(),
            KW_REPEAT => self.parse_repeat(),
            KW_FOREVER => self.parse_forever(),
            KW_WHILE => self.parse_while(),
            KW_UNTIL => self.parse_until(),
            KW_FOR => self.parse_for(),
            _ => self.parse_expression(),
        }
    }

    fn parse_block(&mut self) {
        if !self.at(LBRACE) {
            self.error();
            return;
        }
        self.builder.start_node(BLOCK.into());
        self.bump();
        while !self.at(EOF) && !self.eat(RBRACE) {
            if self.at(KW_SPRITE) {
                break;
            }

            self.parse_statement();
        }
        self.builder.finish_node();
    }

    fn parse_function(&mut self) {
        let checkpoint = self.builder.checkpoint();
        self.eat(KW_INLINE);
        if !self.eat(KW_FN) {
            let span = self.peek_span();
            self.diagnostics
                .error("expected `fn` after `inline`", [primary(span, "")]);
            return;
        }
        self.builder.start_node_at(checkpoint, FN.into());
        self.expect(IDENTIFIER);
        if self.at(LBRACKET) {
            self.parse_generics();
        }
        if self.at(LPAREN) {
            self.parse_function_parameters();
        }
        if self.eat(ARROW) {
            self.parse_expression();
        }
        self.parse_block();
        self.builder.finish_node();
    }

    fn parse_generics(&mut self) {
        self.builder.start_node(GENERICS.into());
        self.bump(); // LBRACKET
        while !self.at(EOF) && !self.eat(RBRACKET) {
            self.expect(IDENTIFIER);
            self.eat(COMMA);
        }
        self.builder.finish_node();
    }

    fn parse_costume_list(&mut self) {
        self.builder.start_node(COSTUME_LIST.into());
        self.bump(); // KW_COSTUMES
        self.expect(LBRACE);
        while !self.at(EOF) && !self.eat(RBRACE) {
            if !self.at(STRING) {
                self.error();
                continue;
            }
            self.builder.start_node(COSTUME.into());
            self.bump();
            self.expect(COLON);
            self.expect(STRING);
            self.eat(COMMA);
            self.builder.finish_node();
        }
        self.builder.finish_node();
    }

    fn parse_sprite(&mut self) {
        self.builder.start_node(SPRITE.into());
        let kw_sprite_span = self.peek_span();
        self.bump(); // KW_SPRITE
        if !self.at(LBRACE) {
            self.expect(IDENTIFIER);
        }
        let lbrace_span = self.expect(LBRACE);
        while !self.eat(RBRACE) {
            match self.peek() {
                KW_INLINE | KW_FN => self.parse_function(),
                KW_COSTUMES => self.parse_costume_list(),
                KW_LET => self.parse_let(),
                EOF | KW_SPRITE => {
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
            KW_SPRITE => self.parse_sprite(),
            KW_INLINE | KW_FN => self.parse_function(),
            KW_LET => self.parse_let(),
            _ => self.error(),
        }
    }

    fn parse(mut self) -> SyntaxNode {
        self.builder.start_node(DOCUMENT.into());
        while !self.at(EOF) {
            self.parse_top_level_item();
        }
        self.builder.finish_node();
        SyntaxNode::new_root(self.builder.finish())
    }
}

const PRECEDENCE_TABLE: &[&[SyntaxKind]] = &[
    &[EQ],
    &[LT, EQ_EQ, GT],
    &[PLUS, MINUS],
    &[STAR, SLASH, PERCENT],
];

fn binding_power(kind: SyntaxKind) -> Option<usize> {
    PRECEDENCE_TABLE
        .iter()
        .position(|level| level.contains(&kind))
}
