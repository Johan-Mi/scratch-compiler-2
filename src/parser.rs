use crate::diagnostics::{primary, secondary, Diagnostics};
use codemap::Span;
use logos::Logos;
use rowan::GreenNodeBuilder;
use std::iter::Peekable;

pub fn parse(
    file: &codemap::File,
    diagnostics: &mut Diagnostics,
) -> SyntaxNode {
    let source_code = file.source();
    Parser {
        builder: GreenNodeBuilder::new(),
        iter: SyntaxKind::lexer(source_code)
            .spanned()
            .map(|(token, span)| Token {
                kind: token.unwrap_or(ERROR),
                text: &source_code[span.clone()],
                span: file.span.subspan(span.start as u64, span.end as u64),
            })
            .peekable(),
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
    FN,
    FUNCTION_PARAMETERS,
    PARAMETER,
    EXTERNAL_PARAMETER_NAME,
    BLOCK,
    VARIABLE,
    FUNCTION_CALL,
    ARGUMENTS,
    NAMED_ARGUMENT,
    LET,
    PARENTHESIZED_EXPRESSION,
    BINARY_EXPRESSION,
    LITERAL,

    #[token("(")]
    LPAREN,
    #[token(")")]
    RPAREN,
    #[token("{")]
    LBRACE,
    #[token("}")]
    RBRACE,
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

    #[token("sprite")]
    KW_SPRITE,
    #[token("fn")]
    KW_FN,
    #[token("let")]
    KW_LET,

    #[regex(r"[\p{XID_Start}_][\p{XID_Continue}-]*")]
    IDENTIFIER,

    // TODO: binary, hex, octal and decimals
    #[regex(r"[+-]?[0-9]+")]
    NUMBER,

    ERROR,
}

use SyntaxKind::*;

impl SyntaxKind {
    pub const fn is_binary_operator(self) -> bool {
        matches!(self, PLUS | MINUS | STAR | SLASH | PERCENT)
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

struct Parser<'src, I: Iterator<Item = Token<'src>>> {
    builder: GreenNodeBuilder<'static>,
    iter: Peekable<I>,
    span: Span,
    diagnostics: &'src mut Diagnostics,
}

impl<'src, I: Iterator<Item = Token<'src>>> Parser<'src, I> {
    fn skip_trivia(&mut self) {
        if self.iter.peek().is_some_and(|token| token.kind == TRIVIA) {
            self.bump();
        }
    }

    fn peek(&mut self) -> SyntaxKind {
        self.skip_trivia();
        self.iter.peek().map_or(EOF, |token| token.kind)
    }

    fn peek_span(&mut self) -> Span {
        self.skip_trivia();
        self.iter.peek().map_or_else(
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
        self.iter.peek().is_some_and(|token| token.kind == kind)
    }

    fn bump(&mut self) {
        if let Some(token) = self.iter.next() {
            self.builder.token(token.kind.into(), token.text);
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
            KW_FN => self.parse_function(),
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
            NUMBER => {
                self.builder.start_node(LITERAL.into());
                self.bump();
                self.builder.finish_node();
            }
            _ => self.error(),
        }
    }

    fn parse_expression(&mut self) {
        self.parse_recursive_expression(EOF);
    }

    fn parse_recursive_expression(&mut self, left: SyntaxKind) {
        let checkpoint = self.builder.checkpoint();
        self.parse_atom();

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

    fn parse_function_parameters(&mut self) {
        self.builder.start_node(FUNCTION_PARAMETERS.into());
        self.bump(); // LPAREN
        while !self.at(EOF) && !self.eat(RPAREN) {
            if !self.at(IDENTIFIER) {
                self.error();
                continue;
            }

            self.builder.start_node(PARAMETER.into());
            self.builder.start_node(EXTERNAL_PARAMETER_NAME.into());
            self.bump();
            self.builder.finish_node();
            if !self.at(COLON) {
                self.expect(IDENTIFIER);
            }
            self.expect(COLON);
            self.parse_expression();
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

    fn parse_statement(&mut self) {
        match self.peek() {
            KW_LET => self.parse_let(),
            _ => self.parse_expression(),
        }
    }

    fn parse_block(&mut self) {
        self.builder.start_node(BLOCK.into());
        self.expect(LBRACE);
        while !self.at(EOF) && !self.eat(RBRACE) {
            if self.at(KW_SPRITE) {
                break;
            }

            self.parse_statement();
        }
        self.builder.finish_node();
    }

    fn parse_function(&mut self) {
        self.builder.start_node(FN.into());
        self.bump(); // KW_FN
        self.expect(IDENTIFIER);
        if self.at(LPAREN) {
            self.parse_function_parameters();
        }
        if self.eat(ARROW) {
            self.parse_expression();
        }
        self.parse_block();
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
                KW_FN => self.parse_function(),
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
            KW_FN => self.parse_function(),
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

const PRECEDENCE_TABLE: &[&[SyntaxKind]] =
    &[&[PLUS, MINUS], &[STAR, SLASH, PERCENT]];

fn binding_power(kind: SyntaxKind) -> Option<usize> {
    PRECEDENCE_TABLE
        .iter()
        .position(|level| level.contains(&kind))
}
