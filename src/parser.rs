#![allow(clippy::enum_glob_use)]

use crate::diagnostics::{primary, Diagnostics};
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

    #[token("{")]
    LBRACE,
    #[token("}")]
    RBRACE,

    #[token("sprite")]
    KW_SPRITE,
    #[token("fn")]
    KW_FN,

    #[regex(r"\p{XID_Start}[\p{XID_Continue}-]*")]
    IDENTIFIER,

    ERROR,
}

use SyntaxKind::*;

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
        while self.iter.peek().is_some_and(|token| token.kind == TRIVIA) {
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

    fn parse_function(&mut self) {
        self.builder.start_node(FN.into());
        self.bump(); // KW_FN
        self.expect(IDENTIFIER);
        self.expect(LBRACE);
        while !self.at(EOF) && !self.eat(RBRACE) {
            if self.at(KW_SPRITE) {
                break;
            }

            self.error();
        }
        self.builder.finish_node();
    }

    fn parse_sprite(&mut self) {
        self.builder.start_node(SPRITE.into());
        let kw_sprite_span = self.peek_span();
        self.bump(); // KW_SPRITE
        self.expect(IDENTIFIER);
        let lbrace_span = self.expect(LBRACE);
        while !self.eat(RBRACE) {
            match self.peek() {
                KW_FN => self.parse_function(),
                EOF | KW_SPRITE => {
                    let mut labels = vec![primary(kw_sprite_span, "")];
                    if let Some(lbrace_span) = lbrace_span {
                        labels.push(primary(lbrace_span, "unclosed brace"));
                    }
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
