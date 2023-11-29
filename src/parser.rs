#![allow(clippy::enum_glob_use)]

use logos::Logos;
use rowan::GreenNodeBuilder;
use std::iter::Peekable;

pub fn parse(file: &codemap::File) -> SyntaxNode {
    let source_code = file.source();
    Parser {
        builder: GreenNodeBuilder::new(),
        iter: SyntaxKind::lexer(source_code)
            .spanned()
            .map(|(token, span)| (token.unwrap_or(ERROR), &source_code[span]))
            .peekable(),
    }
    .parse()
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Logos)]
#[allow(non_camel_case_types, clippy::upper_case_acronyms)]
#[repr(u16)]
#[logos(skip r"\p{Whitespace}")]
pub enum SyntaxKind {
    EOF = 0,

    DOCUMENT,
    SPRITE,

    #[token("{")]
    LBRACE,
    #[token("}")]
    RBRACE,

    #[token("sprite")]
    KW_SPRITE,

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

type SyntaxNode = rowan::SyntaxNode<Lang>;

type Token<'src> = (SyntaxKind, &'src str);

struct Parser<'src, I: Iterator<Item = Token<'src>>> {
    builder: GreenNodeBuilder<'static>,
    iter: Peekable<I>,
}

impl<'src, I: Iterator<Item = Token<'src>>> Parser<'src, I> {
    fn peek(&mut self) -> SyntaxKind {
        self.iter.peek().map_or(EOF, |(t, _)| *t)
    }

    fn at(&mut self, kind: SyntaxKind) -> bool {
        self.peek() == kind
    }

    fn bump(&mut self) {
        if let Some((token, text)) = self.iter.next() {
            self.builder.token(token.into(), text);
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

    fn expect(&mut self, kind: SyntaxKind) -> bool {
        if self.at(kind) {
            self.bump();
            true
        } else {
            self.error();
            false
        }
    }

    fn parse_sprite(&mut self) {
        self.builder.start_node(SPRITE.into());
        self.bump(); // KW_SPRITE
        let _ = self.expect(IDENTIFIER)
            && self.expect(LBRACE)
            && self.expect(RBRACE);
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
