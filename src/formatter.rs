use crate::{
    diagnostics::Diagnostics,
    parser::{
        SyntaxKind::{self, *},
        SyntaxNode, SyntaxToken,
    },
};
use codemap::CodeMap;
use rowan::NodeOrToken;

const INDENTATION_SIZE: usize = 4;

pub fn format_stdin_to_stdout(diagnostics: &mut Diagnostics) -> Result<(), ()> {
    let source_code = std::io::read_to_string(std::io::stdin())
        .map_err(|err| diagnostics.error(err.to_string(), []))?;
    print!("{}", format(source_code));
    Ok(())
}

fn format(source_code: String) -> String {
    let capacity = source_code.len();
    let file = CodeMap::new().add_file(String::new(), source_code);
    let cst = crate::parser::parse(&file, &mut Diagnostics::default());
    let mut formatter = Formatter {
        output: String::with_capacity(capacity),
        indentation: 0,
    };
    formatter.node(&cst);
    formatter.finish()
}

type SyntaxElement = rowan::SyntaxElement<crate::parser::Lang>;

struct Formatter {
    output: String,
    indentation: usize,
}

impl Formatter {
    fn finish(mut self) -> String {
        if self.output.ends_with("\n\n") {
            self.output.pop();
        }
        self.output
    }

    fn node(&mut self, node: &SyntaxNode) {
        for child in node.children_with_tokens() {
            self.element(&child);
        }
    }

    fn token(&mut self, token: &SyntaxToken) {
        // FIXME: indentation should be handled by the enclosing node.
        if matches!(token.kind(), RPAREN | RBRACE) {
            self.indentation =
                self.indentation.saturating_sub(INDENTATION_SIZE);
        }

        if token.kind() == TRIVIA {
            self.trivia(token.text());
        } else {
            if self.output.ends_with('\n') {
                self.output
                    .extend(std::iter::repeat(' ').take(self.indentation));
            } else if token_wants_leading_space(
                token.kind(),
                self.output.as_bytes().last().copied(),
            ) {
                self.leading_space();
            }
            self.output.push_str(token.text());
        }

        // FIXME: indentation should be handled by the enclosing node.
        if matches!(token.kind(), LPAREN | LBRACE) {
            self.indentation =
                self.indentation.saturating_add(INDENTATION_SIZE);
        }
    }

    fn trivia(&mut self, mut text: &str) {
        while !text.is_empty() {
            if let Some(t) = text.strip_prefix('\n') {
                text = t;
                self.newline();
            } else if text.starts_with('#') {
                let end = text.find('\n').unwrap_or(text.len());
                let (comment, after) = text.split_at(end);
                self.leading_space();
                self.output.push_str("# ");
                self.output.push_str(comment[1..].trim());
                text = after;
            } else {
                text = text.trim_start();
            }
        }
    }

    fn newline(&mut self) {
        if !self.output.is_empty() && !self.output.ends_with("\n\n") {
            self.output.push('\n');
        }
    }

    fn leading_space(&mut self) {
        if self
            .output
            .ends_with(|c: char| !c.is_whitespace() && c != '(')
        {
            self.output.push(' ');
        }
    }

    fn element(&mut self, element: &SyntaxElement) {
        match element {
            NodeOrToken::Node(node) => self.node(node),
            NodeOrToken::Token(token) => self.token(token),
        }
    }
}

fn token_wants_leading_space(kind: SyntaxKind, last: Option<u8>) -> bool {
    !matches!(kind, RPAREN | COLON | COMMA)
        && (kind, last) != (RBRACE, Some(b'{'))
}
