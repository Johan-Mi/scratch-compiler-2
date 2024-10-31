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
    let source_code =
        std::io::read_to_string(std::io::stdin()).map_err(|err| {
            diagnostics.error("failed to read source code", []);
            diagnostics.note(err.to_string(), []);
        })?;
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
        newlines: 2,
        comment: false,
    };
    formatter.node(&cst);
    formatter.finish()
}

struct Formatter {
    output: String,
    indentation: usize,
    newlines: u8,
    comment: bool,
}

impl Formatter {
    fn finish(mut self) -> String {
        match self.newlines {
            0 => self.output.push('\n'),
            2 => _ = self.output.pop(),
            _ => {}
        }
        self.output
    }

    fn node(&mut self, node: &SyntaxNode) {
        let mut indented = false;
        for child in node.children_with_tokens() {
            match child {
                NodeOrToken::Node(node) => self.node(&node),
                NodeOrToken::Token(token) => {
                    if indented
                        && matches!(token.kind(), RPAREN | RBRACE | RBRACKET)
                    {
                        self.indentation =
                            self.indentation.saturating_sub(INDENTATION_SIZE);
                        indented = false;
                    } else if node.kind() == METHOD_CALL && token.kind() == DOT
                    {
                        self.indentation += INDENTATION_SIZE;
                        indented = true;
                    }
                    self.token(
                        &token,
                        matches!(node.kind(), ARGUMENTS | FUNCTION_PARAMETERS)
                            && token.kind() == LPAREN
                            || matches!(
                                node.kind(),
                                GENERICS | TYPE_PARAMETERS
                            ) && token.kind() == LBRACKET,
                    );
                    if matches!(token.kind(), LPAREN | LBRACE | LBRACKET) {
                        self.indentation += INDENTATION_SIZE;
                        indented = true;
                    }
                }
            };
        }
        if indented {
            self.indentation =
                self.indentation.saturating_sub(INDENTATION_SIZE);
        }
    }

    fn token(&mut self, token: &SyntaxToken, immediately: bool) {
        if token.kind() == TRIVIA {
            self.trivia(token.text());
        } else {
            if matches!(token.kind(), COMMA | COLON) {
                self.remove_newlines();
            }

            if matches!(token.kind(), RPAREN | RBRACKET)
                && self.output.ends_with(',')
            {
                let _: Option<char> = self.output.pop();
            }

            self.indent();
            self.newlines = 0;
            self.comment = false;
            if !immediately
                && token_wants_leading_space(
                    token.kind(),
                    self.output.as_bytes().last().copied(),
                )
            {
                self.leading_space();
            }
            self.output.push_str(token.text());
        }
    }

    fn remove_newlines(&mut self) {
        let to_remove = usize::from(self.newlines - u8::from(self.comment));
        self.output.truncate(self.output.len() - to_remove);
        self.newlines = u8::from(self.comment);
    }

    fn indent(&mut self) {
        if self.newlines != 0 {
            self.output
                .extend(std::iter::repeat_n(' ', self.indentation));
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
                self.indent();
                self.leading_space();
                let comment = comment[1..].trim_end();
                self.output.push('#');
                if comment.starts_with(|c: char| !c.is_whitespace()) {
                    self.output.push(' ');
                }
                self.output.push_str(comment);
                text = after;
                self.newlines = 0;
                self.comment = true;
            } else {
                text = text.trim_start_matches(|c: char| {
                    c.is_whitespace() && c != '\n'
                });
            }
        }
    }

    fn newline(&mut self) {
        if self.newlines < 2 {
            self.output.push('\n');
            self.newlines += 1;
        }
    }

    fn leading_space(&mut self) {
        if self.output.ends_with(|c: char| {
            !c.is_whitespace() && !matches!(c, '(' | '[' | '&')
        }) {
            self.output.push(' ');
        }
    }
}

fn token_wants_leading_space(kind: SyntaxKind, last: Option<u8>) -> bool {
    !matches!(kind, RPAREN | RBRACKET | COLON | COMMA | DOT)
        && (kind, last) != (RBRACE, Some(b'{'))
        && (kind, last) != (IDENTIFIER, Some(b'.'))
}
