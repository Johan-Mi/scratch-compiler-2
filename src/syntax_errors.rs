use crate::{
    diagnostics::{primary, span, Diagnostics},
    parser::{SyntaxNode, K},
};
use codemap::File;

pub fn check(document: &SyntaxNode, file: &File, diagnostics: &mut Diagnostics) {
    for thing in document.descendants_with_tokens() {
        match thing {
            rowan::NodeOrToken::Node(node) => {
                if node.kind() == K::Error {
                    let span = span(file, node.text_range());
                    let message = if node.first_token().is_some_and(|it| it.kind() == K::Error) {
                        "invalid token"
                    } else {
                        "syntax error"
                    };
                    diagnostics.error(message, [primary(span, "")]);
                }
            }
            rowan::NodeOrToken::Token(token) => {
                if token.kind() == K::Identifier {
                    if token.text().ends_with('-') {
                        let span = span(file, token.text_range());
                        diagnostics.warning("suspicious identifier", [primary(span, "")]);
                        diagnostics
                            .note("to avoid confusion, don't end an identifier with `-`", []);
                    }
                } else if token.kind() == K::String && !token.text().ends_with('"') {
                    let span = span(file, token.text_range());
                    diagnostics.error("unterminated string literal", [primary(span, "")]);
                }
            }
        }
    }
}
