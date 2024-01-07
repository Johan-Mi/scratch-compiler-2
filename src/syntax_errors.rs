use crate::{
    diagnostics::{primary, span, Diagnostics},
    parser::{SyntaxKind::*, SyntaxNode},
};
use codemap::File;

pub fn check(
    document: &SyntaxNode,
    file: &File,
    diagnostics: &mut Diagnostics,
) {
    for thing in document.descendants_with_tokens() {
        match thing {
            rowan::NodeOrToken::Node(node) => {
                if node.kind() == ERROR {
                    let span = span(file, node.text_range());
                    diagnostics.error("syntax error", [primary(span, "")]);
                }
            }
            rowan::NodeOrToken::Token(token) => {
                if token.kind() == STRING && !token.text().ends_with('"') {
                    let span = span(file, token.text_range());
                    diagnostics.error(
                        "unterminated string literal",
                        [primary(span, "")],
                    );
                }
            }
        }
    }
}
