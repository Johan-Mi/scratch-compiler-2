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
    for node in document.descendants() {
        if node.kind() == ERROR {
            let span = span(file, node.text_range());
            diagnostics.error("syntax error", [primary(span, "")]);
        }
    }
}
