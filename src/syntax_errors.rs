use crate::{
    diagnostics::{primary, Diagnostics},
    parser::{SyntaxKind::*, SyntaxNode},
};
use codemap::{File, Span};
use rowan::TextRange;

fn span(file: &File, text_range: TextRange) -> Span {
    let range = std::ops::Range::<u32>::from(text_range);
    file.span.subspan(range.start.into(), range.end.into())
}

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
