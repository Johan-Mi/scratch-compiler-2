use crate::{
    diagnostics::Diagnostics,
    parser::{SyntaxNode, SyntaxToken},
};
use codemap::CodeMap;
use rowan::NodeOrToken;

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
    };
    formatter.node(&cst);
    formatter.output
}

type SyntaxElement = rowan::SyntaxElement<crate::parser::Lang>;

struct Formatter {
    output: String,
}

impl Formatter {
    fn node(&mut self, node: &SyntaxNode) {
        for child in node.children_with_tokens() {
            self.element(&child);
        }
    }

    fn token(&mut self, token: &SyntaxToken) {
        self.output.push_str(token.text());
    }

    fn element(&mut self, element: &SyntaxElement) {
        match element {
            NodeOrToken::Node(node) => self.node(node),
            NodeOrToken::Token(token) => self.token(token),
        }
    }
}
