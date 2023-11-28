#![deny(unsafe_code)]
#![warn(clippy::nursery, clippy::pedantic)]

#[allow(unsafe_code)]
mod parser;

fn main() {
    let source_file = std::env::args().nth(1).expect("no source file provided");
    let source_code = std::fs::read_to_string(source_file)
        .expect("failed to read source code");
    let document = parser::parse(&source_code);
    eprintln!("{document:#?}");
}
