#![deny(unsafe_code)]
#![warn(clippy::nursery, clippy::pedantic)]

mod diagnostics;
#[allow(unsafe_code)]
mod parser;

use diagnostics::Diagnostics;

fn main() {
    let mut diagnostics = Diagnostics::default();
    let _ = real_main(&mut diagnostics);
    diagnostics.show();
}

fn real_main(diagnostics: &mut Diagnostics) -> Result<(), ()> {
    let source_file = std::env::args()
        .nth(1)
        .ok_or_else(|| diagnostics.error("no source file provided", []))?;
    let source_code = std::fs::read_to_string(source_file).map_err(|err| {
        diagnostics.error("failed to read source code", []);
        diagnostics.note(err.to_string(), []);
    })?;
    let document = parser::parse(&source_code);
    eprintln!("{document:#?}");

    Ok(())
}
