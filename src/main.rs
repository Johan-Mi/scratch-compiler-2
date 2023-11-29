#![deny(unsafe_code)]
#![warn(clippy::nursery, clippy::pedantic)]

mod diagnostics;
#[allow(unsafe_code)]
mod parser;

use codemap::CodeMap;
use diagnostics::Diagnostics;

fn main() {
    let mut code_map = CodeMap::default();
    let mut diagnostics = Diagnostics::default();
    let _ = real_main(&mut code_map, &mut diagnostics);
    diagnostics.show(&code_map);
}

fn real_main(
    code_map: &mut CodeMap,
    diagnostics: &mut Diagnostics,
) -> Result<(), ()> {
    let source_file = std::env::args()
        .nth(1)
        .ok_or_else(|| diagnostics.error("no source file provided", []))?;
    let source_code = std::fs::read_to_string(&source_file).map_err(|err| {
        diagnostics.error("failed to read source code", []);
        diagnostics.note(err.to_string(), []);
    })?;
    let file = code_map.add_file(source_file, source_code);
    let document = parser::parse(&file);
    eprintln!("{document:#?}");

    Ok(())
}
