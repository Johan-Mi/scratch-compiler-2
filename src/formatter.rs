use crate::diagnostics::Diagnostics;
use codemap::CodeMap;

pub fn format_stdin_to_stdout(diagnostics: &mut Diagnostics) -> Result<(), ()> {
    let source_code = std::io::read_to_string(std::io::stdin())
        .map_err(|err| diagnostics.error(err.to_string(), []))?;
    print!("{}", format(source_code));
    Ok(())
}

fn format(source_code: String) -> String {
    let file = CodeMap::new().add_file(String::new(), source_code);
    let cst = crate::parser::parse(&file, &mut Diagnostics::default());

    // TODO: actually implement the formatter
    cst.text().to_string()
}
