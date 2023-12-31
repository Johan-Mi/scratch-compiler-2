#![deny(unsafe_code)]
#![warn(clippy::nursery, clippy::pedantic)]
#![allow(clippy::enum_glob_use)]

mod ast;
mod builtins;
mod codegen;
mod comptime;
mod diagnostics;
mod early_dce;
mod function;
mod hir;
mod linter;
mod name;
#[allow(unsafe_code)]
mod parser;
mod semantics;
mod syntax_errors;
mod ty;

use codemap::CodeMap;
use diagnostics::Diagnostics;
use std::{path::Path, process::ExitCode};

fn main() -> ExitCode {
    let mut code_map = CodeMap::default();
    let mut diagnostics = Diagnostics::default();
    let res = real_main(&mut code_map, &mut diagnostics);
    diagnostics.show(&code_map);
    if res.is_ok() {
        ExitCode::SUCCESS
    } else {
        ExitCode::FAILURE
    }
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
    let document = parser::parse(&file, diagnostics);
    syntax_errors::check(&document, &file, diagnostics);
    if std::env::var_os("DUMP_CST").is_some() {
        eprintln!("{document:#?}");
    }
    let mut document = hir::lower(document, &file, diagnostics);
    if std::env::var_os("DUMP_HIR").is_some() {
        eprintln!("{document:#?}");
    }
    builtins::add_to_hir(&mut document, code_map);
    let resolved_calls = ty::check(&document, &file, diagnostics);
    semantics::check(&document, diagnostics);
    linter::lint(&document, &file, diagnostics);

    if !diagnostics.successful() {
        return Err(());
    }

    early_dce::perform(&mut document, &resolved_calls);

    codegen::generate(document, &resolved_calls, Path::new("project.sb3"))
        .map_err(|err| {
            diagnostics.error("failed to create project file", []);
            diagnostics.note(err.to_string(), []);
        })
}
