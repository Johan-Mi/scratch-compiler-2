#![deny(unsafe_code)]
#![warn(clippy::nursery, clippy::pedantic)]
#![allow(clippy::enum_glob_use, clippy::option_if_let_else)]

mod ast;
mod builtins;
mod codegen;
mod comptime;
mod diagnostics;
mod early_dce;
mod formatter;
mod function;
mod hir;
mod linter;
mod mir;
mod name;
#[allow(unsafe_code)]
mod parser;
mod recursive_inlining;
mod semantics;
mod syntax_errors;
mod ty;

use codemap::CodeMap;
use diagnostics::Diagnostics;
use std::{
    collections::{BTreeMap, HashMap, HashSet},
    path::Path,
    process::ExitCode,
};

fn main() -> ExitCode {
    let mut code_map = CodeMap::default();
    let mut diagnostics = Diagnostics::default();
    let res = real_main(&mut code_map, &mut diagnostics);
    diagnostics.show(&code_map);
    match res {
        Ok(()) => ExitCode::SUCCESS,
        Err(()) => ExitCode::FAILURE,
    }
}

fn real_main(
    code_map: &mut CodeMap,
    diagnostics: &mut Diagnostics,
) -> Result<(), ()> {
    let mut args = std::env::args().skip(1);
    let command = args.next().ok_or_else(|| {
        diagnostics.error("no command provided", []);
        diagnostics
            .help("valid commands are `compile`, `check` or `format`", []);
    })?;

    match &*command {
        "compile" | "check" => {
            let source_file = args.next().ok_or_else(|| {
                diagnostics.error("no source file provided", []);
            })?;
            if args.next().is_some() {
                diagnostics.error("too many command line arguments", []);
            }
            let only_check = command == "check";
            compile_or_check(source_file, diagnostics, code_map, only_check)
        }
        "format" => {
            if args.next().is_some() {
                diagnostics.error("too many command line arguments", []);
            }
            formatter::format_stdin_to_stdout(diagnostics)
        }
        _ => {
            diagnostics.error(format!("invalid command: `{command}`"), []);
            diagnostics
                .help("valid commands are `compile`, `check` or `format`", []);
            Err(())
        }
    }
}

fn compile_or_check(
    source_file: String,
    diagnostics: &mut Diagnostics,
    code_map: &mut CodeMap,
    only_check: bool,
) -> Result<(), ()> {
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

    let mut resolved_calls = HashMap::new();

    let mut tcx = ty::Context {
        sprite: None,
        top_level_functions: &BTreeMap::new(),
        diagnostics,
        variable_types: HashMap::new(),
        comptime_known_variables: HashSet::new(),
        resolved_calls: &mut resolved_calls,
    };

    let mut document = hir::lowering::lower(document, &file, &mut tcx);
    if std::env::var_os("DUMP_HIR").is_some() {
        eprintln!("{document:#?}");
    }
    builtins::add_to_hir(&mut document, code_map, &mut tcx);
    ty::check(&document, &mut tcx);
    semantics::check(&document, diagnostics);
    recursive_inlining::check(&document, &resolved_calls, diagnostics);
    linter::lint(&document, &file, diagnostics);

    early_dce::perform(&mut document, &resolved_calls, diagnostics);

    if !diagnostics.successful() {
        return Err(());
    }

    if only_check {
        return Ok(());
    }

    let mut generator = mir::Generator::default();
    let mut document = mir::lower(document, &resolved_calls, &mut generator);
    mir::optimize(&mut document, &mut generator);
    if std::env::var_os("DUMP_MIR").is_some() {
        eprintln!("{document:#?}");
    }
    codegen::generate(document, Path::new("project.sb3")).map_err(|err| {
        diagnostics.error("failed to create project file", []);
        diagnostics.note(err.to_string(), []);
    })
}
