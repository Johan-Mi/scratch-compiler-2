mod ast;
mod builtins;
mod codegen;
mod comptime;
mod diagnostics;
mod early_dce;
mod formatter;
mod function;
mod generator;
mod hir;
mod imports;
mod linter;
mod mir;
mod name;
#[expect(
    unsafe_code,
    reason = "`SyntaxKind` cast uses `transmute`, which is not truly required but is way less boilerplate-y than the safe way to do it"
)]
mod parser;
mod recursive_inlining;
mod semantics;
mod stdx;
mod syntax_errors;
mod ty;

use codemap::CodeMap;
use diagnostics::Diagnostics;
use std::{
    collections::{BTreeMap, HashMap},
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

fn real_main(code_map: &mut CodeMap, diagnostics: &mut Diagnostics) -> Result<(), ()> {
    let mut args = std::env::args().skip(1);
    let command = args.next().ok_or_else(|| {
        diagnostics.error("no command provided", []);
        diagnostics.help("valid commands are `compile`, `check` or `format`", []);
    })?;

    match &*command {
        "compile" | "check" => {
            let source_file = args.next().ok_or_else(|| {
                diagnostics.error("no source file provided", []);
            })?;
            if args.next().is_some() {
                diagnostics.error("too many command line arguments", []);
                return Err(());
            }
            let only_check = command == "check";
            compile_or_check(source_file, diagnostics, code_map, only_check)
        }
        "format" => {
            if args.next().is_some() {
                diagnostics.error("too many command line arguments", []);
                return Err(());
            }
            formatter::format_stdin_to_stdout(diagnostics)
        }
        _ => {
            diagnostics.error(format!("invalid command: `{command}`"), []);
            diagnostics.help("valid commands are `compile`, `check` or `format`", []);
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
    let mut resolved_calls = HashMap::new();

    let mut tcx = ty::Context {
        sprite: None,
        functions: &BTreeMap::new(),
        diagnostics,
        variable_types: HashMap::new(),
        comptime_known_variables: HashMap::new(),
        resolved_calls: &mut resolved_calls,
        function_return_ty: Err(()),
    };

    let mut generator = generator::Generator::default();

    let mut document = builtins::hir(&mut generator, code_map, tcx.diagnostics);
    imports::import(
        &mut document,
        source_file,
        &mut generator,
        tcx.diagnostics,
        code_map,
    )
    .map_err(|err| {
        tcx.diagnostics.error("failed to read source code", []);
        tcx.diagnostics.note(err.to_string(), []);
    })?;

    if std::env::var_os("DUMP_HIR").is_some() {
        eprintln!("{document:#?}");
    }

    let mut document = hir::typed::lower(document, &mut tcx, &mut generator);
    if std::env::var_os("DUMP_THIR").is_some() {
        eprintln!("{document:#?}");
    }
    ty::check(&document, &mut tcx);
    semantics::check(&document, diagnostics);
    recursive_inlining::check(&document, &resolved_calls, diagnostics);

    early_dce::perform(&mut document, &resolved_calls, diagnostics);

    if !diagnostics.successful() {
        return Err(());
    }

    if only_check {
        return Ok(());
    }

    let (mut document, stage_variables, stage_lists) =
        mir::lower(document, &resolved_calls, &mut generator);
    mir::optimize(&mut document, &mut generator);
    if std::env::var_os("DUMP_MIR").is_some() {
        eprintln!("{document:#?}");
    }
    codegen::generate(
        document,
        Path::new("project.sb3"),
        &stage_variables,
        &stage_lists,
    )
    .map_err(|err| {
        diagnostics.error("failed to create project file", []);
        diagnostics.note(err.to_string(), []);
    })
}
