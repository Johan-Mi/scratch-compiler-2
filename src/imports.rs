pub fn import(
    root: &mut crate::hir::Document,
    path: String,
    tcx: &mut crate::ty::Context,
    code_map: &mut codemap::CodeMap,
) -> Result<(), ()> {
    let source_code = std::fs::read_to_string(&path).map_err(|err| {
        tcx.diagnostics.error("failed to read source code", []);
        tcx.diagnostics.note(err.to_string(), []);
    })?;
    let file = code_map.add_file(path, source_code);
    let document = crate::parser::parse(&file, tcx.diagnostics);
    crate::syntax_errors::check(&document, &file, tcx.diagnostics);
    let document = crate::hir::lowering::lower(document, &file, tcx);
    crate::linter::lint(&document, &file, tcx.diagnostics);

    root.merge(document);

    Ok(())
}
