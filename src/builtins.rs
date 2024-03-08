pub fn add_to_hir(
    document: &mut crate::hir::Document,
    code_map: &mut codemap::CodeMap,
    tcx: &mut crate::ty::Context,
) {
    let source_code = include_str!("builtins.sc2");
    let file =
        code_map.add_file("<builtins>".to_owned(), source_code.to_owned());
    let cst = crate::parser::parse(&file, tcx.diagnostics);
    let mut hir = crate::hir::lowering::lower(cst, &file, tcx);
    for function in hir.functions.values_mut() {
        function.is_from_builtins = true;
        if function.body.statements.is_empty() {
            function.is_intrinsic = true;
        }
    }
    document.functions.extend(hir.functions);
}
