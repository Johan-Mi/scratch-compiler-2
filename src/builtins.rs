pub fn add_to_hir(
    document: &mut crate::hir::Document,
    code_map: &mut codemap::CodeMap,
) {
    let source_code = include_str!("builtins.txt");
    let file =
        code_map.add_file("<builtins>".to_owned(), source_code.to_owned());
    let mut diagnostics = crate::diagnostics::Diagnostics::default();
    let cst = crate::parser::parse(&file, &mut diagnostics);
    let hir = crate::hir::lower(cst, &file, &mut diagnostics);
    debug_assert!(diagnostics.successful());
    document.functions.extend(hir.functions);
}
