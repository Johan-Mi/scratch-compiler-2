use rowan::ast::AstNode;

pub fn hir(
    generator: &mut crate::generator::Generator,
    code_map: &mut codemap::CodeMap,
    tcx: &mut crate::ty::Context,
) -> crate::hir::Document {
    let source_code = include_str!("builtins.sc2");
    let file =
        code_map.add_file("<builtins>".to_owned(), source_code.to_owned());
    let cst = crate::parser::parse(&file, tcx.diagnostics);
    let mut hir = crate::hir::Document::lower(
        &crate::ast::Document::cast(cst).unwrap(),
        generator,
        &file,
        tcx,
    );
    for function in hir.functions.values_mut() {
        function.is_from_builtins = true;
        if function.body.statements.is_empty() {
            function.is_intrinsic = true;
        }
    }
    hir
}
