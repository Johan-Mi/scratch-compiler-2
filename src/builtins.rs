use rowan::ast::AstNode;

pub fn hir(
    generator: &mut crate::generator::Generator,
    code_map: &mut codemap::CodeMap,
    diagnostics: &mut crate::diagnostics::Diagnostics,
) -> crate::hir::Document {
    let source_code = include_str!("builtins.sc2");
    let file = code_map.add_file("<builtins>".to_owned(), source_code.to_owned());
    let cst = crate::parser::parse(&file, diagnostics);
    let mut hir = crate::hir::Document::lower(
        &crate::ast::Document::cast(cst).unwrap(),
        generator,
        &file,
        diagnostics,
    );
    for function in hir.functions.values_mut() {
        function.kind = crate::hir::FunctionKind::Intrinsic;
    }
    hir
}
