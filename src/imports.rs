use rowan::ast::AstNode;
use std::collections::HashSet;

pub fn import(
    root: &mut crate::hir::Document,
    path: String,
    tcx: &mut crate::ty::Context,
    code_map: &mut codemap::CodeMap,
) -> std::io::Result<()> {
    let mut pending = Vec::from([(std::fs::canonicalize(&path)?, path)]);
    let mut done = HashSet::new();

    while let Some((absolute_path, path_name)) = pending.pop() {
        let source_code = std::fs::read_to_string(&absolute_path)?;
        let file = code_map.add_file(path_name, source_code);
        let document = crate::parser::parse(&file, tcx.diagnostics);
        crate::syntax_errors::check(&document, &file, tcx.diagnostics);
        let document = crate::ast::Document::cast(document).unwrap();

        for import in document
            .imports()
            .filter_map(|it| it.path())
            .filter_map(|it| crate::hir::parse_string_literal(it.text()).ok())
        {
            let absolute_path = std::fs::canonicalize(&import)?;
            if !done.contains(&absolute_path) {
                pending.push((absolute_path, import));
            }
        }

        let document = crate::hir::Document::lower(&document, &file, tcx);
        crate::linter::lint(&document, &file, tcx.diagnostics);

        root.merge(document);
        done.insert(absolute_path);
    }

    Ok(())
}
