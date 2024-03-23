use crate::generator::Generator;
use rowan::ast::AstNode;
use std::{collections::HashSet, io};

pub fn import(
    root: &mut crate::hir::Document,
    path: String,
    generator: &mut Generator,
    tcx: &mut crate::ty::Context,
    code_map: &mut codemap::CodeMap,
) -> io::Result<()> {
    let mut pending = Vec::from([(std::fs::canonicalize(&path)?, path)]);
    let mut done = HashSet::new();

    let sandbox_root = std::env::current_dir()?;

    while let Some((absolute_path, path_name)) = pending.pop() {
        if !absolute_path.starts_with(&sandbox_root) {
            return Err(io::Error::new(
                io::ErrorKind::PermissionDenied,
                format!("{path_name:?} is outside of the compilation sandbox"),
            ));
        }

        let source_code = std::fs::read_to_string(&absolute_path)?;
        done.insert(absolute_path.clone());
        let file = code_map.add_file(path_name, source_code);
        let document = crate::parser::parse(&file, tcx.diagnostics);
        crate::syntax_errors::check(&document, &file, tcx.diagnostics);
        let document = crate::ast::Document::cast(document).unwrap();

        for import in document
            .imports()
            .filter_map(|it| it.path())
            .filter_map(|it| crate::hir::parse_string_literal(&it).ok())
        {
            let absolute_path = std::fs::canonicalize(
                &absolute_path.parent().unwrap().join(&import),
            )?;
            if !done.contains(&absolute_path) {
                pending.push((absolute_path, import));
            }
        }

        let document =
            crate::hir::Document::lower(&document, generator, &file, tcx);
        crate::linter::lint(&document, &file, tcx.diagnostics);

        root.merge(document);
    }

    Ok(())
}
