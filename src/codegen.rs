use crate::hir;
use sb3_builder::Project;
use std::path::Path;

pub fn generate(
    document: &hir::Document,
    output_path: &Path,
) -> Result<(), Box<dyn std::error::Error>> {
    let project = Project::default();

    let file = std::fs::File::create(output_path)?;
    project.finish(file)
}
