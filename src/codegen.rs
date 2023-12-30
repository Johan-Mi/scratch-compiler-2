use crate::hir;
use sb3_builder::{block, Costume, Project, Target};
use std::path::Path;

type Result<T> = std::result::Result<T, Box<dyn std::error::Error>>;

pub fn generate(document: hir::Document, output_path: &Path) -> Result<()> {
    let mut project = Project::default();

    for (name, sprite) in document.sprites {
        compile_sprite(sprite, name, &mut project)?;
    }

    let file = std::fs::File::create(output_path)?;
    project.finish(file)
}

fn compile_sprite(
    hir: hir::Sprite,
    name: String,
    project: &mut Project,
) -> Result<()> {
    let mut sprite = if name == "Stage" {
        project.stage()
    } else {
        project.add_sprite(name)
    };

    for costume in hir.costumes {
        sprite.add_costume(Costume::from_file(
            costume.name,
            costume.path.as_ref(),
        )?);
    }

    for function in &hir.functions {
        compile_function(function, &mut sprite);
    }

    Ok(())
}

fn compile_function(hir: &hir::Function, sprite: &mut Target) {
    match &**hir.name {
        "when-flag-clicked" => {
            sprite.start_script(block::when_flag_clicked());
        }
        _ => todo!(),
    }
}
