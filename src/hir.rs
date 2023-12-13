use crate::{
    ast,
    diagnostics::{primary, secondary, span, Diagnostics},
    parser::SyntaxNode,
};
use codemap::{File, Span};
use rowan::ast::AstNode;
use std::collections::HashMap;

/// All error reporting uses the `Diagnostics` struct. This typedef is only
/// used to make short-circuiting more convenient. A result of `Ok(())` does not
/// necessarily mean that there are no errors; broken HIR may be constructed for
/// the sake of resilience.
type Result<T> = std::result::Result<T, ()>;

pub fn lower(
    document: SyntaxNode,
    file: &File,
    diagnostics: &mut Diagnostics,
) -> Document {
    Document::lower(&ast::Document::cast(document).unwrap(), file, diagnostics)
}

#[derive(Debug)]
pub struct Document {
    sprites: Vec<Sprite>,
}

impl Document {
    pub fn lower(
        ast: &ast::Document,
        file: &File,
        diagnostics: &mut Diagnostics,
    ) -> Self {
        let mut sprites = HashMap::<_, Sprite>::new();

        for sprite in ast.sprites() {
            let Ok((name, sprite)) = Sprite::lower(&sprite, file, diagnostics)
            else {
                continue;
            };

            if let Some(prev_sprite) = sprites.get(&*name) {
                diagnostics.error(
                    format!("redefinition of sprite `{name}`"),
                    [
                        primary(sprite.name_span, "second definition here"),
                        secondary(
                            prev_sprite.name_span,
                            "previously defined here",
                        ),
                    ],
                );
            } else {
                sprites.insert(name, sprite);
            }
        }

        Self {
            sprites: sprites.into_values().collect(),
        }
    }
}

#[derive(Debug)]
pub struct Sprite {
    name_span: Span,
}

impl Sprite {
    fn lower(
        ast: &ast::Sprite,
        file: &File,
        diagnostics: &mut Diagnostics,
    ) -> Result<(String, Self)> {
        let name = ast.name().ok_or_else(|| {
            diagnostics.error(
                "sprite has no name",
                [primary(
                    span(file, ast.syntax().text_range()),
                    "defined here",
                )],
            );
        })?;
        let name_span = span(file, name.text_range());
        Ok((name.to_string(), Self { name_span }))
    }
}
