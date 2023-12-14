use crate::{
    ast,
    diagnostics::{primary, secondary, span, Diagnostics},
    name::Name,
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
    functions: Vec<Function>,
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

        let functions = ast
            .functions()
            .filter_map(|function| {
                Function::lower(&function, file, diagnostics).ok()
            })
            .collect();

        Ok((
            name.to_string(),
            Self {
                name_span,
                functions,
            },
        ))
    }
}

#[derive(Debug)]
pub struct Function {
    name: String,
    parameters: Vec<Parameter>,
    return_ty: Expression,
}

impl Function {
    fn lower(
        ast: &ast::Function,
        file: &File,
        diagnostics: &mut Diagnostics,
    ) -> Result<Self> {
        let defined_here = || {
            [primary(
                span(file, ast.syntax().text_range()),
                "defined here",
            )]
        };

        let name = ast
            .name()
            .ok_or_else(|| {
                diagnostics.error("function has no name", defined_here());
            })?
            .to_string();

        let parameters = ast
            .parameters()
            .ok_or_else(|| {
                diagnostics
                    .error("function has no parameter list", defined_here());
            })
            .into_iter()
            .flat_map(|p| p.parameters())
            .map(|parameter| Parameter::lower(&parameter, file, diagnostics))
            .collect::<Result<_>>()?;

        Ok(Self {
            name,
            parameters,
            return_ty: ast.return_ty().map_or(Expression::UnitType, |ty| {
                Expression::lower(&ty, file, diagnostics)
            }),
        })
    }
}

#[derive(Debug)]
pub struct Parameter {
    external_name: String,
    internal_name: String,
    ty: Expression,
}

impl Parameter {
    fn lower(
        ast: &ast::Parameter,
        file: &File,
        diagnostics: &mut Diagnostics,
    ) -> Result<Self> {
        let external_name = ast.external_name().unwrap().identifier();
        let internal_name = ast
            .internal_name()
            .ok_or_else(|| {
                diagnostics.error(
                    "function parameter has no internal name",
                    [primary(span(file, external_name.text_range()), "")],
                );
            })?
            .to_string();
        let ty = ast.ty().ok_or_else(|| {
            diagnostics.error(
                "function parameter has no type",
                [primary(span(file, external_name.text_range()), "")],
            );
        })?;
        Ok(Self {
            external_name: external_name.to_string(),
            internal_name,
            ty: Expression::lower(&ty, file, diagnostics),
        })
    }
}

#[derive(Debug)]
pub enum Expression {
    Variable(Name),
    UnitType,
    Error,
}

impl Expression {
    fn lower(
        ast: &ast::Expression,
        file: &File,
        diagnostics: &mut Diagnostics,
    ) -> Self {
        match ast {
            ast::Expression::Variable(var) => {
                let identifier = var.identifier();
                Name::resolve(&identifier).map_or_else(
                    || {
                        diagnostics.error(
                            "undefined variable",
                            [primary(span(file, identifier.text_range()), "")],
                        );
                        Self::Error
                    },
                    Self::Variable,
                )
            }
        }
    }
}
