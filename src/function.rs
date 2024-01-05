use crate::{
    diagnostics::primary,
    hir::{Argument, Function},
    ty::Context,
};
use codemap::{Pos, Span};
use std::collections::HashMap;

type Result<T> = std::result::Result<T, ()>;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Ref {
    /// An index into the functions of the sprite containing the call.
    SpriteLocal(usize),
    /// An index into the top-level functions.
    TopLevel(usize),
}

impl<'a> Context<'a> {
    pub fn function(&self, index: Ref) -> &'a Function {
        match index {
            Ref::SpriteLocal(index) => {
                &self.sprite.as_ref().unwrap().functions[&index]
            }
            Ref::TopLevel(index) => &self.top_level_functions[&index],
        }
    }
}

pub type ResolvedCalls = HashMap<Pos, Ref>;

pub fn resolve(
    name: &str,
    arguments: &[Argument],
    span: Span,
    tcx: &mut Context,
) -> Result<Ref> {
    let sprite_local_overloads = tcx
        .sprite
        .iter()
        .flat_map(|sprite| &sprite.functions)
        .filter(|(_, function)| *function.name == name)
        .map(|(&index, _)| Ref::SpriteLocal(index));

    let top_level_overloads = tcx
        .top_level_functions
        .iter()
        .filter(|(_, function)| *function.name == name)
        .map(|(&index, _)| Ref::TopLevel(index));

    let all_overloads = sprite_local_overloads
        .chain(top_level_overloads)
        .collect::<Vec<_>>();

    let viable_overloads = all_overloads
        .iter()
        .copied()
        .filter(|&overload| {
            tcx.function(overload).can_be_called_with(arguments, tcx)
        })
        .collect::<Vec<_>>();

    let spans = |tcx: &Context, overloads: &[Ref]| {
        overloads
            .iter()
            .map(|&overload| primary(tcx.function(overload).name.span, ""))
            .collect::<Vec<_>>()
    };

    match *viable_overloads {
        [] => {
            if all_overloads.is_empty() {
                tcx.diagnostics
                    .error("undefined function", [primary(span, "")]);
            } else {
                tcx.diagnostics.error(
                    "function call has no viable overload",
                    [primary(span, "")],
                );
                tcx.diagnostics.note(
                    "following are all of the non-viable overloads:",
                    spans(tcx, &all_overloads),
                );
            }
            Err(())
        }
        [overload] => Ok(overload),
        _ => {
            tcx.diagnostics.error(
                "function call has more than one viable overload",
                [primary(span, "")],
            );
            tcx.diagnostics.note(
                "following are all of the viable overloads:",
                spans(tcx, &viable_overloads),
            );
            Err(())
        }
    }
}
