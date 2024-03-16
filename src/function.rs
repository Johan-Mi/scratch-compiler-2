use crate::{
    diagnostics::primary,
    hir::Argument,
    ty::{Context, Ty},
};
use codemap::{Pos, Span};
use std::collections::HashMap;

type Result<T> = std::result::Result<T, ()>;

pub type ResolvedCalls = HashMap<Pos, usize>;

pub fn resolve(
    name: &str,
    arguments: &[Argument],
    span: Span,
    tcx: &mut Context,
) -> Result<(usize, Result<Ty>)> {
    let all_overloads = tcx
        .functions
        .iter()
        .filter_map(|(&index, function)| {
            ((function.owning_sprite.is_none()
                || function.owning_sprite == tcx.sprite)
                && (*function.name == name))
                .then_some(index)
        })
        .collect::<Vec<_>>();

    let typed_arguments = arguments
        .iter()
        .map(|(name, arg)| (name.as_deref(), arg.ty(None, tcx)))
        .collect::<Vec<_>>();

    let mut viable_overloads = all_overloads
        .iter()
        .copied()
        .filter_map(|overload| {
            Some(overload)
                .zip(tcx.functions[&overload].call_with(&typed_arguments))
        })
        .collect::<Vec<_>>();

    match &*viable_overloads {
        [] => {
            if all_overloads.is_empty() {
                tcx.diagnostics
                    .error("undefined function", [primary(span, "")]);
                suggest_similar(name, tcx);
            } else {
                tcx.diagnostics.error(
                    "function call has no viable overload",
                    [primary(span, "")],
                );
                let spans = all_overloads
                    .iter()
                    .map(|&overload| {
                        primary(tcx.functions[&overload].name.span, "")
                    })
                    .collect::<Vec<_>>();
                tcx.diagnostics.note(
                    "following are all of the non-viable overloads:",
                    spans,
                );
            }
            Err(())
        }
        [_] => Ok(viable_overloads.pop().unwrap()),
        _ => {
            tcx.diagnostics.error(
                "function call has more than one viable overload",
                [primary(span, "")],
            );
            let spans = viable_overloads
                .iter()
                .map(|&(overload, _)| {
                    primary(tcx.functions[&overload].name.span, "")
                })
                .collect::<Vec<_>>();
            tcx.diagnostics
                .note("following are all of the viable overloads:", spans);
            Err(())
        }
    }
}

fn suggest_similar(name: &str, tcx: &mut Context) {
    let mut all_names = tcx
        .functions
        .values()
        .filter(|it| {
            it.owning_sprite.is_none() || it.owning_sprite == tcx.sprite
        })
        .map(|function| &function.name)
        .collect::<Vec<_>>();
    // PERFORMANCE: there's really no need to sort the entire vector but it's
    // the simplest solution.
    all_names.sort_by_key(|it| levenshtein::levenshtein(name, it));

    tcx.diagnostics.help(
        "here are the functions with the most similar names:",
        all_names
            .iter()
            .take(3)
            .map(|it| primary(it.span, ""))
            .collect::<Vec<_>>(),
    );
}

pub enum Special {
    WhenFlagClicked,
}

impl TryFrom<&str> for Special {
    type Error = ();

    fn try_from(name: &str) -> Result<Self> {
        match name {
            "when-flag-clicked" => Ok(Self::WhenFlagClicked),
            _ => Err(()),
        }
    }
}
