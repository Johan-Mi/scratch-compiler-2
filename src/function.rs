use crate::{diagnostics::primary, hir::Argument, ty::Context};
use codemap::Span;

type Result<T> = std::result::Result<T, ()>;

#[derive(Debug)]
pub enum Ref {
    /// An index into the functions of the sprite containing the call.
    User(usize),
    Builtin(Builtin),
}

#[derive(Debug)]
pub enum Builtin {}

pub fn resolve(
    name: &str,
    arguments: &[Argument],
    span: Span,
    tcx: &mut Context,
) -> Result<Ref> {
    // TODO: include builtin functions
    let all_overloads = tcx
        .sprite
        .functions
        .iter()
        .enumerate()
        .filter(|(_, function)| function.name.text() == name)
        .collect::<Vec<_>>();

    let viable_overloads = all_overloads
        .iter()
        .copied()
        .filter(|(_, function)| function.can_be_called_with(arguments, tcx))
        .collect::<Vec<_>>();

    match *viable_overloads {
        [] => {
            if all_overloads.is_empty() {
                tcx.diagnostics
                    .error("undefined function", [primary(span, "")]);
            } else {
                // TODO: show non-viable overloads
                tcx.diagnostics.error(
                    "function call has no viable overloads",
                    [primary(span, "")],
                );
                tcx.diagnostics.note(
                    "following are all of the non-viable overloads:",
                    all_overloads
                        .iter()
                        .map(|(_, function)| {
                            primary(
                                crate::diagnostics::span(
                                    tcx.file,
                                    function.name.text_range(),
                                ),
                                "",
                            )
                        })
                        .collect::<Vec<_>>(),
                );
            }
            Err(())
        }
        [(index, _)] => Ok(Ref::User(index)),
        _ => {
            // TODO: show viable overloads
            tcx.diagnostics.error(
                "function call has more than one viable overload",
                [primary(span, "")],
            );
            Err(())
        }
    }
}
