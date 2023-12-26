use crate::{
    diagnostics::primary,
    hir::{Argument, Parameter},
    ty::{Context, Ty},
};
use codemap::Span;

type Result<T> = std::result::Result<T, ()>;

#[derive(Debug, Clone, Copy)]
pub enum Ref {
    /// An index into the functions of the sprite containing the call.
    User(usize),
    Builtin(&'static Builtin),
}

#[derive(Debug)]
pub struct Builtin {
    name: &'static str,
    parameters: &'static [Parameter],
    pub return_ty: Ty,
}

impl Builtin {
    fn can_be_called_with(
        &self,
        arguments: &[Argument],
        tcx: &mut Context,
    ) -> bool {
        self.parameters.len() == arguments.len()
            && std::iter::zip(self.parameters, arguments).all(
                |(parameter, argument)| {
                    parameter.is_compatible_with(argument, tcx)
                },
            )
    }
}

pub fn resolve(
    name: &str,
    arguments: &[Argument],
    span: Span,
    tcx: &mut Context,
) -> Result<Ref> {
    let user_defined_overloads = tcx
        .sprite
        .functions
        .iter()
        .enumerate()
        .filter(|(_, function)| function.name.text() == name)
        .map(|(index, _)| Ref::User(index));

    let builtin_overloads = BUILTINS
        .iter()
        .filter(|builtin| builtin.name == name)
        .map(Ref::Builtin);

    let all_overloads = user_defined_overloads
        .chain(builtin_overloads)
        .collect::<Vec<_>>();

    let viable_overloads = all_overloads
        .iter()
        .copied()
        .filter(|&overload| match overload {
            Ref::User(index) => {
                tcx.sprite.functions[index].can_be_called_with(arguments, tcx)
            }
            Ref::Builtin(builtin) => builtin.can_be_called_with(arguments, tcx),
        })
        .collect::<Vec<_>>();

    let spans = |overloads: &[Ref]| {
        overloads
            .iter()
            .filter_map(|&overload| match overload {
                Ref::User(index) => Some(primary(
                    crate::diagnostics::span(
                        tcx.file,
                        tcx.sprite.functions[index].name.text_range(),
                    ),
                    "",
                )),
                Ref::Builtin(_) => None,
            })
            .collect::<Vec<_>>()
    };

    match *viable_overloads {
        [] => {
            if all_overloads.is_empty() {
                tcx.diagnostics
                    .error("undefined function", [primary(span, "")]);
            } else {
                tcx.diagnostics.error(
                    "function call has no viable overloads",
                    [primary(span, "")],
                );
                tcx.diagnostics.note(
                    "following are all of the non-viable overloads:",
                    spans(&all_overloads),
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
                spans(&viable_overloads),
            );
            Err(())
        }
    }
}

static BUILTINS: &[Builtin] = &[];
