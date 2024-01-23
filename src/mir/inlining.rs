use crate::{
    function,
    mir::{
        visit::SsaVarReplacer, Block, Document, Function, Op, SsaVar,
        SsaVarGenerator, Value, Visitor,
    },
};
use std::{collections::HashMap, mem};

pub fn inline(document: &mut Document, ssa_var_gen: &mut SsaVarGenerator) {
    while let Some(id) = document
        .functions
        .iter()
        .find_map(|(&id, function)| function.is_inline.then_some(id))
    {
        let mut function = document.functions.remove(&id).unwrap();

        // Optimize the function before inlining it to avoid duplicate work.
        super::optimization::optimize(&mut function);

        Inliner {
            function,
            function_ref: function::Ref::TopLevel(id),
            ssa_var_gen,
        }
        .traverse_document(document);
    }

    for sprite in document.sprites.values_mut() {
        while let Some(id) = sprite
            .functions
            .iter()
            .find_map(|(&id, function)| function.is_inline.then_some(id))
        {
            let mut function = sprite.functions.remove(&id).unwrap();

            // Optimize the function before inlining it to avoid duplicate work.
            super::optimization::optimize(&mut function);

            Inliner {
                function,
                function_ref: function::Ref::SpriteLocal(id),
                ssa_var_gen,
            }
            .traverse_sprite(sprite);
        }
    }
}

struct SsaVarRefresher<'a> {
    ssa_var_gen: &'a mut SsaVarGenerator,
    renames: HashMap<SsaVar, SsaVar>,
}

impl Visitor for SsaVarRefresher<'_> {
    fn visit_op(&mut self, op: &mut Op) {
        let (Op::Call {
            variable: Some(variable),
            ..
        }
        | Op::CallBuiltin {
            variable: Some(variable),
            ..
        }) = op
        else {
            return;
        };
        let new_var = self.ssa_var_gen.new_ssa_var();
        self.renames.insert(*variable, new_var);
        *variable = new_var;
    }

    fn visit_value(&mut self, value: &mut Value) {
        let Value::Var(variable) = value else { return };
        if let Some(&new_var) = self.renames.get(variable) {
            *variable = new_var;
        }
    }
}

struct Inliner<'a> {
    function: Function,
    function_ref: function::Ref,
    ssa_var_gen: &'a mut SsaVarGenerator,
}

impl Visitor for Inliner<'_> {
    fn visit_block(&mut self, block: &mut Block) {
        while let Some((index, variable, args)) = block
            .ops
            .iter_mut()
            .enumerate()
            .find_map(|(index, op)| match op {
                Op::Call {
                    variable,
                    function,
                    args,
                } if *function == self.function_ref => {
                    Some((index, variable, args))
                }
                _ => None,
            })
        {
            let mut cloned_body = self.function.body.clone();
            for (arg, param) in
                std::iter::zip(mem::take(args), &self.function.parameters)
            {
                SsaVarReplacer {
                    variable: param.ssa_var,
                    replacement: arg,
                }
                .traverse_block(&mut cloned_body);
            }
            SsaVarRefresher {
                ssa_var_gen: self.ssa_var_gen,
                renames: HashMap::new(),
            }
            .traverse_block(&mut cloned_body);

            if let Some(variable) = *variable {
                let Some(Op::Return(return_value)) = cloned_body.ops.pop()
                else {
                    unreachable!()
                };
                SsaVarReplacer {
                    variable,
                    replacement: return_value,
                }
                .traverse_block(block);
            }

            block.ops.splice(index..=index, cloned_body.ops);
        }
    }
}
