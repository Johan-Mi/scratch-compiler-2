use crate::mir::{
    visit::SsaVarReplacer, Block, Document, Function, Generator, Op, SsaVar, Value, Visitor,
};
use std::{collections::HashMap, mem};

use super::Call;

pub fn inline(document: &mut Document, generator: &mut Generator) {
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
            id,
            generator,
        }
        .traverse_document(document);
    }
}

struct SsaVarRefresher<'a> {
    generator: &'a mut Generator,
    renames: HashMap<SsaVar, SsaVar>,
}

impl Visitor for SsaVarRefresher<'_> {
    fn visit_op(&mut self, op: &mut Op) {
        let Op::Call(Some(variable), _) = op else {
            return;
        };
        let new_var = self.generator.new_ssa_var();
        assert!(self.renames.insert(*variable, new_var).is_none());
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
    id: usize,
    generator: &'a mut Generator,
}

impl Visitor for Inliner<'_> {
    fn visit_block(&mut self, block: &mut Block) {
        while let Some((index, variable, args)) =
            block
                .ops
                .iter_mut()
                .enumerate()
                .find_map(|(index, op)| match op {
                    Op::Call(variable, Call::Custom { function, args }) if *function == self.id => {
                        Some((index, variable, args))
                    }
                    _ => None,
                })
        {
            let mut cloned_body = self.function.body.clone();
            for (arg, param) in std::iter::zip(mem::take(args), &self.function.parameters) {
                SsaVarReplacer {
                    variable: param.ssa_var,
                    replacement: arg,
                }
                .traverse_block(&mut cloned_body);
            }
            SsaVarRefresher {
                generator: self.generator,
                renames: HashMap::new(),
            }
            .traverse_block(&mut cloned_body);

            if let Some(variable) = *variable {
                let Some(Op::Return {
                    value: return_value,
                    ..
                }) = cloned_body.ops.pop()
                else {
                    unreachable!()
                };
                SsaVarReplacer {
                    variable,
                    replacement: return_value,
                }
                .traverse_block(block);
            }

            _ = block.ops.splice(index..=index, cloned_body.ops);
        }
    }
}
