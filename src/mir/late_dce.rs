use super::{Document, Op, Visitor};
use std::collections::BTreeSet;

pub fn perform(document: &mut Document) {
    let mut visitor = DceVisitor {
        pending_functions: document
            .functions
            .iter()
            .filter_map(|(&index, function)| {
                crate::function::Special::try_from(&*function.name)
                    .is_ok()
                    .then_some(index)
            })
            .collect(),
        required_functions: BTreeSet::new(),
    };

    while let Some(index) = visitor.pending_functions.pop_last() {
        visitor.required_functions.insert(index);
        visitor.traverse_function(document.functions.get_mut(&index).unwrap());
    }
    document
        .functions
        .retain(|index, _| visitor.required_functions.contains(index));
}

#[allow(clippy::struct_field_names)]
struct DceVisitor {
    pending_functions: BTreeSet<usize>,
    required_functions: BTreeSet<usize>,
}

impl Visitor for DceVisitor {
    fn visit_op(&mut self, op: &mut Op) {
        let Op::Call { function, .. } = *op else {
            return;
        };
        if !self.required_functions.contains(&function) {
            self.pending_functions.insert(function);
        }
    }
}
