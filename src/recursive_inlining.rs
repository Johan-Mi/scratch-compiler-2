use crate::{
    diagnostics::{primary, Diagnostics},
    function::ResolvedCalls,
    hir::{self, typed::Document, ExpressionKind, Visitor},
};
use petgraph::{
    prelude::{EdgeIndex, NodeIndex},
    Direction, Graph,
};
use std::collections::HashMap;

pub fn check(
    document: &Document,
    resolved_calls: &ResolvedCalls,
    diagnostics: &mut Diagnostics,
) {
    let mut graph = Graph::new();

    let nodes = document
        .functions
        .keys()
        .map(|&id| (id, graph.add_node(id)))
        .collect::<HashMap<_, _>>();

    for (&id, function) in &document.functions {
        if function.is_inline {
            CallGraphVisitor {
                caller: nodes[&id],
                nodes: &nodes,
                graph: &mut graph,
                resolved_calls,
            }
            .traverse_function(function);
        }
    }

    graph.retain_nodes(|graph, node| {
        document.functions[&graph[node]].is_inline
            // The singleton graph is trivially a strongly connected component.
            && graph
                .neighbors_directed(node, Direction::Incoming)
                .next()
                .is_some()
    });

    petgraph::algo::TarjanScc::new().run(&graph, |scc| {
        for &node in scc {
            let span = document.functions[&graph[node]].name.span;
            diagnostics
                .error("inline function is recursive", [primary(span, "")]);
        }
    });
}

struct CallGraphVisitor<'a> {
    caller: NodeIndex,
    nodes: &'a HashMap<usize, NodeIndex>,
    graph: &'a mut Graph<usize, ()>,
    resolved_calls: &'a ResolvedCalls,
}

impl Visitor for CallGraphVisitor<'_> {
    fn visit_expression(&mut self, expr: &hir::Expression) {
        let ExpressionKind::FunctionCall { name_span, .. } = expr.kind else {
            return;
        };
        let Some(index) = self.resolved_calls.get(&name_span.low()) else {
            return;
        };
        let _: EdgeIndex =
            self.graph.add_edge(self.caller, self.nodes[index], ());
    }
}
