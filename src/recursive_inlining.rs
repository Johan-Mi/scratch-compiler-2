use crate::{
    diagnostics::{primary, Diagnostics},
    function::{self, ResolvedCalls},
    hir::{self, ExpressionKind, Visitor},
};
use petgraph::{prelude::NodeIndex, Direction, Graph};
use std::collections::{BTreeMap, HashMap};

pub fn check(
    document: &hir::Document,
    resolved_calls: &ResolvedCalls,
    diagnostics: &mut Diagnostics,
) {
    check_functions(&document.functions, resolved_calls, diagnostics);
    for sprite in document.sprites.values() {
        check_functions(&sprite.functions, resolved_calls, diagnostics);
    }
}

pub fn check_functions(
    functions: &BTreeMap<usize, hir::Function>,
    resolved_calls: &ResolvedCalls,
    diagnostics: &mut Diagnostics,
) {
    let mut graph = Graph::new();

    let nodes = functions
        .keys()
        .map(|&id| (id, graph.add_node(id)))
        .collect::<HashMap<_, _>>();

    for (&id, function) in functions {
        if function.is_inline {
            CallGraphVisitor {
                caller: nodes[&id],
                is_top_level: true,
                nodes: &nodes,
                graph: &mut graph,
                resolved_calls,
            }
            .traverse_function(function, true);
        }
    }

    graph.retain_nodes(|graph, node| {
        functions[&graph[node]].is_inline
            // The singleton graph is trivially a strongly connected component.
            && graph
                .neighbors_directed(node, Direction::Incoming)
                .next()
                .is_some()
    });

    let scc = petgraph::algo::tarjan_scc(&graph);
    for &node in scc.iter().flatten() {
        let span = functions[&graph[node]].name.span;
        diagnostics.error("inline function is recursive", [primary(span, "")]);
    }
}

struct CallGraphVisitor<'a> {
    caller: NodeIndex,
    is_top_level: bool,
    nodes: &'a HashMap<usize, NodeIndex>,
    graph: &'a mut Graph<usize, ()>,
    resolved_calls: &'a ResolvedCalls,
}

impl Visitor for CallGraphVisitor<'_> {
    fn visit_expression(&mut self, expr: &hir::Expression) {
        if !matches!(expr.kind, ExpressionKind::FunctionCall { .. }) {
            return;
        };
        let index = match self.resolved_calls[&expr.span.low()] {
            function::Ref::SpriteLocal(index) if !self.is_top_level => index,
            function::Ref::SpriteLocal(index) if self.is_top_level => index,
            _ => return,
        };
        self.graph.add_edge(self.caller, self.nodes[&index], ());
    }
}