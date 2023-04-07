use crate::kernel::constraints::Constraint;
use dot::{Arrow, ArrowShape, Edges, Fill, GraphWalk, Id, Kind, LabelText, Labeller, Nodes, Style};
use itertools::Itertools;
use petgraph::graph::{DiGraph, EdgeIndex, NodeIndex};
use std::collections::HashMap;
use std::fmt::Display;
use std::hash::Hash;
use std::io::{Result, Write};

#[derive(Clone, Debug, Hash, Eq, PartialEq)]
enum Node<C> {
    Clock(C),
    HyperNode(String),
}

#[derive(Clone)]
struct Edge {
    text: Option<String>,
    start_shape: Arrow,
    end_shape: Arrow,
    style: Style,
}

impl Default for Edge {
    fn default() -> Self {
        Edge {
            text: None,
            start_shape: Arrow::default(),
            end_shape: Arrow::default(),
            style: Style::Bold,
        }
    }
}

struct SpecificationGraph<C>(String, DiGraph<Node<C>, Edge>);

type NodeRef = NodeIndex;
type EdgeRef = EdgeIndex;

pub fn render_dot<C: Display + Hash + Eq + Clone, W: Write>(
    w: &mut W,
    name: &str,
    spec: &[Constraint<C>],
) -> Result<()> {
    let mut nodes = HashMap::new();
    let mut g = DiGraph::new();
    let mut node = |clock: &C| {
        *nodes
            .entry(clock.clone())
            .or_insert_with(|| g.add_node(Node::Clock(clock.clone())))
    };
    let spec = spec
        .iter()
        .map(|c| c.map_clocks(|clock| node(clock)))
        .collect_vec();
    for constraint in spec {
        match constraint {
            Constraint::Coincidence(c) => {}
            Constraint::Causality(c) => {
                g.add_edge(
                    c.left,
                    c.right,
                    Edge {
                        text: Some("<".to_string()),
                        start_shape: Arrow::default(),
                        end_shape: Arrow::default(),
                        style: Style::None,
                    },
                );
            }
            Constraint::Precedence(p) => {
                g.add_edge(
                    p.left,
                    p.right,
                    Edge {
                        text: Some("<=".to_string()),
                        start_shape: Arrow::default(),
                        end_shape: Arrow::default(),
                        style: Style::None,
                    },
                );
            }
            Constraint::SubClock(sub) => {}
            Constraint::Exclusion(ex) => {}
            Constraint::Infinity(inf) => {}
            Constraint::Supremum(sup) => {}
            Constraint::Union(un) => {}
            Constraint::Intersection(int) => {}
            Constraint::Minus(minus) => {}
            Constraint::Repeat(repeat) => {}
            Constraint::Delay(delay) => {
                g.add_edge(
                    delay.out,
                    delay.base,
                    Edge {
                        text: Some(format!("delayed by {}", delay.delay)),
                        start_shape: Arrow::from_arrow(ArrowShape::Dot(Fill::Filled)),
                        end_shape: Arrow::default(),
                        style: Style::Dashed,
                    },
                );
            }
            Constraint::SampleOn(sample) => {}
            Constraint::Diff(diff) => panic!("wtf is diff"),
        }
    }
    let g = SpecificationGraph(name.to_string(), g);
    dot::render(&g, w)
}

impl<'a, C: Display> Labeller<'a, NodeRef, EdgeRef> for SpecificationGraph<C> {
    fn graph_id(&'a self) -> Id<'a> {
        Id::new(&self.0).unwrap()
    }

    fn node_id(&'a self, n: &NodeRef) -> Id<'a> {
        match self.1.node_weight(*n).unwrap() {
            Node::Clock(c) => Id::new(format!("c{}", c)),
            Node::HyperNode(s) => Id::new(s),
        }
        .unwrap()
    }

    fn node_shape(&'a self, _node: &NodeRef) -> Option<LabelText<'a>> {
        None
    }

    fn node_label(&'a self, n: &NodeRef) -> LabelText<'a> {
        match self.1.node_weight(*n).unwrap() {
            Node::Clock(c) => LabelText::label(c.to_string()),
            Node::HyperNode(s) => LabelText::label(s),
        }
    }

    fn edge_label(&'a self, e: &EdgeRef) -> LabelText<'a> {
        let w = self.1.edge_weight(*e).unwrap();
        if let Some(text) = &w.text {
            LabelText::label(text)
        } else {
            LabelText::label("")
        }
    }

    fn node_style(&'a self, n: &NodeRef) -> Style {
        match self.1.node_weight(*n).unwrap() {
            Node::Clock(_) => Style::Rounded,
            Node::HyperNode(_) => Style::Wedged,
        }
    }

    fn node_color(&'a self, n: &NodeRef) -> Option<LabelText<'a>> {
        Some(match self.1.node_weight(*n).unwrap() {
            Node::Clock(_) => LabelText::label("black"),
            Node::HyperNode(_) => LabelText::label("redbrick1"),
        })
    }

    fn edge_end_arrow(&'a self, e: &EdgeRef) -> Arrow {
        self.1.edge_weight(*e).unwrap().end_shape.clone()
    }

    fn edge_start_arrow(&'a self, e: &EdgeRef) -> Arrow {
        self.1.edge_weight(*e).unwrap().start_shape.clone()
    }

    fn edge_style(&'a self, e: &EdgeRef) -> Style {
        self.1.edge_weight(*e).unwrap().style
    }

    fn edge_color(&'a self, e: &EdgeRef) -> Option<LabelText<'a>> {
        None
    }

    fn kind(&self) -> Kind {
        Kind::Digraph
    }
}

impl<'a, C: Clone> GraphWalk<'a, NodeRef, EdgeRef> for SpecificationGraph<C> {
    fn nodes(&'a self) -> Nodes<'a, NodeRef> {
        self.1.node_indices().collect()
    }

    fn edges(&'a self) -> Edges<'a, EdgeRef> {
        self.1.edge_indices().collect()
    }

    fn source(&'a self, edge: &EdgeRef) -> NodeRef {
        self.1.edge_endpoints(*edge).unwrap().0
    }

    fn target(&'a self, edge: &EdgeRef) -> NodeRef {
        self.1.edge_endpoints(*edge).unwrap().1
    }
}
