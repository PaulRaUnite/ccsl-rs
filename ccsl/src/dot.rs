//! Implements translation of LightCCSL into graphviz DOT format.

use crate::kernel::constraints::Constraint;
use dot::{Arrow, ArrowShape, Edges, Fill, GraphWalk, Id, Kind, LabelText, Labeller, Nodes, Style};
use itertools::Itertools;
use petgraph::graph::{DiGraph, EdgeIndex, NodeIndex};
use std::borrow::Cow;
use std::collections::HashMap;
use std::fmt::Display;
use std::hash::Hash;
use std::io::{Result, Write};

#[derive(Clone, Debug, Hash, Eq, PartialEq)]
enum Node<C> {
    Clock(C),
    HyperNode {
        id: String,
        label: String,
        color: &'static str,
    },
}

#[derive(Clone)]
struct Edge {
    text: Option<Cow<'static, str>>,
    start_shape: Arrow,
    end_shape: Arrow,
    style: Style,
    color: &'static str,
}

impl Default for Edge {
    fn default() -> Self {
        Edge {
            text: None,
            start_shape: Arrow::default(),
            end_shape: Arrow::default(),
            style: Style::Solid,
            color: "black",
        }
    }
}
impl Edge {
    fn asyncronous() -> Self {
        Self {
            text: None,
            start_shape: Arrow::default(),
            end_shape: Arrow::default(),
            style: Style::None,
            color: "black",
        }
    }
    fn syncronous() -> Self {
        Self {
            text: None,
            start_shape: solid_ball_arrow(),
            end_shape: solid_ball_arrow(),
            style: Style::Dashed,
            color: "firebrick1",
        }
    }
}

struct SpecificationGraph<C>(String, DiGraph<Node<C>, Edge>);

type NodeRef = NodeIndex;
type EdgeRef = EdgeIndex;

fn solid_ball_arrow() -> Arrow {
    Arrow::from_arrow(ArrowShape::Dot(Fill::Filled))
}

/// Renders CCSL specification into a generic Writer.
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
        .map(|c| (constraint_to_dot_label(c), c.map_clocks(&mut node)))
        .collect_vec();
    for (node_label, constraint) in spec {
        match constraint {
            Constraint::Coincidence(c) => {
                g.add_edge(
                    c.left,
                    c.right,
                    Edge {
                        text: Some(Cow::Borrowed("=")),
                        ..Edge::syncronous()
                    },
                );
            }
            Constraint::Causality(c) => {
                g.add_edge(
                    c.left,
                    c.right,
                    Edge {
                        text: Some(Cow::Borrowed("&#x227C;")),
                        ..Edge::asyncronous()
                    },
                );
            }
            Constraint::Precedence(c) => {
                g.add_edge(
                    c.left,
                    c.right,
                    Edge {
                        text: Some(Cow::Borrowed("&#x227A;")),
                        ..Edge::asyncronous()
                    },
                );
            }
            Constraint::SubClock(c) => {
                g.add_edge(
                    c.left,
                    c.right,
                    Edge {
                        text: Some(Cow::Borrowed("&sube;")),
                        start_shape: Arrow::none(),
                        ..Edge::syncronous()
                    },
                );
            }
            Constraint::Exclusion(c) => {
                let exclusion_node = g.add_node(Node::HyperNode {
                    id: format!("exclusion_{}", c.clocks.iter().map(|i| i.index()).join("_")),
                    label: node_label,
                    color: "black",
                });
                for clock in c.clocks.iter() {
                    g.add_edge(
                        exclusion_node,
                        *clock,
                        Edge {
                            text: Some(Cow::Borrowed("&sube;")),
                            start_shape: Arrow::none(),
                            end_shape: Arrow::none(),
                            ..Edge::syncronous()
                        },
                    );
                }
            }
            Constraint::Infimum(inf) => {
                let infinum_node = g.add_node(Node::HyperNode {
                    id: format!(
                        "inf_{}_{}_{}",
                        inf.out.index(),
                        inf.left.index(),
                        inf.right.index()
                    ),
                    label: node_label,
                    color: "firebrick1",
                });
                g.add_edge(
                    infinum_node,
                    inf.out,
                    Edge {
                        text: Some(Cow::Borrowed("=")),
                        ..Edge::syncronous()
                    },
                );
                g.add_edge(inf.left, infinum_node, Edge::asyncronous());
                g.add_edge(inf.right, infinum_node, Edge::asyncronous());
            }
            Constraint::Supremum(sup) => {
                let supremum_node = g.add_node(Node::HyperNode {
                    id: format!(
                        "sup_{}_{}_{}",
                        sup.out.index(),
                        sup.left.index(),
                        sup.right.index()
                    ),
                    label: node_label,
                    color: "firebrick1",
                });
                g.add_edge(
                    supremum_node,
                    sup.out,
                    Edge {
                        text: Some(Cow::Borrowed("=")),
                        ..Edge::syncronous()
                    },
                );
                g.add_edge(sup.left, supremum_node, Edge::asyncronous());
                g.add_edge(sup.right, supremum_node, Edge::asyncronous());
            }
            Constraint::Union(un) => {
                let union_node = g.add_node(Node::HyperNode {
                    id: format!(
                        "union_{}_{}",
                        un.out.index(),
                        un.args.iter().map(|i| i.index()).join("_")
                    ),
                    label: node_label,
                    color: "black",
                });
                g.add_edge(
                    union_node,
                    un.out,
                    Edge {
                        text: Some(Cow::Borrowed("=")),
                        ..Edge::syncronous()
                    },
                );
                for clock in un.args.iter() {
                    g.add_edge(
                        union_node,
                        *clock,
                        Edge {
                            text: Some(Cow::Borrowed("&or;")),
                            start_shape: solid_ball_arrow(),
                            end_shape: Arrow::none(),
                            ..Edge::syncronous()
                        },
                    );
                }
            }
            Constraint::Intersection(int) => {
                let int_node = g.add_node(Node::HyperNode {
                    id: format!(
                        "int_{}_{}",
                        int.out.index(),
                        int.args.iter().map(|i| i.index()).join("_")
                    ),
                    label: node_label,
                    color: "black",
                });
                g.add_edge(
                    int_node,
                    int.out,
                    Edge {
                        text: Some(Cow::Borrowed("=")),
                        ..Edge::syncronous()
                    },
                );
                for clock in int.args.iter() {
                    g.add_edge(
                        int_node,
                        *clock,
                        Edge {
                            text: Some(Cow::Borrowed("&and;")),
                            start_shape: Arrow::none(),
                            end_shape: solid_ball_arrow(),
                            ..Edge::syncronous()
                        },
                    );
                }
            }
            Constraint::Minus(minus) => {
                let minus_node = g.add_node(Node::HyperNode {
                    id: format!(
                        "inf_{}_{}_{}",
                        minus.out.index(),
                        minus.left.index(),
                        minus.right.index()
                    ),
                    label: node_label,
                    color: "black",
                });
                g.add_edge(
                    minus_node,
                    minus.out,
                    Edge {
                        text: Some(Cow::Borrowed("=")),
                        ..Edge::syncronous()
                    },
                );
                g.add_edge(
                    minus.left,
                    minus_node,
                    Edge {
                        start_shape: solid_ball_arrow(),
                        ..Edge::syncronous()
                    },
                );
                g.add_edge(minus.right, minus_node, Edge::syncronous());
            }
            Constraint::Repeat(repeat) => {
                g.add_edge(
                    repeat.base,
                    repeat.out,
                    Edge {
                        text: Some(Cow::Owned(node_label)),
                        ..Edge::syncronous()
                    },
                );
            }
            Constraint::Delay(delay) => {
                if let Some(on) = delay.on {
                    let delay_node = g.add_node(Node::HyperNode {
                        id: format!(
                            "delay_{}_{}_{}_{}",
                            delay.out.index(),
                            delay.trigger.index(),
                            delay.delay,
                            on.index(),
                        ),
                        label: node_label,
                        color: "black",
                    });
                    g.add_edge(
                        delay_node,
                        delay.out,
                        Edge {
                            text: Some(Cow::Borrowed("=")),
                            ..Edge::syncronous()
                        },
                    );
                    g.add_edge(delay.trigger, delay_node, Edge::asyncronous());
                    g.add_edge(
                        on,
                        delay_node,
                        Edge {
                            start_shape: solid_ball_arrow(),
                            ..Edge::syncronous()
                        },
                    );
                } else {
                    g.add_edge(
                        delay.out,
                        delay.trigger,
                        Edge {
                            text: Some(Cow::Owned(node_label)),
                            end_shape: solid_ball_arrow(),
                            ..Edge::syncronous()
                        },
                    );
                }
            }
            Constraint::SampleOn(sample) => {
                let sample_node = g.add_node(Node::HyperNode {
                    id: format!(
                        "sample_{}_{}_{}",
                        sample.out.index(),
                        sample.base.index(),
                        sample.trigger.index()
                    ),
                    label: node_label,
                    color: "firebrick1",
                });
                g.add_edge(
                    sample_node,
                    sample.out,
                    Edge {
                        start_shape: Arrow::default(),
                        text: Some(Cow::Borrowed("=")),
                        ..Edge::syncronous()
                    },
                );
                g.add_edge(
                    sample.base,
                    sample_node,
                    Edge {
                        start_shape: Arrow::default(),
                        ..Edge::syncronous()
                    },
                );
                g.add_edge(sample.trigger, sample_node, Edge::asyncronous());
            }
            Constraint::Diff(diff) => panic!("wtf is diff"),
        }
    }
    let g = SpecificationGraph(name.to_string(), g);
    dot::render(&g, w)
}

fn constraint_to_dot_label<C: Display>(c: &Constraint<C>) -> String {
    match c {
        Constraint::Coincidence(c) => format!("{} = {}", c.left, c.right),
        Constraint::Causality(c) => format!("{} &#x227C; {}", c.left, c.right),
        Constraint::Precedence(c) => format!("{} &#x227A; {}", c.left, c.right),
        Constraint::SubClock(c) => format!("{} &sube; {}", c.left, c.right),
        Constraint::Exclusion(c) => c.clocks.iter().join("#"),
        Constraint::Infimum(c) => format!("{} = inf({}, {})", c.out, c.left, c.right),
        Constraint::Supremum(c) => format!("{} = sup({}, {})", c.out, c.left, c.right),
        Constraint::Union(c) => format!("{} = &cup;({})", c.out, c.args.iter().join(",")),
        Constraint::Intersection(c) => format!("{} = &cap;({})", c.out, c.args.iter().join(",")),
        Constraint::Minus(c) => format!("{} = {} - {}", c.out, c.left, c.right),
        Constraint::Repeat(c) => format!(
            "{}=each {} of {} [{},{}]",
            c.out,
            c.every,
            c.base,
            c.from.map_or("-&infin;".to_owned(), |v| v.to_string()),
            c.up_to.map_or("+&infin;".to_owned(), |v| v.to_string())
        ),
        Constraint::Delay(c) => format!(
            "{} = {} delayed by {}{}",
            c.out,
            c.trigger,
            c.delay,
            c.on.as_ref().map_or("".to_owned(), |v| format!("on {}", v))
        ),
        Constraint::SampleOn(c) => format!("{}", c),
        Constraint::Diff(c) => format!("{}", c),
    }
}
impl<'a, C: Display> Labeller<'a, NodeRef, EdgeRef> for SpecificationGraph<C> {
    fn graph_id(&'a self) -> Id<'a> {
        Id::new(&self.0).unwrap()
    }

    fn node_id(&'a self, n: &NodeRef) -> Id<'a> {
        match self.1.node_weight(*n).unwrap() {
            Node::Clock(c) => Id::new(format!("c{}", c)),
            Node::HyperNode { id, .. } => Id::new(id),
        }
        .unwrap()
    }

    fn node_shape(&'a self, _node: &NodeRef) -> Option<LabelText<'a>> {
        None
    }

    fn node_label(&'a self, n: &NodeRef) -> LabelText<'a> {
        match self.1.node_weight(*n).unwrap() {
            Node::Clock(c) => LabelText::label(c.to_string()),
            Node::HyperNode { label, .. } => LabelText::label(label),
        }
    }

    fn edge_label(&'a self, e: &EdgeRef) -> LabelText<'a> {
        let w = self.1.edge_weight(*e).unwrap();
        if let Some(text) = &w.text {
            LabelText::label(text.as_ref())
        } else {
            LabelText::label("")
        }
    }

    fn node_style(&'a self, n: &NodeRef) -> Style {
        match self.1.node_weight(*n).unwrap() {
            Node::Clock(_) => Style::Rounded,
            Node::HyperNode { .. } => Style::None,
        }
    }

    fn node_color(&'a self, n: &NodeRef) -> Option<LabelText<'a>> {
        Some(match self.1.node_weight(*n).unwrap() {
            Node::Clock(_) => LabelText::label("black"),
            Node::HyperNode { color, .. } => LabelText::label(*color),
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
        Some(LabelText::label(self.1.edge_weight(*e).unwrap().color))
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
