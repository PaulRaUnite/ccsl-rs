use std::hash::Hash;
use std::iter::once;

use itertools::Itertools;
use petgraph::Graph;

use crate::lccsl::automata::{ClockLabel, Delta, State, STS};
use crate::lccsl::expressions::BooleanExpression;
use petgraph::prelude::NodeIndex;

pub fn unfold_specification<C>(spec: &[STS<C>], trim: bool) -> Vec<Graph<String, ClockLabel<C>>>
where
    C: Ord + Hash + Clone,
{
    spec.iter()
        .map(|sts| sts.states().iter())
        .multi_cartesian_product()
        .map(|comb| unfold_state_combination(spec, &comb, trim))
        .collect()
}

fn unfold_state_combination<'a, 'b, C>(
    spec: &'a [STS<C>],
    comb: &'b [&'a State<BooleanExpression<Delta<C>>>],
    trim: bool,
) -> Graph<String, ClockLabel<'a, C>>
where
    C: Hash + Clone + Ord + 'a,
{
    let mut g = Graph::new();
    let root = g.add_node(format!("0:{}", spec[0]));

    let mut previous_nodes: Vec<(Vec<ClockLabel<C>>, NodeIndex, bool)> =
        vec![(vec![], root, false)];

    for (i, (c, s)) in spec.iter().zip(comb.iter()).enumerate() {
        previous_nodes = previous_nodes
            .iter()
            .cartesian_product(c.transitions(s))
            .filter_map(|((before, prev, conflicted), t)| {
                let mut conflicts = vec![];
                for (i, t2) in before.iter().enumerate() {
                    if t.label.has_conflict(t2) {
                        conflicts.push(i)
                    }
                }
                let conflicted = *conflicted || conflicts.len() > 0;

                let conflicts = if conflicts.len() == 0 {
                    "".to_owned()
                } else {
                    format!("({})", conflicts.iter().join(","))
                };
                let next = if i != spec.len() - 1 {
                    g.add_node(format!("{}:{}{}", i + 1, spec[i + 1], conflicts))
                } else {
                    g.add_node(format!(
                        "{}{}",
                        if conflicted { "*" } else { "S" },
                        conflicts
                    ))
                };
                g.add_edge(*prev, next, t.label.clone());
                if conflicted && trim {
                    None
                } else {
                    Some((
                        before
                            .iter()
                            .map(|v| v.clone())
                            .chain(once(t.label))
                            .collect(),
                        next,
                        conflicted,
                    ))
                }
            })
            .collect();
    }
    g
}
