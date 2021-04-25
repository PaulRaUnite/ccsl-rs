use std::hash::Hash;
use std::iter::once;

use itertools::Itertools;
use petgraph::Graph;

use crate::lccsl::automata::{ClockLabel, Delta, State, STS};
use crate::lccsl::expressions::BooleanExpression;

pub fn unfold_specification<C>(spec: &[STS<C>]) -> Vec<Graph<String, ClockLabel<C>>>
where
    C: Ord + Hash + Clone,
{
    spec.iter()
        .map(|sts| sts.states().iter())
        .multi_cartesian_product()
        .map(|comb| unfold_state_combination(spec, &comb))
        .collect()
}

fn unfold_state_combination<'a, 'b, C>(
    spec: &'a [STS<C>],
    comb: &'b [&'a State<BooleanExpression<Delta<C>>>],
) -> Graph<String, ClockLabel<'a, C>>
where
    C: Hash + Clone + Ord + 'a,
{
    let mut g = Graph::new();
    let root = g.add_node("R".to_owned());

    let mut previous_nodes: Vec<(Vec<ClockLabel<C>>, _)> = vec![(vec![], root)];

    for (i, (c, s)) in spec.iter().zip(comb.iter()).enumerate() {
        previous_nodes = previous_nodes
            .iter()
            .cartesian_product(c.transitions(s))
            .map(|((before, prev), t)| {
                let mut conflicts = vec![];
                for (i, t2) in before.iter().enumerate() {
                    if t.label.has_conflict(t2) {
                        conflicts.push(i)
                    }
                }
                let next = g.add_node(format!("C{}({})", i, conflicts.iter().join(",")));
                g.add_edge(*prev, next, t.label.clone());
                (
                    before
                        .iter()
                        .map(|v| v.clone())
                        .chain(once(t.label))
                        .collect(),
                    next,
                )
            })
            .collect();
    }
    g
}
