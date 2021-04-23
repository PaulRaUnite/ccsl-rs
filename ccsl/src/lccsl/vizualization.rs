use std::collections::HashMap;
use std::hash::Hash;
use std::iter::once;
use std::rc::Rc;

use itertools::Itertools;
use petgraph::Graph;

use crate::lccsl::algo::conflict;
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

fn unfold_state_combination<C>(
    spec: &[STS<C>],
    comb: &[&State<BooleanExpression<Delta<C>>>],
) -> Graph<String, ClockLabel<C>>
where
    C: Hash + Clone + Ord,
{
    let mut g = Graph::new();
    let root = g.add_node("R".to_owned());

    let mut previous_nodes: Vec<(Vec<Rc<HashMap<&C, bool>>>, _)> = vec![(vec![], root)];

    for (i, (c, s)) in spec.iter().zip(comb.iter()).enumerate() {
        previous_nodes = previous_nodes
            .iter()
            .cartesian_product(
                c.full_transitions(s)
                    .map(|t| Rc::new(t))
                    .collect::<Vec<_>>(),
            )
            .map(|((before, prev), t)| {
                let mut conflicts = vec![];
                for (i, t2) in before.iter().enumerate() {
                    if conflict(&t, t2) {
                        conflicts.push(i)
                    }
                }
                let next = g.add_node(format!("C{}({})", i, conflicts.iter().join(",")));
                g.add_edge(
                    *prev,
                    next,
                    t.iter().map(|(c, b)| (c.clone(), *b)).collect(),
                );
                (
                    before.iter().map(|v| v.clone()).chain(once(t)).collect(),
                    next,
                )
            })
            .collect();
    }
    g
}
