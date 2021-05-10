use crate::lccsl::algo::approx_conflict_map;
use crate::lccsl::automata::STS;
use itertools::Itertools;
use num::rational::Ratio;
use permutation::sort;
use petgraph::Direction;
use std::hash::Hash;

pub fn optimize_spec<C: Clone + Hash + Ord>(spec: &[STS<C>]) -> Vec<STS<C>> {
    let squished_spec = spec.iter().map(|c| c.clone().squish()).collect_vec();
    let comb = squished_spec
        .iter()
        .map(|c| c.states().iter().next().unwrap())
        .collect_vec();
    let conflict_map = approx_conflict_map(spec, &comb);
    let weights: Vec<Ratio<usize>> = conflict_map
        .node_indices()
        .into_iter()
        .map(|n| {
            conflict_map
                .edges_directed(n, Direction::Outgoing)
                .map(|e| {
                    let weight = e.weight();
                    Ratio::from(weight.all) - weight.solutions
                })
                .sum::<Ratio<usize>>()
                / Ratio::from(conflict_map.edges_directed(n, Direction::Outgoing).count())
        })
        .collect_vec();
    sort(weights).apply_slice(spec)
}
