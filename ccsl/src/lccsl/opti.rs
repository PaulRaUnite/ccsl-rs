use crate::lccsl::algo::approx_conflict_map;
use crate::lccsl::automata::{STSBuilder, STS};
use crate::lccsl::constraints::Constraint;
use itertools::Itertools;
use num::rational::Ratio;
use permutation::sort;
use petgraph::Direction;
use std::fmt::Display;
use std::hash::Hash;

pub fn optimize_spec<C>(spec: &[Constraint<C>]) -> Vec<Constraint<C>>
where
    C: Clone + Hash + Ord + Display,
{
    let squished_spec: Vec<STS<C>> = spec
        .iter()
        .map(|c| {
            Into::<STS<C>>::into(Into::<STSBuilder<C>>::into(c.clone()))
                .squish()
                .into()
        })
        .collect_vec();
    let comb = squished_spec.iter().map(|c| c.initial()).collect_vec();
    let conflict_map = approx_conflict_map(&squished_spec, &comb);
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
