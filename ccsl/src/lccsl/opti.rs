use crate::lccsl::algo::{approx_conflict_map, ConflictEffect, ConflictSource};
use crate::lccsl::automata::{Label, STSBuilder, STS};
use crate::lccsl::constraints::Constraint;
use itertools::Itertools;
use num::rational::Ratio;
use permutation::{sort, Permutation};
use petgraph::algo::min_spanning_tree;
use petgraph::data::FromElements;
use petgraph::graph::{DiGraph, NodeIndex, UnGraph};
use petgraph::unionfind::UnionFind;
use petgraph::visit::{Bfs, Dfs, EdgeRef, NodeIndexable};
use petgraph::{Direction, Graph};
use std::collections::HashMap;
use std::fmt::Display;
use std::hash::Hash;
use std::ops::BitOr;

pub fn optimize_spec_by_sort<C, L>(spec: &[Constraint<C>]) -> Permutation
where
    C: Clone + Hash + Ord + Display,
    L: Clone + Eq + Hash,
    L: Label<C>,
    for<'a, 'b> &'a L: BitOr<&'b L, Output = L>,
{
    let (_, weights) = spec_weights(spec);
    sort(weights)
}

fn squished_map<C, L>(spec: &[Constraint<C>]) -> Graph<ConflictSource, ConflictEffect<Ratio<usize>>>
where
    C: Clone + Hash + Ord + Display,
    L: Clone + Eq + Hash,
    L: Label<C>,
    for<'a, 'b> &'a L: BitOr<&'b L, Output = L>,
{
    let squished_spec: Vec<STS<C, L>> = spec
        .iter()
        .map(|c| {
            Into::<STS<C, L>>::into(Into::<STSBuilder<C>>::into(c))
                .squish()
                .into()
        })
        .collect_vec();
    let comb = squished_spec.iter().map(|c| c.initial()).collect_vec();
    let conflict_map = approx_conflict_map(&squished_spec, &comb);
    conflict_map
}

fn spec_weights<C, L>(
    spec: &[Constraint<C>],
) -> (
    Graph<ConflictSource, ConflictEffect<Ratio<usize>>>,
    Vec<(Ratio<usize>, usize)>,
)
where
    C: Clone + Hash + Ord + Display,
    L: Clone + Eq + Hash,
    L: Label<C>,
    for<'a, 'b> &'a L: BitOr<&'b L, Output = L>,
{
    let conflict_map = squished_map(spec);
    let weights: Vec<(Ratio<usize>, usize)> = conflict_map
        .node_indices()
        .into_iter()
        .map(|n| {
            let count = conflict_map.edges_directed(n, Direction::Outgoing).count();
            let w = if count == 0 {
                Ratio::from(0)
            } else {
                conflict_map
                    .edges_directed(n, Direction::Outgoing)
                    .map(|e| {
                        let weight = e.weight();
                        weight.solutions
                    })
                    .sum::<Ratio<usize>>()
                    / Ratio::from(count)
            };
            (w, count)
        })
        .collect_vec();
    (conflict_map, weights)
}

fn get_components<N, E>(g: &DiGraph<N, E>) -> impl Iterator<Item = Vec<usize>> {
    let mut vertex_sets = UnionFind::new(g.node_bound());
    for edge in g.edge_references() {
        let (a, b) = (edge.source(), edge.target());

        vertex_sets.union(g.to_index(a), g.to_index(b));
    }
    let groups: HashMap<usize, Vec<usize>> = vertex_sets
        .into_labeling()
        .into_iter()
        .enumerate()
        .map(|(i, g)| (g, i))
        .into_group_map();
    groups.into_iter().map(|(_, v)| v)
}

fn split_merge_components<C, L, F>(spec: &[Constraint<C>], mapper: F) -> Permutation
where
    C: Clone + Hash + Ord + Display,
    L: Clone + Eq + Hash,
    L: Label<C>,
    F: Fn(&[Constraint<C>]) -> Permutation,
    for<'a, 'b> &'a L: BitOr<&'b L, Output = L>,
{
    let conflict_map = squished_map(spec);
    let mut spec_perm = Vec::with_capacity(spec.len());
    for comp_map in get_components(&conflict_map) {
        let comp = comp_map.iter().map(|i| spec[*i].clone()).collect_vec();
        let comp_perm = mapper(&comp);
        spec_perm.extend(comp_perm.apply_slice(comp_map));
    }

    assert_eq!(
        spec_perm.iter().copied().sum::<usize>(),
        (0..spec.len()).sum(),
        "{:?} {:?}",
        &spec_perm,
        &conflict_map
    );
    assert_eq!(
        spec_perm.len(),
        spec.len(),
        "{:?} {:?}",
        &spec_perm,
        &conflict_map
    );
    Permutation::from_vec(spec_perm)
}

pub fn optimize_by_tree_width<C, L>(spec: &[Constraint<C>]) -> Permutation
where
    C: Clone + Hash + Ord + Display,
    L: Clone + Eq + Hash,
    L: Label<C>,
    for<'a, 'b> &'a L: BitOr<&'b L, Output = L>,
{
    split_merge_components(spec, optimize_component_by_tree_width)
}

pub fn optimize_component_by_tree_width<C, L>(spec: &[Constraint<C>]) -> Permutation
where
    C: Clone + Hash + Ord + Display,
    L: Clone + Eq + Hash,
    L: Label<C>,
    for<'a, 'b> &'a L: BitOr<&'b L, Output = L>,
{
    let (map, weights) = spec_weights(spec);
    let root_idx = weights.into_iter().position_min().unwrap();

    let spanning_tree = min_spanning_tree(&map);
    let tree: UnGraph<_, _> = Graph::from_elements(spanning_tree);
    let mut bfs = Bfs::new(&tree, NodeIndex::new(root_idx));
    let mut perm = Vec::with_capacity(spec.len());
    while let Some(nx) = bfs.next(&tree) {
        perm.push(nx.index());
    }
    assert_eq!(perm.len(), spec.len(), "{:?} {:?}", &perm, &tree);
    Permutation::from_vec(perm)
}

pub fn optimize_by_tree_depth<C, L>(spec: &[Constraint<C>]) -> Permutation
where
    C: Clone + Hash + Ord + Display,
    L: Clone + Eq + Hash,
    L: Label<C>,
    for<'a, 'b> &'a L: BitOr<&'b L, Output = L>,
{
    split_merge_components(spec, optimize_component_by_tree_depth)
}

fn optimize_component_by_tree_depth<C, L>(spec: &[Constraint<C>]) -> Permutation
where
    C: Clone + Hash + Ord + Display,
    L: Clone + Eq + Hash,
    L: Label<C>,
    for<'a, 'b> &'a L: BitOr<&'b L, Output = L>,
{
    let (map, weights) = spec_weights(spec);
    let root_idx = weights.into_iter().position_min().unwrap();

    let tree: UnGraph<_, _> = Graph::from_elements(min_spanning_tree(&map));
    let mut dfs = Dfs::new(&tree, NodeIndex::new(root_idx));
    let mut perm = Vec::with_capacity(spec.len());
    while let Some(nx) = dfs.next(&tree) {
        perm.push(nx.index());
    }
    assert_eq!(perm.len(), spec.len(), "{:?} {:?}", &perm, &tree);
    Permutation::from_vec(perm)
}
