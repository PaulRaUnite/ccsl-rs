use crate::lccsl::algo::{approx_conflict_map, approx_conflict_map_undirect};
use crate::lccsl::automata::{Label, STSBuilder, STS};
use crate::lccsl::constraints::Constraint;
use itertools::Itertools;
use num::rational::Ratio;
use permutation::{sort, Permutation};
use petgraph::algo::min_spanning_tree;
use petgraph::data::FromElements;
use petgraph::graph::{NodeIndex, UnGraph};
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

pub fn squished_map<C, L>(spec: &[Constraint<C>]) -> UnGraph<usize, usize>
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
    let conflict_map = approx_conflict_map_undirect(&squished_spec, &comb);
    let mut new = Graph::with_capacity(conflict_map.node_count(), conflict_map.edge_count() / 2);
    for n in conflict_map.raw_nodes() {
        new.add_node(n.weight.transitions);
    }
    for e in conflict_map.raw_edges() {
        if new.edges_connecting(e.source(), e.target()).count() == 0 {
            new.add_edge(e.source(), e.target(), e.weight.solutions);
        }
    }
    new
}

fn spec_weights<C, L>(spec: &[Constraint<C>]) -> (UnGraph<usize, usize>, Vec<(Ratio<usize>, usize)>)
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
    let weights = conflict_map
        .node_indices()
        .into_iter()
        .map(|n| {
            let count = conflict_map.edges_directed(n, Direction::Outgoing).count();
            let w = if count == 0 {
                Ratio::from(0)
            } else {
                conflict_map
                    .edges_directed(n, Direction::Outgoing)
                    .map(|e| e.weight().solutions)
                    .sum::<Ratio<usize>>()
                    / count
            };
            (
                w * conflict_map.node_weight(n).unwrap().transitions,
                spec[n.index()].rank(),
            )
        })
        .collect_vec();
    let conflict_map = squished_map(spec);
    (conflict_map, weights)
}

fn get_components<N, E>(g: &UnGraph<N, E>) -> impl Iterator<Item = Vec<usize>> {
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

    let spanning_tree = min_spanning_tree(&map);
    let tree: UnGraph<_, _> = Graph::from_elements(spanning_tree);
    let root_idx = find_suitable_root(&map, &weights);
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

pub fn optimize_component_by_tree_depth<C, L>(spec: &[Constraint<C>]) -> Permutation
where
    C: Clone + Hash + Ord + Display,
    L: Clone + Eq + Hash,
    L: Label<C>,
    for<'a, 'b> &'a L: BitOr<&'b L, Output = L>,
{
    let (map, weights) = spec_weights(spec);
    let root_idx = find_suitable_root(&map, &weights);

    let tree: UnGraph<_, _> = Graph::from_elements(min_spanning_tree(&map));
    let mut dfs = Dfs::new(&tree, NodeIndex::new(root_idx));
    let mut perm = Vec::with_capacity(spec.len());
    while let Some(nx) = dfs.next(&tree) {
        perm.push(nx.index());
    }
    assert_eq!(perm.len(), spec.len(), "{:?} {:?}", &perm, &tree);
    Permutation::from_vec(perm)
}

pub fn optimize_component_by_tree_depth_by_root<C, L>(
    spec: &[Constraint<C>],
    root_idx: usize,
) -> Permutation
where
    C: Clone + Hash + Ord + Display,
    L: Clone + Eq + Hash,
    L: Label<C>,
    for<'a, 'b> &'a L: BitOr<&'b L, Output = L>,
{
    let (map, _) = spec_weights(spec);

    let tree: UnGraph<_, _> = Graph::from_elements(min_spanning_tree(&map));
    let mut dfs = Dfs::new(&tree, NodeIndex::new(root_idx));
    let mut perm = Vec::with_capacity(spec.len());
    while let Some(nx) = dfs.next(&tree) {
        perm.push(nx.index());
    }
    assert_eq!(perm.len(), spec.len(), "{:?} {:?}", &perm, &tree);
    Permutation::from_vec(perm)
}

fn find_suitable_root(map: &UnGraph<usize, usize>, weights: &Vec<(Ratio<usize>, usize)>) -> usize {
    let leaves = map
        .node_indices()
        .zip(weights.iter())
        .filter(|(n, _)| map.neighbors(*n).count() == 1)
        .collect_vec();
    if leaves.len() > 0 {
        leaves.iter().min_by_key(|(_, w)| *w).unwrap().0.index()
    } else {
        weights.iter().position_min().unwrap()
    }
}
