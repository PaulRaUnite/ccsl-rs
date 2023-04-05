// TODO: better function naming
use std::collections::HashSet;
use std::fmt::Display;
use std::hash::Hash;
use std::mem::swap;
use std::ops::{Add, BitOr};

use itertools::Itertools;
use num::rational::Ratio;
use num::Zero;
use permutation::{sort, Permutation};
use petgraph::algo::{dijkstra, min_spanning_tree};
use petgraph::data::FromElements;
use petgraph::graph::{NodeIndex, UnGraph};
use petgraph::visit::{Bfs, Dfs, EdgeRef};
use petgraph::{Direction, Graph};

use crate::algo::{squished_conflict_map, unidirect_squished_map, ConflictEffect, ConflictSource};
use crate::kernel::automata::label::Label;
use crate::kernel::constraints::Constraint;
use crate::optimization::root::weights_with_init;

pub fn optimize_by_sort_weights<C, L>(spec: &[Constraint<C>]) -> Permutation
where
    C: Clone + Hash + Ord + Display,
    L: Clone + Eq + Hash,
    L: Label<C>,
    for<'a, 'b> &'a L: BitOr<&'b L, Output = L>,
{
    let map = squished_conflict_map(spec);
    let weights = weights_from_min_outgoing(&map);
    sort(weights)
}

fn weights_from_min_outgoing(
    conflict_map: &Graph<ConflictSource, ConflictEffect<Ratio<usize>>>,
) -> Vec<Ratio<usize>> {
    let weights = conflict_map
        .node_indices()
        .map(|n| {
            conflict_map
                .edges_directed(n, Direction::Outgoing)
                .map(|e| e.weight().solutions)
                .min()
                .unwrap_or(Ratio::from(usize::MAX))
        })
        .collect_vec();
    weights
}

pub fn root_by_min_outgoing(
    conflict_map: &Graph<ConflictSource, ConflictEffect<Ratio<usize>>>,
) -> usize {
    weights_from_min_outgoing(conflict_map)
        .iter()
        .position_min()
        .unwrap()
}

pub fn optimize_by_tree_width<C, L>(spec: &[Constraint<C>]) -> Permutation
where
    C: Clone + Hash + Ord + Display,
    L: Clone + Eq + Hash,
    L: Label<C>,
    for<'a, 'b> &'a L: BitOr<&'b L, Output = L>,
{
    let map = squished_conflict_map(spec);
    let root_idx = root_by_min_outgoing(&map);

    let map = unidirect_squished_map(spec);
    let spanning_tree = min_spanning_tree(&map);
    let tree: UnGraph<_, _> = Graph::from_elements(spanning_tree);

    let mut bfs = Bfs::new(&tree, NodeIndex::new(root_idx));
    let mut perm = Vec::with_capacity(spec.len());
    while let Some(nx) = bfs.next(&tree) {
        perm.push(nx.index());
    }
    assert_eq!(perm.len(), spec.len(), "{:?} {:?}", &perm, &tree);
    Permutation::oneline(perm).inverse()
}

pub fn optimize_by_tree_depth<C, L>(spec: &[Constraint<C>]) -> Permutation
where
    C: Clone + Hash + Ord + Display,
    L: Clone + Eq + Hash,
    L: Label<C>,
    for<'a, 'b> &'a L: BitOr<&'b L, Output = L>,
{
    let map = squished_conflict_map(spec);
    let root_idx = root_by_min_outgoing(&map);

    optimize_component_by_tree_depth_by_root(spec, root_idx)
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
    let map = unidirect_squished_map(spec);

    let tree: UnGraph<_, _> = Graph::from_elements(min_spanning_tree(&map));
    let mut dfs = Dfs::new(&tree, NodeIndex::new(root_idx));
    let mut perm = Vec::with_capacity(spec.len());
    while let Some(nx) = dfs.next(&tree) {
        perm.push(nx.index());
    }
    assert_eq!(perm.len(), spec.len(), "{:?} {:?}", &perm, &tree);
    Permutation::oneline(perm).inverse()
}

pub fn optimize_by_min_front_init_weights<C, L>(spec: &[Constraint<C>]) -> Permutation
where
    C: Clone + Hash + Ord + Display,
    L: Clone + Eq + Hash,
    L: Label<C>,
    for<'a, 'b> &'a L: BitOr<&'b L, Output = L>,
{
    let conflict_map = squished_conflict_map(spec);
    let root_idx = weights_with_init(&conflict_map);

    order_by_min_front(&conflict_map, root_idx)
}

pub fn optimize_by_min_front_init_weights_root<C, L>(
    spec: &[Constraint<C>],
    root: usize,
) -> Permutation
where
    C: Clone + Hash + Ord + Display,
    L: Clone + Eq + Hash,
    L: Label<C>,
    for<'a, 'b> &'a L: BitOr<&'b L, Output = L>,
{
    let conflict_map = squished_conflict_map(spec);

    order_by_min_front(&conflict_map, root)
}

pub fn order_via_dijkstra(
    conflict_map: &Graph<ConflictSource, ConflictEffect<Ratio<usize>>>,
    root_idx: usize,
) -> Permutation {
    let node_order = dijkstra(&conflict_map, NodeIndex::new(root_idx), None, |e| {
        Weight(e.weight().solutions)
    });
    Permutation::oneline(
        node_order
            .into_iter()
            .map(|(v, w)| (w, v.index()))
            .sorted_by(|(w1, _), (w2, _)| Ord::cmp(w1, w2))
            .map(|(_, v)| v)
            .collect_vec(),
    )
    .inverse()
}

pub fn order_by_min_front(
    conflict_map: &Graph<ConflictSource, ConflictEffect<Ratio<usize>>>,
    root_idx: usize,
) -> Permutation {
    let mut perm = Vec::with_capacity(conflict_map.node_count());
    perm.push(root_idx);

    let mut edges = conflict_map
        .raw_edges()
        .iter()
        .map(|e| (e.source().index(), e.target().index(), e.weight.solutions))
        .filter(|(_, t, _)| t != &root_idx)
        .sorted_by_key(|(_, _, w)| *w)
        .collect_vec();
    let mut temp = Vec::with_capacity(edges.len());
    let mut visited = HashSet::new();
    visited.insert(root_idx);

    while !edges.is_empty() {
        let next = edges
            .iter()
            .find(|(s, _, _)| visited.contains(s))
            .expect("failed to get next vertex: not connected?")
            .1;
        temp.clear();
        temp.extend(edges.iter().filter(|(_, t, _)| t != &next).copied());
        swap(&mut edges, &mut temp);
        perm.push(next);
        visited.insert(next);
    }
    Permutation::oneline(perm).inverse()
}

#[derive(Debug, Copy, Clone, PartialOrd, Ord, PartialEq, Eq)]
struct Weight(Ratio<usize>);

impl Add for Weight {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Self(self.0.add(rhs.0))
    }
}

impl Default for Weight {
    fn default() -> Self {
        Self(Ratio::zero())
    }
}

pub fn root_by_tricost(
    conflict_map: &Graph<ConflictSource, ConflictEffect<Ratio<usize>>>,
) -> usize {
    if conflict_map.node_count() <= 2 {
        0
    } else {
        conflict_map
            .raw_nodes()
            .iter()
            .enumerate()
            .map(|(i, n)| {
                let edge1 = conflict_map
                    .edges_directed(NodeIndex::new(i), Direction::Outgoing)
                    .min_by_key(|e| e.weight().solutions)
                    .expect("no edges");
                let edge2 = conflict_map
                    .edges_directed(NodeIndex::new(i), Direction::Outgoing)
                    .chain(conflict_map.edges_directed(edge1.target(), Direction::Outgoing))
                    .filter(|e| e.target() != edge1.source() && e.target() != edge1.target())
                    .min_by_key(|e| e.weight().solutions)
                    .expect("no more edges");
                let second_node = conflict_map.node_weight(edge1.target()).unwrap();
                let third_node = conflict_map.node_weight(edge2.target()).unwrap();
                let solutions = n.weight.transitions;
                let complexity_1 = solutions * second_node.transitions;
                let solutions_1 = edge1.weight().solutions * solutions;
                let complexity_2 = solutions_1 * third_node.transitions + complexity_1;
                let solutions_2 = edge2.weight().solutions * solutions_1;
                // println!(
                //     "{} ({}) ->({};{}) {} ({}) -> ({};{}) {} ({})",
                //     i,
                //     n.weight.transitions,
                //     solutions_1,
                //     complexity_1,
                //     edge1.target().index(),
                //     second_node.transitions,
                //     complexity_2,
                //     solutions_2,
                //     edge2.target().index(),
                //     third_node.transitions
                // );
                (i, (solutions_2, complexity_2))
            })
            .min_by_key(|(_, w)| *w)
            .unwrap()
            .0
    }
}

pub fn optimize_by_min_front_with_tricost_root<C, L>(spec: &[Constraint<C>]) -> Permutation
where
    C: Clone + Hash + Ord + Display,
    L: Clone + Eq + Hash,
    L: Label<C>,
    for<'a, 'b> &'a L: BitOr<&'b L, Output = L>,
{
    let conflict_map = squished_conflict_map(spec);
    let root = root_by_tricost(&conflict_map);

    order_by_min_front(&conflict_map, root)
}

pub type Rooter = dyn Fn(&Graph<ConflictSource, ConflictEffect<Ratio<usize>>>) -> usize;
pub type Orderer =
    dyn Fn(&Graph<ConflictSource, ConflictEffect<Ratio<usize>>>, usize) -> Permutation;

pub fn optimize<C, L>(spec: &[Constraint<C>], rooter: &Rooter, orderer: &Orderer) -> Permutation
where
    C: Clone + Hash + Ord + Display,
    L: Clone + Eq + Hash,
    L: Label<C>,
    for<'a, 'b> &'a L: BitOr<&'b L, Output = L>,
{
    let map = squished_conflict_map::<C, L>(spec);
    let root = rooter(&map);
    orderer(&map, root)
}

pub mod root {
    use itertools::Itertools;
    use num::rational::Ratio;
    use num::Zero;
    use petgraph::{Direction, Graph};

    use crate::algo::{ConflictEffect, ConflictSource};
    use rand::{Rng, SeedableRng};

    pub fn weights_with_init(
        conflict_map: &Graph<ConflictSource, ConflictEffect<Ratio<usize>>>,
    ) -> usize {
        let weights = conflict_map
            .node_indices()
            .map(|n| {
                let count = conflict_map.edges_directed(n, Direction::Outgoing).count();
                let source_transitions = conflict_map.node_weight(n).unwrap().transitions;
                if count == 0 {
                    (source_transitions.into(), Ratio::zero())
                } else {
                    let (edge, sol) = conflict_map
                        .edges_directed(n, Direction::Outgoing)
                        .map(|e| (e, e.weight().solutions * source_transitions))
                        .min_by_key(|(_, w)| *w)
                        .unwrap();
                    (
                        sol,
                        if count > 1 {
                            conflict_map
                                .edges_directed(n, Direction::Outgoing)
                                .filter(|e| e != &edge)
                                .map(|e| e.weight().solutions)
                                .sum::<Ratio<usize>>()
                                / Ratio::new(count - 1, 1)
                        } else {
                            Ratio::zero()
                        },
                    )
                }
            })
            .collect_vec();
        weights.iter().position_min().unwrap()
    }

    pub fn random(conflict_map: &Graph<ConflictSource, ConflictEffect<Ratio<usize>>>) -> usize {
        rand::rngs::StdRng::from_entropy().gen_range(0..conflict_map.node_count())
    }
}

pub mod order {}
