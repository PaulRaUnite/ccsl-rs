use std::collections::{HashMap, HashSet};
use std::fmt::Display;
use std::hash::Hash;
use std::io::Write;
use std::mem::swap;
use std::ops::{Add, BitOr};
use std::process::{Command, Stdio};

use itertools::Itertools;
use na::{DMatrix, DVector};
use num::rational::Ratio;
use num::{ToPrimitive, Zero};
use permutation::{sort, Permutation};
use petgraph::algo::{dijkstra, min_spanning_tree};
use petgraph::data::FromElements;
use petgraph::graph::{NodeIndex, UnGraph};
use petgraph::unionfind::UnionFind;
use petgraph::visit::{Bfs, Dfs, EdgeRef, NodeIndexable};
use petgraph::{Direction, Graph};

use crate::lccsl::algo::{
    squished_conflict_map, unidirect_squished_map, ConflictEffect, ConflictSource,
};
use crate::lccsl::automata::Label;
use crate::lccsl::constraints::Constraint;
use crate::lccsl::opti::root::weights_with_init;

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
        .into_iter()
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
    let conflict_map = unidirect_squished_map(spec);
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

pub fn optimize_unconnected_by_tree_width<C, L>(spec: &[Constraint<C>]) -> Permutation
where
    C: Clone + Hash + Ord + Display,
    L: Clone + Eq + Hash,
    L: Label<C>,
    for<'a, 'b> &'a L: BitOr<&'b L, Output = L>,
{
    split_merge_components(spec, optimize_by_tree_width)
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
    Permutation::from_vec(perm)
}

pub fn optimize_unconnected_by_tree_depth<C, L>(spec: &[Constraint<C>]) -> Permutation
where
    C: Clone + Hash + Ord + Display,
    L: Clone + Eq + Hash,
    L: Label<C>,
    for<'a, 'b> &'a L: BitOr<&'b L, Output = L>,
{
    split_merge_components(spec, optimize_by_tree_depth)
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
    Permutation::from_vec(perm)
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
    Permutation::from_vec(
        node_order
            .into_iter()
            .map(|(v, w)| (w, v.index()))
            .sorted_by(|(w1, _), (w2, _)| Ord::cmp(w1, w2))
            .map(|(_, v)| v)
            .collect_vec(),
    )
}

pub fn order_by_min_front(
    conflict_map: &Graph<ConflictSource, ConflictEffect<Ratio<usize>>>,
    root_idx: usize,
) -> Permutation {
    let mut perm = Vec::with_capacity(conflict_map.node_count());
    perm.push(root_idx);

    let mut edges = conflict_map
        .raw_edges()
        .into_iter()
        .map(|e| (e.source().index(), e.target().index(), e.weight.solutions))
        .filter(|(_, t, _)| t != &root_idx)
        .sorted_by_key(|(_, _, w)| *w)
        .collect_vec();
    let mut temp = Vec::with_capacity(edges.len());
    let mut visited = HashSet::new();
    visited.insert(root_idx);

    while edges.len() > 0 {
        let next = edges
            .iter()
            .filter(|(s, _, _)| visited.contains(s))
            .next()
            .expect("failed to get next vertex: not connected?")
            .1;
        temp.clear();
        temp.extend(edges.iter().filter(|(_, t, _)| t != &next).copied());
        swap(&mut edges, &mut temp);
        perm.push(next);
        visited.insert(next);
    }
    Permutation::from_vec(perm)
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

pub fn optimize_dijkstra_with_networkx_root<C, L>(spec: &[Constraint<C>]) -> Permutation
where
    C: Clone + Hash + Ord + Display,
    L: Clone + Eq + Hash,
    L: Label<C>,
    for<'a, 'b> &'a L: BitOr<&'b L, Output = L>,
{
    let conflict_map = squished_conflict_map(spec);
    let root_idx = networkx_root(&conflict_map);
    order_via_dijkstra(&conflict_map, root_idx)
}

pub fn networkx_root(conflict_map: &Graph<ConflictSource, ConflictEffect<Ratio<usize>>>) -> usize {
    let mut proc = Command::new("bash")
        .arg("/home/paulra/Code/ccsl-rs/rooter/run.sh")
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .spawn()
        .expect("failed to spawn rooter");
    let stdin = proc.stdin.as_mut().expect("failed to get stdin");
    let edge_count = conflict_map.edge_count();
    for (i, e) in conflict_map.raw_edges().into_iter().enumerate() {
        write!(
            stdin,
            "{} {} {{'weight':{}}}",
            e.source().index(),
            e.target().index(),
            e.weight.solutions.to_f64().expect("failed to get f64")
        )
        .expect("failed to write edge");
        if i != (edge_count - 1) {
            write!(stdin, "\n").expect("failed to write newline");
        }
    }

    let output = proc.wait_with_output().expect("failed to wait for process");

    if !output.status.success() {
        panic!("command executed with failing error code");
    }
    let contents = String::from_utf8(output.stdout).expect("failed to decode utf8");
    let root = contents
        .strip_suffix("\n")
        .unwrap()
        .parse()
        .expect("failed to parse usize from command");
    assert!(root < conflict_map.node_count());
    root
}

pub fn heatmap_root(conflict_map: &Graph<ConflictSource, ConflictEffect<Ratio<usize>>>) -> usize {
    let n = conflict_map.node_count();
    let weights = DVector::from_vec(
        conflict_map
            .raw_nodes()
            .into_iter()
            .map(|n| Ratio::from_integer(n.weight.transitions))
            .collect(),
    );
    let mut adjacency = DMatrix::zeros(n, n);
    for e in conflict_map.raw_edges() {
        let i = e.source().index();
        let j = e.target().index();
        adjacency[(i, j)] = e.weight.solutions;
    }
    let mut res = DMatrix::identity(n, n);
    for _ in 1..n {
        res *= &adjacency;
    }
    let weights = res * weights;
    weights.argmax().0
}

pub fn optimize_dijkstra_with_heatmap_root<C, L>(spec: &[Constraint<C>]) -> Permutation
where
    C: Clone + Hash + Ord + Display,
    L: Clone + Eq + Hash,
    L: Label<C>,
    for<'a, 'b> &'a L: BitOr<&'b L, Output = L>,
{
    let conflict_map = squished_conflict_map(spec);
    let root = heatmap_root(&conflict_map);
    order_via_dijkstra(&conflict_map, root)
}
//
// pub fn find_root_from_topology<C, L>(spec: &[Constraint<C>]) -> usize
// where
//     C: Clone + Hash + Ord + Display,
//     L: Clone + Eq + Hash,
//     L: Label<C>,
//     for<'a, 'b> &'a L: BitOr<&'b L, Output = L>,
// {
//     let conflict_map = squished_map(spec);
//     let (nodes, edges) = (conflict_map.raw_nodes(), conflict_map.raw_edges());
//
//     // init
//     let root = weights_by_min_outgoing(spec)
//         .into_iter()
//         .position_min()
//         .expect("failed to get min");
//     0
// }
//
pub fn root_by_tricost(
    conflict_map: &Graph<ConflictSource, ConflictEffect<Ratio<usize>>>,
) -> usize {
    if conflict_map.node_count() <= 2 {
        0
    } else {
        conflict_map
            .raw_nodes()
            .into_iter()
            .enumerate()
            .map(|(i, n)| {
                let edge1 = conflict_map
                    .edges_directed(NodeIndex::new(i), Direction::Outgoing)
                    .into_iter()
                    .min_by_key(|e| e.weight().solutions)
                    .expect("no edges");
                let edge2 = conflict_map
                    .edges_directed(NodeIndex::new(i), Direction::Outgoing)
                    .into_iter()
                    .chain(
                        conflict_map
                            .edges_directed(edge1.target(), Direction::Outgoing)
                            .into_iter(),
                    )
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

    use crate::lccsl::algo::{ConflictEffect, ConflictSource};
    use rand::{Rng, SeedableRng};

    pub fn weights_with_init(
        conflict_map: &Graph<ConflictSource, ConflictEffect<Ratio<usize>>>,
    ) -> usize {
        let weights = conflict_map
            .node_indices()
            .into_iter()
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
