use std::cmp::Ordering;
use std::collections::HashSet;
use std::fmt;
use std::fmt::{Debug, Formatter};
use std::hash::Hash;
use std::ops::BitOr;

use itertools::Itertools;
use num::rational::Ratio;
use petgraph::prelude::EdgeRef;
use petgraph::{Direction, Graph};

use crate::lccsl::automata::{Label, MergedTransition, StateRef, STS};

#[derive(Debug, Copy, Clone)]
pub struct ConflictEffect<R> {
    pub solutions: R,
    pub all: usize,
}

impl<R: PartialEq> PartialEq for ConflictEffect<R> {
    fn eq(&self, other: &Self) -> bool {
        self.solutions.eq(&other.solutions)
    }
}
impl<R: PartialOrd> PartialOrd for ConflictEffect<R> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.solutions.partial_cmp(&other.solutions)
    }
}

impl<R: fmt::Display> fmt::Display for ConflictEffect<R> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{} ({})", self.solutions, self.all)
    }
}

#[derive(Debug, Clone)]
pub struct ConflictSource {
    pub name: String,
    pub transitions: usize,
}

impl fmt::Display for ConflictSource {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{} ({})", self.name, self.transitions)
    }
}

pub fn approx_conflict_map<C, L>(
    spec: &[STS<C, L>],
    comb: &[StateRef],
) -> Graph<ConflictSource, ConflictEffect<Ratio<usize>>>
where
    C: Clone + Hash + Ord,
    L: Label<C>,
{
    generic_conflict_map(spec, comb, approx_solutions_edger)
}

pub fn approx_conflict_map_undirect<C, L>(
    spec: &[STS<C, L>],
    comb: &[StateRef],
) -> Graph<ConflictSource, ConflictEffect<usize>>
where
    C: Clone + Hash + Ord,
    L: Label<C>,
{
    generic_conflict_map(spec, comb, approx_solutions_edger_undirect)
}

pub fn limit_conflict_map<C, L>(
    spec: &[STS<C, L>],
    comb: &[StateRef],
) -> Graph<ConflictSource, ConflictEffect<usize>>
where
    C: Clone + Hash + Ord,
    L: Label<C>,
{
    generic_conflict_map(spec, comb, limit_solutions_edger)
}

pub fn generic_conflict_map<C, L, R, F>(
    spec: &[STS<C, L>],
    comb: &[StateRef],
    edger: F,
) -> Graph<ConflictSource, ConflictEffect<R>>
where
    C: Clone + Hash + Ord,
    L: Label<C>,
    F: Fn(&STS<C, L>, &STS<C, L>, StateRef, StateRef) -> (ConflictEffect<R>, ConflictEffect<R>),
{
    let mut g = Graph::new();
    let nodes: Vec<_> = spec
        .iter()
        .enumerate()
        .map(|(i, c)| {
            g.add_node(ConflictSource {
                name: (&c).to_string(),
                transitions: c.transitions_len(comb[i]),
            })
        })
        .collect();
    for ((i1, c1), (i2, c2)) in spec.iter().enumerate().tuple_combinations::<(_, _)>() {
        let n1 = nodes[i1];
        let n2 = nodes[i2];
        let (direct, backwards) = edger(c1, c2, comb[i1], comb[i2]);
        g.add_edge(n1, n2, direct);
        g.add_edge(n2, n1, backwards);
    }
    g
}

fn limit_solutions_edger<C, L>(
    left: &STS<C, L>,
    right: &STS<C, L>,
    left_state: StateRef,
    right_state: StateRef,
) -> (ConflictEffect<usize>, ConflictEffect<usize>)
where
    C: Eq + Hash + Clone + Ord,
    L: Label<C>,
{
    let sol_len1 = left.transitions_len(left_state);
    let sol_len2 = right.transitions_len(right_state);

    let direct = ConflictEffect {
        solutions: limit_solutions(left.transitions(left_state), right.transitions(right_state)),
        all: sol_len2,
    };
    let backward = ConflictEffect {
        solutions: limit_solutions(right.transitions(right_state), left.transitions(left_state)),
        all: sol_len1,
    };
    (direct, backward)
}

fn limit_solutions<'a, C, L>(
    m1: impl Iterator<Item = MergedTransition<'a, C, L>>,
    m2: impl Iterator<Item = MergedTransition<'a, C, L>> + Clone,
) -> usize
where
    C: Eq + Hash + Clone + Ord + 'a,
    L: Label<C> + 'a,
{
    m1.into_iter()
        .map(|x| {
            m2.clone()
                .into_iter()
                .map(move |y| if x.label.has_conflict(&y.label) { 0 } else { 1 })
                .sum()
        })
        .max()
        .unwrap_or(0)
}

fn approx_solutions_edger<C, L>(
    left: &STS<C, L>,
    right: &STS<C, L>,
    left_state: StateRef,
    right_state: StateRef,
) -> (ConflictEffect<Ratio<usize>>, ConflictEffect<Ratio<usize>>)
where
    C: Eq + Hash + Clone + Ord,
    L: Label<C>,
{
    let sol_len1 = left.transitions_len(left_state);
    let sol_len2 = right.transitions_len(right_state);
    let approx = approx_solutions(left.transitions(left_state), right.transitions(right_state));

    let direct = ConflictEffect {
        solutions: Ratio::new(approx, sol_len1),
        all: sol_len2,
    };
    let backward = ConflictEffect {
        solutions: Ratio::new(approx, sol_len2),
        all: sol_len1,
    };
    (direct, backward)
}

fn approx_solutions_edger_undirect<C, L>(
    left: &STS<C, L>,
    right: &STS<C, L>,
    left_state: StateRef,
    right_state: StateRef,
) -> (ConflictEffect<usize>, ConflictEffect<usize>)
where
    C: Eq + Hash + Clone + Ord,
    L: Label<C>,
{
    let sol_len1 = left.transitions_len(left_state);
    let sol_len2 = right.transitions_len(right_state);
    let approx = approx_solutions(left.transitions(left_state), right.transitions(right_state));

    let edge = ConflictEffect {
        solutions: approx,
        all: sol_len1 * sol_len2,
    };
    (edge, edge)
}

fn approx_solutions<'a, C, L>(
    m1: impl Iterator<Item = MergedTransition<'a, C, L>>,
    m2: impl Iterator<Item = MergedTransition<'a, C, L>> + Clone,
) -> usize
where
    C: Eq + Hash + Clone + Ord + 'a,
    L: Label<C> + 'a,
{
    m1.into_iter()
        .map(|x| {
            m2.clone()
                .into_iter()
                .map(move |y| if x.label.has_conflict(&y.label) { 0 } else { 1 })
                .sum::<usize>()
        })
        .sum()
}

pub struct CountingVisitor {
    pub test: usize,
    pub down: usize,
    pub solutions: usize,
}

pub trait Visitor<C> {
    fn test(&mut self);
    fn down(&mut self);
    fn solution(&mut self);
}

impl CountingVisitor {
    pub fn new() -> Self {
        Self {
            test: 0,
            down: 0,
            solutions: 0,
        }
    }
}

impl<C> Visitor<C> for CountingVisitor {
    fn test(&mut self) {
        self.test += 1;
    }

    fn down(&mut self) {
        self.down += 1;
    }

    fn solution(&mut self) {
        self.solutions += 1;
    }
}

impl fmt::Display for CountingVisitor {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "tests: {} downs: {} solutions: {}",
            self.test, self.down, self.solutions
        )
    }
}

struct DummyVisitor;

impl<C> Visitor<C> for DummyVisitor {
    fn test(&mut self) {}

    fn down(&mut self) {}

    fn solution(&mut self) {}
}

pub fn find_solutions<'a: 'b, 'b, C, L>(
    spec: &'a [STS<C, L>],
    states: &'b [StateRef],
    visitor: Option<&mut dyn Visitor<C>>,
) -> usize
where
    C: Clone + Eq + Hash + Ord,
    L: Label<C>,
    for<'c, 'd> &'c L: BitOr<&'d L, Output = L>,
{
    inline_recursive_solutions(
        spec,
        states,
        visitor.unwrap_or(&mut DummyVisitor),
        L::with_capacity_hint(spec.iter().flat_map(|c| c.clocks()).unique().count()),
    )
    .unwrap_or(0)
}

fn inline_recursive_solutions<'a, 'b, C, L>(
    spec: &'a [STS<C, L>],
    states: &'b [StateRef],
    visitor: &mut dyn Visitor<C>,
    applied: L,
) -> Option<usize>
where
    C: Clone + Eq + Hash + Ord,
    L: Label<C>,
    for<'c, 'd> &'c L: BitOr<&'d L, Output = L>,
{
    let (sts, spec) = spec.split_first()?;
    let (state, states) = states.split_first()?;
    let solutions: usize = sts
        .transitions(*state)
        .map(|t| {
            visitor.test();
            if !t.label.has_conflict(&applied) {
                visitor.down();
                inline_recursive_solutions(spec, states, visitor, &applied | t.label)
                    .unwrap_or_else(|| {
                        visitor.solution();
                        1
                    })
            } else {
                0
            }
        })
        .sum();
    Some(solutions)
}

#[derive(Debug, Default, Copy, Clone)]
pub struct Complexity<R> {
    pub solutions: R,
    pub all: usize,
    pub downs: R,
    pub tests: R,
}

impl<R: fmt::Debug> fmt::Display for Complexity<R> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

pub fn complexity_from_graph<R>(g: &Graph<ConflictSource, ConflictEffect<R>>) -> Complexity<R>
where
    R: num::Num + Copy + Clone + From<usize> + Ord,
{
    if g.node_count() == 0 {
        return Complexity::<R> {
            solutions: R::zero(),
            all: 0,
            downs: R::zero(),
            tests: R::zero(),
        };
    }
    let mut selected: HashSet<_> = HashSet::new();
    let mut aprox = Complexity::<R> {
        solutions: R::one(),
        all: 1,
        downs: R::zero(),
        tests: R::zero(),
    };
    for to in g.node_indices() {
        let (solution, all): (R, usize) = g
            .edges_directed(to, Direction::Incoming)
            .filter_map(|e| {
                if selected.contains(&e.source()) {
                    Some((
                        aprox.solutions * e.weight().solutions,
                        aprox.all * e.weight().all,
                    ))
                } else {
                    None
                }
            })
            .min_by_key(|(s, _)| *s)
            .unwrap_or_else(|| {
                let t = g.node_weight(to).unwrap().transitions;
                (aprox.solutions * t.into(), aprox.all * t)
            });
        selected.insert(to);
        aprox.downs = aprox.downs + solution;
        aprox.tests = aprox.tests + aprox.solutions * g.node_weight(to).unwrap().transitions.into();
        aprox.solutions = solution;
        aprox.all = all;
    }
    aprox
}

pub fn generate_combinations<'a, C, L>(
    spec: &'a [STS<C, L>],
) -> impl Iterator<Item = Vec<StateRef>> + 'a + Send
where
    C: Ord + Hash + Clone,
{
    spec.iter()
        .map(|sts| sts.states())
        .multi_cartesian_product()
}
