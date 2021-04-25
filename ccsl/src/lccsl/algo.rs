use std::collections::{BTreeMap, HashMap};
use std::fmt;
use std::fmt::{Debug, Formatter};
use std::hash::Hash;

use itertools::Itertools;
use num::{One, Rational64};
use petgraph::prelude::EdgeRef;
use petgraph::{Direction, Graph};

use crate::lccsl::automata::{Delta, Guard, LabeledTransitionSystem, State, Transition, STS};
use crate::lccsl::expressions::BooleanExpression;

pub fn conflict<C: Eq + Hash>(m1: &HashMap<&C, bool>, m2: &HashMap<&C, bool>) -> bool {
    for (c, b) in m1 {
        if m2.get(c).map_or(false, |v| v ^ b) {
            return true;
        }
    }
    false
}

#[derive(Debug, Copy, Clone)]
pub struct ConflictRatio {
    pub solutions: Rational64,
    pub all: usize,
}

impl fmt::Display for ConflictRatio {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let (nom, denom) = self.solutions.into();
        write!(f, "{}/{} ({})", nom, denom, self.all)
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

pub fn conflict_map_combinations<C>(spec: &[STS<C>]) -> Vec<Graph<ConflictSource, ConflictRatio>>
where
    C: Ord + Hash + Clone,
{
    spec.iter()
        .map(|sts| sts.states().iter())
        .multi_cartesian_product()
        .map(|comb: Vec<&State<BooleanExpression<Delta<C>>>>| conflict_map(spec, &comb))
        .collect()
}

fn conflict_map<'a, C>(
    spec: &'a [STS<C>],
    comb: &[&State<BooleanExpression<Delta<C>>>],
) -> Graph<ConflictSource, ConflictRatio>
where
    C: Clone + Hash + Ord,
{
    let mut index: HashMap<&C, Vec<(usize, &STS<C>)>> = HashMap::new();
    for (i, constraint) in spec.iter().enumerate() {
        for clock in constraint.clocks() {
            index
                .entry(clock)
                .or_insert_with(|| vec![])
                .push((i, constraint));
        }
    }

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
    for (clock, constraints) in index.into_iter() {
        for ((i1, c1), (i2, c2)) in constraints.into_iter().tuple_combinations::<(_, _)>() {
            let (solutions, all) =
                count_conflict(c1.transitions(comb[i1]), c2.transitions(comb[i2]));
            let n1 = nodes[i1];
            let n2 = nodes[i2];
            let sol_len1 = c1.transitions_len(comb[i1]);
            let sol_len2 = c2.transitions_len(comb[i2]);
            g.add_edge(
                n1,
                n2,
                ConflictRatio {
                    solutions: Rational64::new(solutions as i64, sol_len1 as i64),
                    all: sol_len2,
                },
            );
            g.add_edge(
                n2,
                n1,
                ConflictRatio {
                    solutions: Rational64::new(solutions as i64, sol_len2 as i64),
                    all: sol_len1,
                },
            );
        }
    }
    g
}

fn count_conflict<'a, C: 'a, G>(
    m1: impl Iterator<Item = Transition<'a, C, G>>,
    m2: impl Iterator<Item = Transition<'a, C, G>> + Clone,
) -> (usize, usize)
where
    C: Eq + Hash + Clone + Ord,
    G: Clone,
{
    let (mut solutions, mut all): (usize, usize) = (0, 0);
    for has_conflict in m1
        .into_iter()
        .cartesian_product(m2.into_iter())
        .map(|(x, y)| x.label.has_conflict(&y.label))
    {
        solutions += if has_conflict { 0 } else { 1 };
        all += 1;
    }
    (solutions, all)
}

#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq, Ord, PartialOrd)]
pub enum SearchActions {
    Test,
    Down,
    Up,
}

fn dummy_visitor(_: SearchActions, _: usize) {}
fn logging_visitor(a: SearchActions, v: usize) {
    println!("{:?}: {}", a, v);
}
pub struct CountingVisitor {
    data: BTreeMap<SearchActions, usize>,
    solutions: usize,
}

impl CountingVisitor {
    pub fn new() -> Self {
        Self {
            data: BTreeMap::new(),
            solutions: 0,
        }
    }

    pub fn count(&mut self, a: SearchActions, s: usize) {
        let counter = self.data.entry(a).or_insert(0);
        *counter += 1;
        self.solutions = s;
    }
}

impl fmt::Display for CountingVisitor {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{:?} {}", self.data, self.solutions)
    }
}

pub fn find_solutions<'a: 'b, 'b, C, G: 'b, D>(
    spec: &'a [LabeledTransitionSystem<C, D, G>],
    states: &'b [&State<G>],
    visitor: Option<&mut dyn FnMut(SearchActions, usize) -> ()>,
) -> usize
where
    C: Clone + Eq + Hash + Ord,
    G: Guard<D> + Default,
{
    rec_solutions(
        spec,
        states,
        visitor.unwrap_or(&mut dummy_visitor),
        HashMap::with_capacity(spec.iter().map(|c| c.clocks().len()).sum()),
    )
    .unwrap_or(0)
}

fn rec_solutions<'a, 'b, C, G: 'b, D>(
    spec: &'a [LabeledTransitionSystem<C, D, G>],
    states: &'b [&State<G>],
    visitor: &mut dyn FnMut(SearchActions, usize) -> (),
    applied: HashMap<C, bool>,
) -> Option<usize>
where
    C: Clone + Eq + Hash + Ord,
    G: Guard<D> + Default,
{
    let (sts, spec) = spec.split_first()?;
    let (state, states) = states.split_first()?;
    let solutions: usize = sts
        .transitions(state)
        .map(|t| {
            visitor(SearchActions::Test, 0);
            if !t.label.has_conflict_with_map(&applied) {
                visitor(SearchActions::Down, 0);
                rec_solutions(
                    spec,
                    states,
                    visitor,
                    applied
                        .iter()
                        .map(|(c, b)| (c.clone(), *b))
                        .chain(t.label.clocks.iter().map(|c| (c.clone(), false)))
                        .chain(t.label.present.into_iter().map(|c| (c.clone(), true)))
                        .collect(),
                )
                .unwrap_or(1)
            } else {
                0
            }
        })
        .sum();
    visitor(SearchActions::Up, solutions);
    Some(solutions)
}

pub fn approximate_complexity(
    g: &Graph<ConflictSource, ConflictRatio>,
) -> Option<(Rational64, Rational64)> {
    let mut selected: HashMap<_, (Rational64, Rational64)> = HashMap::new();
    let mut solution_amount = Rational64::one();
    let mut all_amount = Rational64::one();
    for to in g.node_indices() {
        let (solution, all) = g
            .edges_directed(to, Direction::Incoming)
            .map(|e| {
                selected
                    .get(&e.source())
                    .map(|(s, a)| (s * e.weight().solutions, a * (e.weight().all as i64)))
            })
            .flatten()
            .min_by_key(|(s, _)| *s)
            .unwrap_or_else(|| {
                let t = Rational64::new(g.node_weight(to).unwrap().transitions as i64, 1);
                (solution_amount * t, all_amount * t)
            });
        selected.insert(to, (solution, all));
        solution_amount = solution;
        all_amount = all;
    }
    g.node_indices()
        .last()
        .map(|n| selected.get(&n))
        .flatten()
        .map(|v| *v)
}

pub fn compare_approx_and_solutions<C>(spec: &[STS<C>]) -> Vec<(usize, CountingVisitor, Rational64)>
where
    C: Ord + Hash + Clone,
{
    spec.iter()
        .map(|sts| sts.states().iter())
        .multi_cartesian_product()
        .map(|comb: Vec<&State<BooleanExpression<Delta<C>>>>| {
            let mut visitor = CountingVisitor::new();
            (
                find_solutions(spec, &comb, Some(&mut |a, s| visitor.count(a, s))),
                visitor,
                approximate_complexity(&conflict_map(spec, &comb))
                    .unwrap()
                    .0,
            )
        })
        .collect()
}
