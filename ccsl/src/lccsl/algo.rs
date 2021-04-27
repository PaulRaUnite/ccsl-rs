use std::collections::{HashMap, HashSet};
use std::fmt;
use std::fmt::{Debug, Formatter};
use std::hash::Hash;

use itertools::Itertools;
use petgraph::prelude::EdgeRef;
use petgraph::{Direction, Graph};

use crate::lccsl::automata::{Delta, Guard, LabeledTransitionSystem, MergedTransition, State, STS};
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
    pub solutions: usize,
    pub all: usize,
}

impl fmt::Display for ConflictRatio {
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
    for (_, constraints) in index.into_iter() {
        for ((i1, c1), (i2, c2)) in constraints.into_iter().tuple_combinations::<(_, _)>() {
            let solutions = count_solutions(c1.transitions(comb[i1]), c2.transitions(comb[i2]));
            let n1 = nodes[i1];
            let n2 = nodes[i2];
            let sol_len1 = c1.transitions_len(comb[i1]);
            let sol_len2 = c2.transitions_len(comb[i2]);
            g.add_edge(
                n1,
                n2,
                ConflictRatio {
                    solutions,
                    all: sol_len2,
                },
            );
            g.add_edge(
                n2,
                n1,
                ConflictRatio {
                    solutions,
                    all: sol_len1,
                },
            );
        }
    }
    g
}

fn count_solutions<'a, C, G>(
    m1: impl Iterator<Item = MergedTransition<'a, C, G>>,
    m2: impl Iterator<Item = MergedTransition<'a, C, G>> + Clone,
) -> usize
where
    C: Eq + Hash + Clone + Ord + 'a,
    G: Clone + 'a,
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

pub struct CountingVisitor {
    test: usize,
    down: usize,
    solutions: usize,
}

pub trait Visitor<C> {
    fn test(&mut self);
    fn down(&mut self);
    fn solution(&mut self, sol: &HashMap<C, bool>);
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

impl<C: fmt::Debug> Visitor<C> for CountingVisitor {
    fn test(&mut self) {
        self.test += 1;
    }

    fn down(&mut self) {
        self.down += 1;
    }

    fn solution(&mut self, _: &HashMap<C, bool>) {
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

    fn solution(&mut self, _: &HashMap<C, bool>) {}
}

pub fn find_solutions<'a: 'b, 'b, C, G: 'b, D>(
    spec: &'a [LabeledTransitionSystem<C, D, G>],
    states: &'b [&State<G>],
    visitor: Option<&mut dyn Visitor<C>>,
) -> usize
where
    C: Clone + Eq + Hash + Ord,
    G: Guard<D> + Default,
{
    rec_solutions(
        spec,
        states,
        visitor.unwrap_or(&mut DummyVisitor),
        HashMap::with_capacity(spec.iter().map(|c| c.clocks().len()).sum()),
    )
    .unwrap_or(0)
}

fn rec_solutions<'a, 'b, C, G: 'b, D>(
    spec: &'a [LabeledTransitionSystem<C, D, G>],
    states: &'b [&State<G>],
    visitor: &mut dyn Visitor<C>,
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
            visitor.test();
            if !t.label.has_conflict_with_map(&applied) {
                visitor.down();
                rec_solutions(
                    spec,
                    states,
                    visitor,
                    applied
                        .iter()
                        .map(|(c, b)| (c, *b))
                        .chain(t.label.clocks.iter().map(|c| (c, false)))
                        .chain(t.label.present.iter().map(|c| (c, true)))
                        .map(|(c, b)| (c.clone(), b))
                        .collect(),
                )
                .unwrap_or_else(|| {
                    visitor.solution(&applied);
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
pub struct Approximation {
    pub solutions: usize,
    pub all: usize,
    pub downs: usize,
    pub tests: usize,
}

impl fmt::Display for Approximation {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

pub fn approximate_complexity(g: &Graph<ConflictSource, ConflictRatio>) -> Approximation {
    if g.node_count() == 0 {
        return Approximation::default();
    }
    let mut selected: HashSet<_> = HashSet::new();
    let mut aprox = Approximation {
        solutions: 1,
        all: 1,
        downs: 0,
        tests: 0,
    };
    for to in g.node_indices() {
        let (solution, all) = g
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
                (aprox.solutions * t, aprox.all * t)
            });
        selected.insert(to);
        aprox.downs += solution;
        aprox.tests += aprox.solutions * g.node_weight(to).unwrap().transitions;
        aprox.solutions = solution;
        aprox.all = all;
    }
    aprox
}

pub fn compare_approx_and_solutions<C>(
    spec: &[STS<C>],
) -> Vec<(usize, CountingVisitor, Approximation)>
where
    C: Ord + Hash + Clone + Debug,
{
    spec.iter()
        .map(|sts| sts.states().iter())
        .multi_cartesian_product()
        .map(|comb: Vec<&State<BooleanExpression<Delta<C>>>>| {
            let mut visitor = CountingVisitor::new();
            (
                find_solutions(spec, &comb, Some(&mut visitor)),
                visitor,
                approximate_complexity(&conflict_map(spec, &comb)),
            )
        })
        .collect()
}
