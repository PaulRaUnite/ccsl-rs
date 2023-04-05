use std::iter::FromIterator;

use itertools::Itertools;
use petgraph::prelude::*;
use rand::prelude::*;

use crate::generation::graph::{random_processing_network, TreeIterator};
use crate::kernel::constraints::{
    Causality, Constraint, Delay, Exclusion, Infinity, Intersection, Minus, Precedence, Repeat,
    Subclocking, Supremum, Union,
};
use petgraph::visit::IntoNodeReferences;
use std::collections::BTreeSet;
use std::fmt::{Display, Formatter};
use std::num::NonZeroUsize;
use std::ops::Range;
use std::str::FromStr;

pub fn to_precedence_spec<N, E>(g: &DiGraph<N, E>) -> Vec<Constraint<usize>> {
    let mut spec = Vec::with_capacity(g.edge_count());
    spec.extend(g.raw_edges().iter().map(|e| {
        Precedence {
            left: e.source().index(),
            right: e.target().index(),
            init: None,
            max: None,
        }
        .into()
    }));
    spec
}

pub fn to_subclocking_spec<N, E>(g: &DiGraph<N, E>) -> Vec<Constraint<usize>> {
    let mut spec = Vec::with_capacity(g.edge_count());
    spec.extend(g.raw_edges().iter().map(|e| {
        Subclocking {
            left: e.source().index(),
            right: e.target().index(),
        }
        .into()
    }));
    spec
}

pub fn random_specification(seed: u64, size: usize) -> Vec<Constraint<usize>> {
    let mut spec = Vec::with_capacity(size);
    let mut rng = StdRng::seed_from_u64(seed);
    let clock_size = 2 * size;

    for _ in 0..size {
        let all = (0..rng.gen_range(1..clock_size))
            .map(|_| rng.gen_range(0..clock_size))
            .collect();
        let left = rng.gen_range(0..clock_size);
        let right = {
            let clock = rng.gen_range(0..clock_size - 1);
            if clock >= left {
                clock + 1
            } else {
                clock
            }
        };
        let out = rng.gen_range(0..clock_size);
        let others = (0..rng.gen_range(1..clock_size))
            .map(|_| {
                let clock = rng.gen_range(0..clock_size - 1);
                if clock >= out {
                    clock + 1
                } else {
                    clock
                }
            })
            .collect();
        let c: Constraint<usize> = match rng.gen_range(0..11) {
            0 => Causality {
                left,
                right,
                init: None,
                max: None,
            }
            .into(),

            1 => Precedence {
                left,
                right,
                init: None,
                max: None,
            }
            .into(),
            2 => Subclocking { left, right }.into(),
            3 => Exclusion { clocks: all }.into(),
            4 => Union { out, args: others }.into(),
            5 => Intersection { out, args: others }.into(),
            6 => Infinity { out, left, right }.into(),
            7 => Supremum { out, left, right }.into(),
            8 => Minus {
                out,
                left: rng.gen_range(0..clock_size),
                right: rng.gen_range(0..clock_size),
            }
            .into(),
            9 => Repeat {
                out,
                every: rng.gen_range(0..clock_size),
                base: rng.gen_range(0..clock_size),
                from: Some(rng.gen_range(0..clock_size)),
                up_to: None,
            }
            .into(),
            10 => Delay {
                out,
                base: left,
                delay: rng.gen_range(0..clock_size),
                on: None,
            }
            .into(),
            _ => {
                panic!();
            }
        };
        spec.push(c);
    }
    spec
}

fn gen_2_clocks(rng: &mut StdRng, known: &BTreeSet<usize>, clock_size: usize) -> (usize, usize) {
    let left = *known.iter().choose(rng).unwrap();
    let right = rng.gen_range(0..clock_size - 1);
    let right = if right < left { right } else { right + 1 };
    if rng.gen_ratio(1, 2) {
        (left, right)
    } else {
        (right, left)
    }
}

fn gen_expr_clocks(
    rng: &mut StdRng,
    known: &BTreeSet<usize>,
    clock_size: usize,
    fixed_size: bool,
) -> (usize, BTreeSet<usize>) {
    let size = if fixed_size {
        3
    } else {
        rng.gen_range(3..clock_size)
    };
    let mut all = Vec::with_capacity(size);
    let first = *known.iter().choose(rng).unwrap();
    all.push(first);
    let mut complement = BTreeSet::from_iter(0..clock_size);
    complement.remove(&first);
    all.extend(complement.iter().copied().choose_multiple(rng, size - 1));
    all.shuffle(rng);
    let (first, tail) = all.split_first().unwrap();
    (*first, tail.iter().copied().collect())
}

pub fn random_connected_specification(
    seed: u64,
    size: usize,
    fixed_size: bool,
) -> Vec<Constraint<usize>> {
    let mut spec = Vec::with_capacity(size);
    let mut rng = StdRng::seed_from_u64(seed);
    let clock_size = 3 * size;
    let mut known_clocks = BTreeSet::new();
    known_clocks.insert(0);

    for _ in 0..size {
        let constr = rng.gen_range(0..6);
        let c: Constraint<usize> = match constr {
            0 => {
                // TODO: make it compact
                let (left, right) = gen_2_clocks(&mut rng, &known_clocks, clock_size);
                known_clocks.insert(left);
                known_clocks.insert(right);
                Causality {
                    left,
                    right,
                    init: None,
                    max: None,
                }
                .into()
            }

            1 => {
                let (left, right) = gen_2_clocks(&mut rng, &known_clocks, clock_size);
                known_clocks.insert(left);
                known_clocks.insert(right);
                Precedence {
                    left,
                    right,
                    init: None,
                    max: None,
                }
                .into()
            }
            2 => {
                let (left, right) = gen_2_clocks(&mut rng, &known_clocks, clock_size);
                known_clocks.insert(left);
                known_clocks.insert(right);
                Subclocking { left, right }.into()
            }
            3 => {
                if fixed_size {
                    let (left, right) = gen_2_clocks(&mut rng, &known_clocks, clock_size);
                    let clocks = vec![left, right];
                    known_clocks.extend(clocks.iter().copied());
                    Exclusion {
                        clocks: clocks.into_iter().collect(),
                    }
                    .into()
                } else {
                    let (out, mut others) =
                        gen_expr_clocks(&mut rng, &known_clocks, clock_size, false);
                    others.insert(out);
                    known_clocks.extend(others.iter().copied());
                    Exclusion { clocks: others }.into()
                }
            }
            4 => {
                let (out, others) =
                    gen_expr_clocks(&mut rng, &known_clocks, clock_size, fixed_size);
                known_clocks.insert(out);
                known_clocks.extend(others.iter().copied());
                Union { out, args: others }.into()
            }
            5 => {
                let (out, others) =
                    gen_expr_clocks(&mut rng, &known_clocks, clock_size, fixed_size);
                known_clocks.insert(out);
                known_clocks.extend(others.iter().copied());
                Intersection { out, args: others }.into()
            }
            6 => {
                let (out, others) = gen_expr_clocks(&mut rng, &known_clocks, clock_size, true);
                known_clocks.insert(out);
                known_clocks.extend(others.iter().copied());
                let (left, right) = others.into_iter().collect_tuple().unwrap();
                Infinity { out, left, right }.into()
            }
            7 => {
                let (out, others) = gen_expr_clocks(&mut rng, &known_clocks, clock_size, true);
                known_clocks.insert(out);
                known_clocks.extend(others.iter().copied());
                let (left, right) = others.into_iter().collect_tuple().unwrap();
                Supremum { out, left, right }.into()
            }
            8 => {
                let (out, others) = gen_expr_clocks(&mut rng, &known_clocks, clock_size, true);
                known_clocks.insert(out);
                known_clocks.extend(others.iter().copied());
                let (left, right) = others.into_iter().collect_tuple().unwrap();
                Minus { out, left, right }.into()
            }
            9 => {
                let (left, right) = gen_2_clocks(&mut rng, &known_clocks, clock_size);
                known_clocks.insert(left);
                known_clocks.insert(right);
                Repeat {
                    out: left,
                    every: rng.gen_range(0..clock_size),
                    base: right,
                    from: Some(rng.gen_range(0..clock_size)),
                    up_to: None,
                }
                .into()
            }
            10 => {
                let (left, right) = gen_2_clocks(&mut rng, &known_clocks, clock_size);
                known_clocks.insert(left);
                known_clocks.insert(right);
                Delay {
                    out: left,
                    base: right,
                    delay: rng.gen_range(0..clock_size),
                    on: None,
                }
            }
            .into(),
            _ => {
                panic!();
            }
        };
        spec.push(c);
    }
    spec.shuffle(&mut rng);
    spec
}

pub fn cycle_with_tail_and_spike(
    size: usize,
    tail: usize,
    spike: usize,
    buffer: usize,
) -> Vec<Constraint<usize>> {
    let mut spec = vec![];
    spec.extend(prec_chain(0..size + tail + spike));
    if buffer > 0 {
        // otherwise the
        spec.push(
            Delay {
                out: tail + size,
                base: tail,
                delay: buffer,
                on: None,
            }
            .into(),
        );
    }
    spec
}

fn shift_range(r: Range<usize>, shift: usize) -> Range<usize> {
    let Range { start, end } = r;
    start + shift..end + shift
}
fn prec_chain(r: Range<usize>) -> impl Iterator<Item = Constraint<usize>> {
    r.clone().zip(shift_range(r, 1)).map(|(l, r)| {
        Precedence {
            left: l,
            right: r,
            init: None,
            max: None,
        }
        .into()
    })
}

pub fn deadlock_free() -> Vec<Constraint<usize>> {
    todo!()
}

pub fn deadlocking() -> Vec<Constraint<usize>> {
    todo!()
}

pub fn trees_with_backpressure(
    size: usize,
    buffer: usize,
) -> impl Iterator<Item = Vec<Constraint<usize>>> + Clone {
    TreeIterator::new(size + 1).map(move |g| {
        let (first_leaf, _) = g
            .node_references()
            .find(|(n, _)| g.edges_directed(*n, Outgoing).next().is_none())
            .unwrap();
        let root = 0; // FIXME: is it??
        let mut spec = to_precedence_spec(&g);
        spec.extend(point_backpressure(root, first_leaf.index(), buffer));
        spec
    })
}

pub fn precedence_trees(size: usize) -> impl Iterator<Item = Vec<Constraint<usize>>> + Clone {
    TreeIterator::new(size + 1).map(|g| to_precedence_spec(&g))
}

pub fn bound_by_exclusion() -> Vec<Constraint<usize>> {
    todo!()
}

#[derive(Clone, Debug)]
pub struct NetworkParams {
    pub sources: usize,
    pub intermediates: Vec<usize>,
    pub sinks: usize,
}

impl From<Vec<NonZeroUsize>> for NetworkParams {
    fn from(value: Vec<NonZeroUsize>) -> Self {
        if value.len() < 2 {
            panic!("dimensions vector is not big enough (expected 2+)");
        }
        let (first, tail) = value.split_first().unwrap();
        let (last, middle) = tail.split_last().unwrap();
        NetworkParams {
            sources: (*first).into(),
            intermediates: middle.iter().copied().map(Into::into).collect_vec(),
            sinks: (*last).into(),
        }
    }
}

impl FromStr for NetworkParams {
    type Err = <NonZeroUsize as FromStr>::Err;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let layers: Vec<NonZeroUsize> = s
            .split(',')
            .map(|chunk| chunk.parse::<NonZeroUsize>())
            .try_collect()?;
        Ok(layers.into())
    }
}

impl Display for NetworkParams {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.sources)?;
        for v in &self.intermediates {
            write!(f, "_{}", v)?;
        }
        write!(f, "_{}", self.sinks)
    }
}

// Robert' idea, only exclusions for now
pub fn inputs_to_restrictions(seed: u64, params: &NetworkParams) -> Vec<Constraint<usize>> {
    let (g, _, outputs) = random_processing_network(seed, params);
    let mut constraints = to_precedence_spec(&g);
    constraints.push(
        Exclusion {
            // TODO: should probably be something more complex than just exclusion
            clocks: outputs.into_iter().map(|n| n.index()).collect(),
        }
        .into(),
    );
    constraints
}

pub fn pipeline_parallel_execution(params: NetworkParams) -> Vec<Constraint<usize>> {
    todo!()
}

// Backpressure can be different:
// - individual for each leaf with different values
// - or though some kind of aggregation (synchronization barrier)
// - or there is only one backpressure link, others will be deducted from it
pub fn point_backpressure<C: Clone>(
    input: C,
    output: C,
    buffer: usize,
) -> impl Iterator<Item = Constraint<C>> {
    let delayed_input = input.clone();
    [
        Delay {
            out: delayed_input.clone(),
            base: input,
            delay: buffer,
            on: None,
        }
        .into(),
        Precedence {
            left: output,
            right: delayed_input,
            init: None,
            max: None,
        }
        .into(),
    ]
    .into_iter()
}
