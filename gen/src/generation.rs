use std::iter::{once, FromIterator};

use itertools::Itertools;
use petgraph::prelude::*;
use rand::prelude::*;

use ccsl::lccsl::constraints::{
    Causality, Constraint, Delay, Exclusion, Infinity, Intersection, Minus, Precedence, Repeat,
    Subclocking, Supremum, Union,
};
use std::collections::BTreeSet;

pub fn cycle_spec(size: usize) -> Option<Vec<Constraint<usize>>> {
    if size <= 1 {
        None
    } else {
        Some(
            (0..size)
                .tuple_windows()
                .map(|(l, r)| {
                    Precedence {
                        left: l,
                        right: r,
                        init: None,
                        max: None,
                    }
                    .into()
                })
                .chain(once(
                    Delay {
                        out: size - 1,
                        base: 0,
                        delay: 1,
                        on: None,
                    }
                    .into(),
                ))
                .collect(),
        )
    }
}

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

pub fn cycle_with_tail(size: usize) -> impl Iterator<Item = Vec<Constraint<usize>>> + Clone {
    let orig_spec = cycle_spec(size).unwrap();
    (0..size - 1).map(move |i| {
        orig_spec
            .iter()
            .cloned()
            .chain(add_chain(size, i, 3, false))
            .collect_vec()
    })
}

pub fn cycle_with_spike(size: usize) -> impl Iterator<Item = Vec<Constraint<usize>>> + Clone {
    let orig_spec = cycle_spec(size).unwrap();
    (0..size - 1).map(move |i| {
        orig_spec
            .iter()
            .cloned()
            .chain(add_chain(size, i, 3, true))
            .collect_vec()
    })
}
pub fn cycle_with_tail_and_spike(
    size: usize,
) -> impl Iterator<Item = Vec<Constraint<usize>>> + Clone {
    let orig_spec = cycle_spec(size).unwrap();
    (0..size - 1).map(move |i| {
        orig_spec
            .iter()
            .cloned()
            .chain(add_chain(size, i, 2, false))
            .chain(add_chain(size + 2, size - i - 1, 2, true))
            .collect_vec()
    })
}

fn add_chain(clock_size: usize, pos: usize, tail: usize, out: bool) -> Vec<Constraint<usize>> {
    let mut constraints = Vec::with_capacity(tail + 1);
    for (l, r) in (clock_size..clock_size + tail).tuple_windows() {
        constraints.push(
            Precedence {
                left: l,
                right: r,
                init: None,
                max: None,
            }
            .into(),
        );
    }
    let (l, r) = if out {
        (pos, clock_size)
    } else {
        (clock_size + tail - 1, pos)
    };
    constraints.push(
        Precedence {
            left: l,
            right: r,
            init: None,
            max: None,
        }
        .into(),
    );
    constraints
}
