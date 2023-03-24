use std::iter::{once, FromIterator};

use itertools::Itertools;
use petgraph::prelude::*;
use rand::prelude::*;

use ccsl::lccsl::constraints::{
    Causality, Constraint, Delay, Exclusion, Infinity, Intersection, Minus, Precedence, Repeat,
    Subclocking, Supremum, Union,
};
use std::cmp::max;
use std::collections::BTreeSet;

pub fn cycle_spec(size: usize) -> Option<Vec<Constraint<usize>>> {
    if size <= 1 {
        None
    } else {
        Some(
            (0..size)
                .into_iter()
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

pub fn star(nodes: usize, width: usize) -> Option<DiGraph<usize, ()>> {
    if nodes == 0 || width == 0 {
        None
    } else {
        let mut g = DiGraph::with_capacity(nodes, nodes - 1);
        g.add_node(0);
        for target in 1..nodes {
            let source = if target.checked_sub(width).map(|x| x > 0).unwrap_or_default() {
                target - width
            } else {
                0
            };
            let target = g.add_node(target);
            let source = NodeIndex::new(source);
            g.add_edge(source, target, ());
        }
        Some(g)
    }
}

pub fn to_precedence_spec<N, E>(g: &DiGraph<N, E>) -> Vec<Constraint<usize>> {
    let mut spec = Vec::with_capacity(g.edge_count());
    spec.extend(g.raw_edges().into_iter().map(|e| {
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
    spec.extend(g.raw_edges().into_iter().map(|e| {
        Subclocking {
            left: e.source().index(),
            right: e.target().index(),
        }
        .into()
    }));
    spec
}

#[derive(Debug, Clone)]
pub struct TreeIterator {
    current_level_sequence: Vec<i64>,
    l: Vec<i64>,
    vertices: usize,
    //k: i64, TODO: why unused? maybe can remove some other variables?
    p: i64,
    q: i64,
    h1: i64,
    h2: i64,
    c: i64,
    r: i64,
    first_time: bool,
}

impl TreeIterator {
    pub fn new(vertices: usize) -> Self {
        Self {
            current_level_sequence: vec![],
            l: vec![],
            vertices,
            //k: 0,
            p: 0,
            q: 0,
            h1: 0,
            h2: 0,
            c: 0,
            r: 0,
            first_time: true,
        }
    }

    fn generate_first_level_sequence(&mut self) {
        self.current_level_sequence = vec![0; self.vertices];
        self.l = vec![0; self.vertices];
        let k = ((self.vertices / 2) + 1) as i64;

        let n = self.vertices as i64;

        if self.vertices == 4 {
            self.p = 3
        } else {
            self.p = n;
        }
        self.q = n - 1;
        self.h1 = k;
        self.h2 = n;
        if self.vertices % 2 == 1 {
            self.c = i64::MAX;
        } else {
            self.c = n + 1;
        }
        self.r = k;

        for i in 1..=k {
            self.l[(i - 1) as usize] = i;
        }
        for i in k + 1..=n {
            self.l[(i - 1) as usize] = i - k + 1;
        }
        for i in 0..self.vertices {
            self.current_level_sequence[i] = i as i64;
        }
        if self.vertices > 2 {
            self.current_level_sequence[k as usize] = 1;
        }
        if self.vertices <= 3 {
            self.q = 0;
        }
    }
    fn generate_next_level_sequence(&mut self) {
        let mut fixit = 0;

        let mut needr = 0;
        let mut needc = 0;
        let mut needh2 = 0;

        let n = self.vertices as i64;
        let mut p = self.p;
        let mut q = self.q;
        let mut h1 = self.h1;
        let mut h2 = self.h2;
        let mut c = self.c;
        let mut r = self.r;
        let l = &mut self.l;
        let w = &mut self.current_level_sequence;

        if c == n + 1
            || p == h2
                && (l[(h1 - 1) as usize] == l[(h2 - 1) as usize] + 1 && n - h2 > r - h1
                    || l[(h1 - 1) as usize] == l[(h2 - 1) as usize] && n - h2 + 1 < r - h1)
        {
            if l[(r - 1) as usize] > 3 {
                p = r;
                q = w[(r - 1) as usize];
                if h1 == r {
                    h1 -= 1;
                }
                fixit = 1;
            } else {
                p = r;
                r -= 1;
                q = 2;
            }
        }

        if p <= h1 {
            h1 = p - 1;
        }
        if p <= r {
            needr = 1;
        } else if p <= h2 {
            needh2 = 1;
        } else if l[(h2 - 1) as usize] == l[(h1 - 1) as usize] - 1 && n - h2 == r - h1 {
            if p <= c {
                needc = 1;
            }
        } else {
            c = i64::MAX;
        }

        let oldp = p;
        let delta = q - p;
        let oldlq = l[(q - 1) as usize];
        let oldwq = w[(q - 1) as usize];
        p = i64::MAX;

        for i in oldp..=n {
            let i1 = (i as i64 - 1 + delta) as usize;
            let i2 = (i - 1) as usize;
            l[i2] = l[i1];
            if l[i2] == 2 {
                w[i2] = 1;
            } else {
                p = i;
                if l[i2] == oldlq {
                    q = oldwq;
                } else {
                    q = w[i1] - delta;
                }
                w[i2] = q;
            }
            if needr == 1 && l[i2] == 2 {
                needr = 0;
                needh2 = 1;
                r = i - 1;
            }
            if needh2 == 1 && l[i2] <= l[(i - 2) as usize] && i > r + 1 {
                needh2 = 0;
                h2 = i - 1;
                if l[(h2 - 1) as usize] == l[(h1 - 1) as usize] - 1 && n - h2 == r - h1 {
                    needc = 1;
                } else {
                    c = i64::MAX;
                }
            }
            if needc == 1 {
                if l[i2] != l[(h1 - h2 + i - 1) as usize] - 1 {
                    needc = 0;
                    c = i;
                } else {
                    c = i + 1;
                }
            }
        }
        if fixit == 1 {
            r = n - h1 + 1;
            for i in r + 1..=n {
                l[(i - 1) as usize] = i - r + 1;
                w[(i - 1) as usize] = i - 1;
            }
            w[r as usize] = 1;
            h2 = n;
            p = n;
            q = p - 1;
            c = i64::MAX;
        } else {
            if p == i64::MAX {
                if l[(oldp - 2) as usize] != 2 {
                    p = oldp - 1;
                } else {
                    p = oldp - 2;
                }
                q = w[(p - 1) as usize];
            }
            if needh2 == 1 {
                h2 = n;
                if l[(h2 - 1) as usize] == l[(h1 - 1) as usize] - 1 && h1 == r {
                    c = n + 1;
                } else {
                    c = i64::MAX;
                }
            }
        }
        self.p = p;
        self.q = q;
        self.h1 = h1;
        self.h2 = h2;
        self.c = c;
        self.r = r;
    }
}

impl Iterator for TreeIterator {
    type Item = DiGraph<usize, ()>;

    fn next(&mut self) -> Option<Self::Item> {
        if !self.first_time && self.q == 0 {
            return None;
        }

        if self.first_time {
            self.first_time = false;
            if self.vertices > 0 {
                self.generate_first_level_sequence();
            } else {
                self.q = 0;
            }
        } else {
            self.generate_next_level_sequence();
        }

        let mut g = DiGraph::with_capacity(self.vertices, self.vertices - 1);
        for i in 0..self.vertices {
            g.add_node(i);
        }

        for i in 1..self.vertices {
            let vertex1 = i;
            let vertex2 = (self.current_level_sequence[i] - 1) as usize;
            g.add_edge(NodeIndex::new(vertex1), NodeIndex::new(vertex2), ());
        }

        Some(g)
    }
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
                let (left, right) = gen_2_clocks(&mut rng, &mut known_clocks, clock_size);
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
                let (left, right) = gen_2_clocks(&mut rng, &mut known_clocks, clock_size);
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
                let (left, right) = gen_2_clocks(&mut rng, &mut known_clocks, clock_size);
                known_clocks.insert(left);
                known_clocks.insert(right);
                Subclocking { left, right }.into()
            }
            3 => {
                if fixed_size {
                    let (left, right) = gen_2_clocks(&mut rng, &mut known_clocks, clock_size);
                    let clocks = vec![left, right];
                    known_clocks.extend(clocks.iter().copied());
                    Exclusion {
                        clocks: clocks.into_iter().collect(),
                    }
                    .into()
                } else {
                    let (out, mut others) =
                        gen_expr_clocks(&mut rng, &mut known_clocks, clock_size, false);
                    others.insert(out);
                    known_clocks.extend(others.iter().copied());
                    Exclusion { clocks: others }.into()
                }
            }
            4 => {
                let (out, others) =
                    gen_expr_clocks(&mut rng, &mut known_clocks, clock_size, fixed_size);
                known_clocks.insert(out);
                known_clocks.extend(others.iter().copied());
                Union { out, args: others }.into()
            }
            5 => {
                let (out, others) =
                    gen_expr_clocks(&mut rng, &mut known_clocks, clock_size, fixed_size);
                known_clocks.insert(out);
                known_clocks.extend(others.iter().copied());
                Intersection { out, args: others }.into()
            }
            6 => {
                let (out, others) = gen_expr_clocks(&mut rng, &mut known_clocks, clock_size, true);
                known_clocks.insert(out);
                known_clocks.extend(others.iter().copied());
                let (left, right) = others.into_iter().collect_tuple().unwrap();
                Infinity { out, left, right }.into()
            }
            7 => {
                let (out, others) = gen_expr_clocks(&mut rng, &mut known_clocks, clock_size, true);
                known_clocks.insert(out);
                known_clocks.extend(others.iter().copied());
                let (left, right) = others.into_iter().collect_tuple().unwrap();
                Supremum { out, left, right }.into()
            }
            8 => {
                let (out, others) = gen_expr_clocks(&mut rng, &mut known_clocks, clock_size, true);
                known_clocks.insert(out);
                known_clocks.extend(others.iter().copied());
                let (left, right) = others.into_iter().collect_tuple().unwrap();
                Minus { out, left, right }.into()
            }
            9 => {
                let (left, right) = gen_2_clocks(&mut rng, &mut known_clocks, clock_size);
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
                let (left, right) = gen_2_clocks(&mut rng, &mut known_clocks, clock_size);
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

pub fn random_connected_graph(seed: u64, vertices: usize) -> UnGraph<(), ()> {
    let (mut rng, mut g) = random_tree(seed, vertices, vertices * 2);
    let edge_count = rng.gen_range(0..vertices ^ 2);
    while g.edge_count() < edge_count {
        g.add_edge(
            NodeIndex::new(rng.gen_range(0..vertices)),
            NodeIndex::new(rng.gen_range(0..vertices)),
            (),
        );
    }
    g
}

pub fn random_tree(seed: u64, vertices: usize, edge_hint: usize) -> (StdRng, UnGraph<(), ()>) {
    assert!(vertices > 0);

    let mut rng = StdRng::seed_from_u64(seed);
    let mut g = UnGraph::with_capacity(vertices, max(edge_hint, vertices - 1));
    for i in 1..vertices {
        let n = g.add_node(());
        g.add_edge(NodeIndex::new(rng.gen_range(0..i)), n, ());
    }
    (rng, g)
}

pub fn cycle_with_tail(size: usize) -> impl Iterator<Item = Vec<Constraint<usize>>> + Clone {
    let orig_spec = cycle_spec(size).unwrap();
    (0..size - 1).into_iter().map(move |i| {
        orig_spec
            .iter()
            .cloned()
            .chain(add_chain(size, i, 3, false))
            .collect_vec()
    })
}

pub fn cycle_with_spike(size: usize) -> impl Iterator<Item = Vec<Constraint<usize>>> + Clone {
    let orig_spec = cycle_spec(size).unwrap();
    (0..size - 1).into_iter().map(move |i| {
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
    (0..size - 1).into_iter().map(move |i| {
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
