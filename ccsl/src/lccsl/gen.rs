use std::iter::once;

use itertools::Itertools;
use petgraph::prelude::*;
use rand::prelude::*;

use crate::lccsl::constraints::{
    Causality, Constraint, Delay, Exclusion, Infinity, Intersection, Minus, Precedence, Repeat,
    Subclocking, Supremum, Union,
};

pub fn circle_spec(size: usize) -> Option<Vec<Constraint<usize>>> {
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

pub fn tree(nodes: usize) -> DiGraph<usize, ()> {
    let mut rng = rand::rngs::StdRng::seed_from_u64(3453452);
    let mut g = DiGraph::with_capacity(nodes, nodes - 1);
    g.add_node(0);

    for node in 1..nodes {
        let target_node = g.add_node(node);
        let source_node = NodeIndex::<u32>::new(rng.gen_range(0..node));
        g.add_edge(source_node, target_node, ());
    }
    g
}

pub fn to_precedence_spec<N, E>(g: &DiGraph<N, E>) -> Vec<Precedence<usize>> {
    let mut spec = Vec::with_capacity(g.edge_count());
    spec.extend(g.raw_edges().into_iter().map(|e| Precedence {
        left: e.source().index(),
        right: e.target().index(),
        init: None,
        max: None,
    }));
    spec
}

pub fn to_subclocking_spec<N, E>(g: &DiGraph<N, E>) -> Vec<Subclocking<usize>> {
    let mut spec = Vec::with_capacity(g.edge_count());
    spec.extend(g.raw_edges().into_iter().map(|e| Subclocking {
        left: e.source().index(),
        right: e.target().index(),
    }));
    spec
}

#[derive(Debug, Clone)]
pub struct TreeIterator {
    current_level_sequence: Vec<i64>,
    l: Vec<i64>,
    vertices: usize,
    k: i64,
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
            k: 0,
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
                    h1 = h1 - 1;
                }
                fixit = 1;
            } else {
                p = r;
                r = r - 1;
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

pub fn random_specification(size: usize) -> Vec<Constraint<usize>> {
    let mut spec = Vec::with_capacity(size);
    let mut rng = rand::rngs::StdRng::from_entropy();
    let clock_size = 2 * size;

    for _ in 0..size {
        let all = (0..rng.gen_range(1..clock_size))
            .map(|_| rng.gen_range(0..clock_size))
            .collect();
        let out = rng.gen_range(0..clock_size);
        let others = (0..rng.gen_range(1..clock_size))
            .map(|_| {
                let clock = rng.gen_range(0..clock_size);
                if clock >= out {
                    clock + 1
                } else {
                    clock
                }
            })
            .collect();
        let c: Constraint<usize> = match rng.gen_range(0..11) {
            0 => Causality {
                left: rng.gen_range(0..clock_size),
                right: rng.gen_range(0..clock_size),
                init: None,
                max: None,
            }
            .into(),

            1 => Precedence {
                left: rng.gen_range(0..clock_size),
                right: rng.gen_range(0..clock_size),
                init: None,
                max: None,
            }
            .into(),
            2 => Subclocking {
                left: rng.gen_range(0..clock_size),
                right: rng.gen_range(0..clock_size),
            }
            .into(),
            3 => Exclusion { clocks: all }.into(),
            4 => Infinity { out, args: others }.into(),
            5 => Supremum { out, args: others }.into(),
            6 => Union { out, args: others }.into(),
            7 => Intersection { out, args: others }.into(),
            8 => Minus {
                out,
                base: rng.gen_range(0..clock_size),
                args: others,
            }
            .into(),
            9 => Repeat {
                out,
                every: Some(rng.gen_range(0..clock_size)),
                base: rng.gen_range(0..clock_size),
                from: Some(rng.gen_range(0..clock_size)),
                up_to: None,
            }
            .into(),
            10 => Delay {
                out,
                base: rng.gen_range(0..clock_size),
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
