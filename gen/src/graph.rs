use petgraph::prelude::*;
use rand::prelude::*;
use std::cmp::max;
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

// stolen from https://docs.rs/graphalgs/latest/src/graphalgs/generate/randomg.rs.html#38-69
pub fn random_digraph(
    nodes: usize,
    nedges: usize,
    mut rng: impl Rng,
) -> Option<DiGraph<usize, ()>> {
    if nodes == 0 {
        return None;
    }

    if nedges > nodes * (nodes - 1) {
        return None;
    }

    let mut graph = DiGraph::with_capacity(nodes, nedges);
    let mut count = 0;

    while count < nedges {
        let i = NodeIndex::new(rng.gen_range(0..nodes));
        let j = NodeIndex::new(rng.gen_range(0..nodes));

        if i != j && !graph.contains_edge(i, j) {
            graph.add_edge(i, j, ());
            count += 1;
        }
    }
    Some(graph)
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
