use crate::lccsl::constraints::{Constraint, Delay, Precedence};
use itertools::Itertools;
use std::iter::once;

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

pub fn precedence_tree(depth: usize) -> Option<Vec<Constraint<usize>>> {
    if depth == 0 {
        None
    } else {
        Some(vec![])
    }
}

pub fn subclocking_tree(depth: usize) -> Option<Vec<Constraint<usize>>> {
    if depth == 0 {
        None
    } else {
        Some(vec![])
    }
}
