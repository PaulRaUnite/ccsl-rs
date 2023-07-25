use crate::kernel::constraints::{
    Causality, Constraint, Delay, Exclusion, Intersection, Minus, Precedence, Repeat, Subclocking,
    Union,
};
use bitvec::slice::BitSlice;
use derive_more::From;
use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use std::hash::Hash;

#[derive(Clone, Debug, From)]
pub struct Trace<C, const L: usize>(pub Vec<(C, [u8; L])>);
pub type TestSet<C, const L: usize> = Vec<(Trace<C, L>, bool)>;

impl<C: Clone + Eq + Hash, const L: usize> Trace<C, L> {
    /// Interprets the traces from left to right (as written).
    /// In reverse to indexing of bits inside of a byte.
    pub fn interpret<'a>(&'a self) -> impl Iterator<Item = HashMap<C, bool>> + 'a {
        (0..L).flat_map(move |i| {
            (0..u8::BITS).map(move |j| {
                (&self.0)
                    .into_iter()
                    .map(|(clock, subtrace)| {
                        (
                            clock.clone(),
                            *BitSlice::<u8>::from_element(&subtrace[i])
                                .get((u8::BITS - j - 1) as usize)
                                .unwrap(),
                        )
                    })
                    .collect::<HashMap<C, bool>>()
            })
        })
    }
}

impl<C, const L: usize> Trace<C, L> {
    pub fn map_clocks<T>(&self, mut f: impl FnMut(&C) -> T) -> Trace<T, L> {
        Trace(
            self.0
                .iter()
                .map(|(clock, subtrace)| (f(clock), *subtrace))
                .collect(),
        )
    }
}

impl<C, const L: usize> Trace<C, L> {
    pub const fn len() -> usize {
        L * (u8::BITS as usize)
    }
}

impl<C: Display, const L: usize> Display for Trace<C, L> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for (clock, subtrace) in self.0.iter() {
            write!(f, "{}: ", clock)?;
            for chunk in subtrace {
                write!(f, "{:#010b}", chunk)?;
            }
            writeln!(f)?;
        }
        Ok(())
    }
}
pub fn causality() -> (Constraint<usize>, TestSet<usize, 1>) {
    let c = Causality {
        cause: 0,
        effect: 1,
        init: None,
        max: None,
    }
    .into();

    let tests: TestSet<usize, 1> = vec![
        (Trace(vec![(0, [0b11111111]), (1, [0b00000000])]), true),
        (Trace(vec![(0, [0b11111111]), (1, [0b11111111])]), true),
        (Trace(vec![(0, [0b11110000]), (1, [0b10101010])]), true),
        (Trace(vec![(0, [0b10101010]), (1, [0b01010100])]), true),
        (Trace(vec![(0, [0b00001000]), (1, [0b00010000])]), false),
        (Trace(vec![(0, [0b10101010]), (1, [0b01010101])]), true),
        (Trace(vec![(0, [0b00001111]), (1, [0b00001000])]), true),
        (Trace(vec![(0, [0b00000000]), (1, [0b11111111])]), false),
    ];
    (c, tests)
}

pub fn bounded_causality() -> (Constraint<usize>, TestSet<usize, 1>) {
    let c = Causality {
        cause: 0,
        effect: 1,
        init: Some(4),
        max: None,
    }
    .into();

    let tests: TestSet<usize, 1> = vec![
        (Trace(vec![(0, [0b00001111]), (1, [0b11111111])]), true),
        (Trace(vec![(0, [0b00000000]), (1, [0b11111111])]), false),
    ];
    (c, tests)
}

pub fn precedence() -> (Constraint<usize>, TestSet<usize, 1>) {
    let c = Precedence {
        cause: 0,
        effect: 1,
        init: None,
        max: None,
    }
    .into();

    let tests: TestSet<usize, 1> = vec![
        (Trace(vec![(0, [0b11111111]), (1, [0b00000000])]), true),
        (Trace(vec![(0, [0b11111111]), (1, [0b01111111])]), true),
        (Trace(vec![(0, [0b11110000]), (1, [0b00101010])]), true),
        (Trace(vec![(0, [0b10101010]), (1, [0b01010100])]), true),
        (Trace(vec![(0, [0b00001000]), (1, [0b00010000])]), false),
        (Trace(vec![(0, [0b00011000]), (1, [0b00010000])]), false),
        (Trace(vec![(0, [0b10101010]), (1, [0b01010101])]), true),
        (Trace(vec![(0, [0b00001111]), (1, [0b00000100])]), true),
        (Trace(vec![(0, [0b00000000]), (1, [0b11111111])]), false),
    ];

    (c, tests)
}

pub fn subclocking() -> (Constraint<usize>, TestSet<usize, 1>) {
    let c = Subclocking { sub: 0, sup: 1 }.into();

    let variants: TestSet<usize, 1> = vec![
        (Trace(vec![(0, [0b00000000]), (1, [0b00000000])]), true),
        (Trace(vec![(0, [0b11111111]), (1, [0b11111111])]), true),
        (Trace(vec![(0, [0b11110000]), (1, [0b11111111])]), true),
        (Trace(vec![(0, [0b00001111]), (1, [0b11110000])]), false),
    ];
    (c, variants)
}

pub fn exclusion() -> (Constraint<usize>, TestSet<usize, 1>) {
    let c = Exclusion {
        clocks: [0, 1].into(),
    }
    .into();

    let variants: TestSet<usize, 1> = vec![
        (Trace(vec![(0, [0b00000000]), (1, [0b00000000])]), true),
        (Trace(vec![(0, [0b11111111]), (1, [0b11111111])]), false),
        (Trace(vec![(0, [0b10101010]), (1, [0b01010101])]), true),
        (Trace(vec![(0, [0b01010101]), (1, [0b10101010])]), true),
        (Trace(vec![(0, [0b00001111]), (1, [0b11110000])]), true),
    ];

    (c, variants)
}

pub fn union() -> (Constraint<usize>, TestSet<usize, 1>) {
    let c = Union {
        out: 2,
        args: [0, 1].into(),
    }
    .into();

    let variants: TestSet<usize, 1> = vec![
        (
            Trace(vec![
                (0, [0b00000000]),
                (1, [0b00000000]),
                (2, [0b00000000]),
            ]),
            true,
        ),
        (
            Trace(vec![
                (0, [0b00011111]),
                (1, [0b10111010]),
                (2, [0b10111111]),
            ]),
            true,
        ),
        (
            Trace(vec![
                (0, [0b01000000]),
                (1, [0b00000010]),
                (2, [0b10111101]),
            ]),
            false,
        ),
        (
            Trace(vec![
                (0, [0b00000000]),
                (1, [0b00000000]),
                (2, [0b11111111]),
            ]),
            false,
        ),
        (
            Trace(vec![
                (0, [0b11111111]),
                (1, [0b00000000]),
                (2, [0b00000000]),
            ]),
            false,
        ),
        (
            Trace(vec![
                (0, [0b00000000]),
                (1, [0b11111111]),
                (2, [0b00000000]),
            ]),
            false,
        ),
    ];

    (c, variants)
}

pub fn intersection() -> (Constraint<usize>, TestSet<usize, 1>) {
    let c = Intersection {
        out: 2,
        args: [0, 1].into(),
    }
    .into();

    let variants: TestSet<usize, 1> = vec![
        (
            Trace(vec![
                (0, [0b00000000]),
                (1, [0b00000000]),
                (2, [0b00000000]),
            ]),
            true,
        ),
        (
            Trace(vec![
                (0, [0b00011111]),
                (1, [0b10111010]),
                (2, [0b00011010]),
            ]),
            true,
        ),
        (
            Trace(vec![
                (0, [0b11111000]),
                (1, [0b00011111]),
                (2, [0b00011000]),
            ]),
            true,
        ),
        (
            Trace(vec![
                (0, [0b00000000]),
                (1, [0b00011111]),
                (2, [0b10011000]),
            ]),
            false,
        ),
    ];

    (c, variants)
}

pub fn minus() -> (Constraint<usize>, TestSet<usize, 1>) {
    let c = Minus {
        out: 2,
        left: 0,
        right: 1,
    }
    .into();

    let variants: TestSet<usize, 1> = vec![
        (
            Trace(vec![
                (0, [0b00000000]),
                (1, [0b00000000]),
                (2, [0b00000000]),
            ]),
            true,
        ),
        (
            Trace(vec![
                (0, [0b11111111]),
                (1, [0b11111111]),
                (2, [0b00000000]),
            ]),
            true,
        ),
        (
            Trace(vec![
                (0, [0b11110000]),
                (1, [0b11111111]),
                (2, [0b00000000]),
            ]),
            true,
        ),
        (
            Trace(vec![
                (0, [0b11110000]),
                (1, [0b00001111]),
                (2, [0b11110000]),
            ]),
            true,
        ),
        (
            Trace(vec![
                (0, [0b00000000]),
                (1, [0b11111111]),
                (2, [0b11111111]),
            ]),
            false,
        ),
        (
            Trace(vec![
                (0, [0b00000000]),
                (1, [0b00000000]),
                (2, [0b11111111]),
            ]),
            false,
        ),
        (
            Trace(vec![
                (0, [0b11111111]),
                (1, [0b00000000]),
                (2, [0b11111111]),
            ]),
            true,
        ),
    ];

    (c, variants)
}

pub fn delay() -> (Constraint<usize>, TestSet<usize, 1>) {
    let c = Delay {
        out: 1,
        trigger: 0,
        delay: 4,
        on: None,
    }
    .into();

    let variants: TestSet<usize, 1> = vec![
        (Trace(vec![(0, [0b00000000]), (1, [0b00000000])]), true),
        (Trace(vec![(0, [0b11111111]), (1, [0b00001111])]), true),
        (Trace(vec![(0, [0b11111111]), (1, [0b00000000])]), false),
        (Trace(vec![(0, [0b11111111]), (1, [0b11111111])]), false),
        (Trace(vec![(0, [0b00000000]), (1, [0b11111111])]), false),
        (Trace(vec![(0, [0b11010101]), (1, [0b00000001])]), true),
    ];

    (c, variants)
}

pub fn delay_on() -> (Constraint<usize>, TestSet<usize, 1>) {
    let c = Delay {
        out: 0,
        trigger: 1,
        delay: 2,
        on: Some(2),
    }
    .into();

    let variants: TestSet<usize, 1> = vec![
        (
            Trace(vec![
                (0, [0b00000000]),
                (1, [0b00000000]),
                (2, [0b00000000]),
            ]),
            true,
        ),
        (
            Trace(vec![
                (0, [0b00000101]),
                (1, [0b11111111]),
                (2, [0b00110101]),
            ]),
            true,
        ),
        (
            Trace(vec![
                (0, [0b11111111]),
                (1, [0b00000000]),
                (2, [0b00000000]),
            ]),
            false,
        ),
    ];

    (c, variants)
}

pub fn repeat() -> (Constraint<usize>, TestSet<usize, 1>) {
    let c = Repeat {
        out: 0,
        every: 3,
        base: 1,
        from: None,
        up_to: None,
    }
    .into();

    let variants: TestSet<usize, 1> = vec![
        (Trace(vec![(0, [0b00000000]), (1, [0b00000000])]), true),
        (Trace(vec![(0, [0b00100100]), (1, [0b11111111])]), true),
        (Trace(vec![(0, [0b11111111]), (1, [0b00000000])]), false),
        (Trace(vec![(0, [0b00000100]), (1, [0b01010101])]), true),
        (Trace(vec![(0, [0b00000000]), (1, [0b11111111])]), false),
    ];

    (c, variants)
}
pub fn all() -> Vec<(Constraint<usize>, TestSet<usize, 1>)> {
    vec![
        precedence(),
        bounded_causality(),
        causality(),
        subclocking(),
        exclusion(),
        union(),
        intersection(),
        minus(),
        delay(),
        delay_on(),
        repeat(),
    ]
}
