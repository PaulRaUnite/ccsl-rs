use crate::kernel::constraints::{
    Causality, Constraint, Delay, Exclusion, Intersection, Minus, Precedence, Subclocking, Union,
};
use bitvec::slice::BitSlice;
use std::collections::HashMap;

type Trace<const L: usize> = Vec<(usize, [u8; L])>;
type TestSet<const L: usize> = Vec<(Trace<L>, bool)>;

/// Iterprets the traces from left to right (as written).
/// In reverse to indexing of bits inside of a byte.
pub fn interpret_trace<'a, const L: usize>(
    trace: &'a Trace<L>,
) -> impl Iterator<Item = HashMap<usize, bool>> + 'a {
    (0..L).flat_map(move |i| {
        (0..u8::BITS).map(move |j| {
            trace
                .iter()
                .map(|(clock, subtrace)| {
                    (
                        *clock,
                        *BitSlice::<u8>::from_element(&subtrace[i])
                            .get((u8::BITS - j - 1) as usize)
                            .unwrap(),
                    )
                })
                .collect::<HashMap<usize, bool>>()
        })
    })
}

pub fn causality() -> (Constraint<usize>, TestSet<1>) {
    let c = Causality {
        cause: 0,
        effect: 1,
        init: None,
        max: None,
    }
    .into();

    let tests: TestSet<1> = vec![
        (vec![(0, [0b11111111]), (1, [0b00000000])], true),
        (vec![(0, [0b11111111]), (1, [0b11111111])], true),
        (vec![(0, [0b11110000]), (1, [0b10101010])], true),
        (vec![(0, [0b10101010]), (1, [0b01010100])], true),
        (vec![(0, [0b00001000]), (1, [0b00010000])], false),
        (vec![(0, [0b10101010]), (1, [0b01010101])], true),
        (vec![(0, [0b00001111]), (1, [0b00001000])], true),
        (vec![(0, [0b00000000]), (1, [0b11111111])], false),
    ];
    (c, tests)
}

pub fn bounded_causality() -> (Constraint<usize>, TestSet<1>) {
    let c = Causality {
        cause: 0,
        effect: 1,
        init: Some(4),
        max: None,
    }
    .into();

    let tests: TestSet<1> = vec![
        (vec![(0, [0b00001111]), (1, [0b11111111])], true),
        (vec![(0, [0b00000000]), (1, [0b11111111])], false),
    ];
    (c, tests)
}

pub fn precedence() -> (Constraint<usize>, TestSet<1>) {
    let c = Precedence {
        cause: 0,
        effect: 1,
        init: None,
        max: None,
    }
    .into();

    let tests: TestSet<1> = vec![
        (vec![(0, [0b11111111]), (1, [0b00000000])], true),
        (vec![(0, [0b11111111]), (1, [0b01111111])], true),
        (vec![(0, [0b11110000]), (1, [0b00101010])], true),
        (vec![(0, [0b10101010]), (1, [0b01010100])], true),
        (vec![(0, [0b00001000]), (1, [0b00010000])], false),
        (vec![(0, [0b00011000]), (1, [0b00010000])], false),
        (vec![(0, [0b10101010]), (1, [0b01010101])], true),
        (vec![(0, [0b00001111]), (1, [0b00000100])], true),
        (vec![(0, [0b00000000]), (1, [0b11111111])], false),
    ];

    (c, tests)
}

pub fn subclocking() -> (Constraint<usize>, TestSet<1>) {
    let c = Subclocking { sub: 0, sup: 1 }.into();

    let variants: TestSet<1> = vec![
        (vec![(0, [0b00000000]), (1, [0b00000000])], true),
        (vec![(0, [0b11111111]), (1, [0b11111111])], true),
        (vec![(0, [0b11110000]), (1, [0b11111111])], true),
        (vec![(0, [0b00001111]), (1, [0b11110000])], false),
    ];
    (c, variants)
}

pub fn exclusion() -> (Constraint<usize>, TestSet<1>) {
    let c = Exclusion {
        clocks: [0, 1].into(),
    }
    .into();

    let variants: TestSet<1> = vec![
        (vec![(0, [0b00000000]), (1, [0b00000000])], true),
        (vec![(0, [0b11111111]), (1, [0b11111111])], false),
        (vec![(0, [0b10101010]), (1, [0b01010101])], true),
        (vec![(0, [0b01010101]), (1, [0b10101010])], true),
        (vec![(0, [0b00001111]), (1, [0b11110000])], true),
    ];

    (c, variants)
}

pub fn union() -> (Constraint<usize>, TestSet<1>) {
    let c = Union {
        out: 2,
        args: [0, 1].into(),
    }
    .into();

    let variants: TestSet<1> = vec![
        (
            vec![(0, [0b00000000]), (1, [0b00000000]), (2, [0b00000000])],
            true,
        ),
        (
            vec![(0, [0b00011111]), (1, [0b10111010]), (2, [0b10111111])],
            true,
        ),
        (
            vec![(0, [0b01000000]), (1, [0b00000010]), (2, [0b10111101])],
            false,
        ),
        (
            vec![(0, [0b00000000]), (1, [0b00000000]), (2, [0b11111111])],
            false,
        ),
        (
            vec![(0, [0b11111111]), (1, [0b00000000]), (2, [0b00000000])],
            false,
        ),
        (
            vec![(0, [0b00000000]), (1, [0b11111111]), (2, [0b00000000])],
            false,
        ),
    ];

    (c, variants)
}

pub fn intersection() -> (Constraint<usize>, TestSet<1>) {
    let c = Intersection {
        out: 2,
        args: [0, 1].into(),
    }
    .into();

    let variants: TestSet<1> = vec![
        (
            vec![(0, [0b00000000]), (1, [0b00000000]), (2, [0b00000000])],
            true,
        ),
        (
            vec![(0, [0b00011111]), (1, [0b10111010]), (2, [0b00011010])],
            true,
        ),
        (
            vec![(0, [0b11111000]), (1, [0b00011111]), (2, [0b00011000])],
            true,
        ),
        (
            vec![(0, [0b00000000]), (1, [0b00011111]), (2, [0b10011000])],
            false,
        ),
    ];

    (c, variants)
}

pub fn minus() -> (Constraint<usize>, TestSet<1>) {
    let c = Minus {
        out: 2,
        left: 0,
        right: 1,
    }
    .into();

    let variants: TestSet<1> = vec![
        (
            vec![(0, [0b00000000]), (1, [0b00000000]), (2, [0b00000000])],
            true,
        ),
        (
            vec![(0, [0b11111111]), (1, [0b11111111]), (2, [0b00000000])],
            true,
        ),
        (
            vec![(0, [0b11110000]), (1, [0b11111111]), (2, [0b00000000])],
            true,
        ),
        (
            vec![(0, [0b11110000]), (1, [0b00001111]), (2, [0b11110000])],
            true,
        ),
        (
            vec![(0, [0b00000000]), (1, [0b11111111]), (2, [0b11111111])],
            false,
        ),
        (
            vec![(0, [0b00000000]), (1, [0b00000000]), (2, [0b11111111])],
            false,
        ),
        (
            vec![(0, [0b11111111]), (1, [0b00000000]), (2, [0b11111111])],
            true,
        ),
    ];

    (c, variants)
}

pub fn delay() -> (Constraint<usize>, TestSet<1>) {
    let c = Delay {
        out: 1,
        trigger: 0,
        delay: 4,
        on: None,
    }
    .into();

    let variants: TestSet<1> = vec![
        (vec![(0, [0b00000000]), (1, [0b00000000])], true),
        (vec![(0, [0b11111111]), (1, [0b00001111])], true),
        (vec![(0, [0b11111111]), (1, [0b00000000])], false),
        (vec![(0, [0b11111111]), (1, [0b11111111])], false),
        (vec![(0, [0b00000000]), (1, [0b11111111])], false),
        (vec![(0, [0b11010101]), (1, [0b00000001])], true),
    ];

    (c, variants)
}
pub fn delay_on() -> (Constraint<usize>, TestSet<1>) {
    let c = Delay {
        out: 0,
        trigger: 1,
        delay: 2,
        on: Some(2),
    }
    .into();

    let variants: TestSet<1> = vec![
        (
            vec![(0, [0b00000000]), (1, [0b00000000]), (2, [0b00000000])],
            true,
        ),
        (
            vec![(0, [0b00000101]), (1, [0b11111111]), (2, [0b00110101])],
            true,
        ),
    ];

    (c, variants)
}

pub fn all() -> Vec<(Constraint<usize>, TestSet<1>)> {
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
    ]
}
