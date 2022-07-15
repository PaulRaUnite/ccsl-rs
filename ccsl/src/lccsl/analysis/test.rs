use crate::lccsl::analysis::Invariant;
use crate::lccsl::constraints::{
    Causality, Delay, Exclusion, Intersection, Minus, Precedence, Subclocking, Union,
};
use std::collections::BTreeMap;

#[test]
fn test_causality() {
    let c: Invariant<usize> = (&Causality {
        left: 0,
        right: 1,
        init: None,
        max: None,
    })
        .into();

    let variants: Vec<(BTreeMap<usize, [u8; 1]>, bool)> = vec![
        (
            [(0, [0b11111111]), (1, [0b11111110])].into_iter().collect(),
            true,
        ),
        (
            [(0, [0b11111111]), (1, [0b11111111])].into_iter().collect(),
            true,
        ),
        (
            [(0, [0b10101010]), (1, [0b11110000])].into_iter().collect(),
            true,
        ),
        (
            [(0, [0b10101010]), (1, [0b01010100])].into_iter().collect(),
            true,
        ),
        (
            [(0, [0b00001000]), (1, [0b00010000])].into_iter().collect(),
            true,
        ),
        (
            [(0, [0b10101010]), (1, [0b01010101])].into_iter().collect(),
            false,
        ),
        (
            [(0, [0b00001000]), (1, [0b00001000])].into_iter().collect(),
            true,
        ),
    ];

    for (traces, expect) in variants {
        assert_eq!(c.check(traces), expect);
    }
}

#[test]
fn test_precedence() {
    let c: Invariant<usize> = (&Precedence {
        left: 0,
        right: 1,
        init: None,
        max: None,
    })
        .into();

    let variants: Vec<(BTreeMap<usize, [u8; 1]>, bool)> = vec![
        (
            [(0, [0b11111111]), (1, [0b11111110])].into_iter().collect(),
            true,
        ),
        (
            [(0, [0b11111111]), (1, [0b11111111])].into_iter().collect(),
            false,
        ),
        (
            [(0, [0b10101010]), (1, [0b01110000])].into_iter().collect(),
            true,
        ),
        (
            [(0, [0b10101010]), (1, [0b01010100])].into_iter().collect(),
            true,
        ),
        (
            [(0, [0b00001000]), (1, [0b00010000])].into_iter().collect(),
            true,
        ),
        (
            [(0, [0b10101010]), (1, [0b01010101])].into_iter().collect(),
            false,
        ),
        (
            [(0, [0b11111111]), (1, [0b00000000])].into_iter().collect(),
            true,
        ),
        (
            [(0, [0b00000000]), (1, [0b11111111])].into_iter().collect(),
            false,
        ),
    ];

    for (traces, expect) in variants {
        assert_eq!(c.check(traces), expect);
    }
}

#[test]
fn test_subclocking() {
    let c: Invariant<usize> = (&Subclocking { left: 0, right: 1 }).into();

    let variants: Vec<(BTreeMap<usize, [u8; 1]>, bool)> = vec![
        ([(0, [0b00000000]), (1, [0b00000000])].into(), true),
        ([(0, [0b11111111]), (1, [0b11111111])].into(), true),
        ([(0, [0b11110000]), (1, [0b11111111])].into(), true),
        ([(0, [0b00001111]), (1, [0b11110000])].into(), false),
    ];

    for (traces, expect) in variants {
        assert_eq!(c.check(traces), expect);
    }
}

#[test]
fn test_exclusion() {
    let c: Invariant<usize> = (&Exclusion {
        clocks: [0, 1].into(),
    })
        .into();

    let variants: Vec<(BTreeMap<usize, [u8; 1]>, bool)> = vec![
        ([(0, [0b00000000]), (1, [0b00000000])].into(), true),
        ([(0, [0b11111111]), (1, [0b11111111])].into(), false),
        ([(0, [0b10101010]), (1, [0b01010101])].into(), true),
        ([(0, [0b01010101]), (1, [0b10101010])].into(), true),
    ];

    for (traces, expect) in variants {
        assert_eq!(c.check(traces), expect);
    }
}

#[test]
fn test_union() {
    let c: Invariant<usize> = (&Union {
        out: 2,
        args: [0, 1].into(),
    })
        .into();

    let variants: Vec<(BTreeMap<usize, [u8; 1]>, bool)> = vec![
        (
            [(0, [0b00000000]), (1, [0b00000000]), (2, [0b00000000])].into(),
            true,
        ),
        (
            [(0, [0b00011111]), (1, [0b10111010]), (2, [0b10111111])].into(),
            true,
        ),
        (
            [(0, [0b01000000]), (1, [0b00000010]), (2, [0b10111101])].into(),
            false,
        ),
        (
            [(0, [0b00000000]), (1, [0b00000000]), (2, [0b11111111])].into(),
            false,
        ),
        (
            [(0, [0b11111111]), (1, [0b00000000]), (2, [0b00000000])].into(),
            false,
        ),
        (
            [(0, [0b00000000]), (1, [0b11111111]), (2, [0b00000000])].into(),
            false,
        ),
    ];

    for (traces, expect) in variants {
        assert_eq!(c.check(traces), expect);
    }
}

#[test]
fn test_intersection() {
    let c: Invariant<usize> = (&Intersection {
        out: 2,
        args: [0, 1].into(),
    })
        .into();

    let variants: Vec<(BTreeMap<usize, [u8; 1]>, bool)> = vec![
        (
            [(0, [0b00000000]), (1, [0b00000000]), (2, [0b00000000])].into(),
            true,
        ),
        (
            [(0, [0b00011111]), (1, [0b10111010]), (2, [0b00011010])].into(),
            true,
        ),
        (
            [(0, [0b11111000]), (1, [0b00011111]), (2, [0b00011000])].into(),
            true,
        ),
        (
            [(0, [0b00000000]), (1, [0b00011111]), (2, [0b10011000])].into(),
            false,
        ),
    ];

    for (traces, expect) in variants {
        assert_eq!(c.check(traces), expect);
    }
}

#[test]
fn test_minus() {
    let c: Invariant<usize> = (&Minus {
        out: 2,
        left: 0,
        right: 1,
    })
        .into();

    let variants: Vec<(BTreeMap<usize, [u8; 1]>, bool)> = vec![
        (
            [(0, [0b00000000]), (1, [0b00000000]), (2, [0b00000000])].into(),
            true,
        ),
        (
            [(0, [0b11111111]), (1, [0b11111111]), (2, [0b00000000])].into(),
            true,
        ),
        (
            [(0, [0b11110000]), (1, [0b11111111]), (2, [0b00000000])].into(),
            true,
        ),
        (
            [(0, [0b11110000]), (1, [0b00001111]), (2, [0b11110000])].into(),
            true,
        ),
        (
            [(0, [0b00000000]), (1, [0b11111111]), (2, [0b11111111])].into(),
            false,
        ),
        (
            [(0, [0b00000000]), (1, [0b00000000]), (2, [0b11111111])].into(),
            false,
        ),
        (
            [(0, [0b11111111]), (1, [0b00000000]), (2, [0b11111111])].into(),
            true,
        ),
    ];

    for (traces, expect) in variants {
        assert_eq!(c.check(traces), expect);
    }
}

#[test]
fn test_delay() {
    let c: Invariant<usize> = (&Delay {
        out: 1,
        base: 0,
        delay: 4,
        on: None,
    })
        .into();

    let variants: Vec<(BTreeMap<usize, [u8; 1]>, bool)> = vec![
        ([(0, [0b00000000]), (1, [0b00000000])].into(), true),
        ([(0, [0b11111111]), (1, [0b11110000])].into(), true),
        ([(0, [0b11111111]), (1, [0b00000000])].into(), false),
        ([(0, [0b11111111]), (1, [0b11111111])].into(), false),
        ([(0, [0b00000000]), (1, [0b11111111])].into(), false),
        ([(0, [0b11010101]), (1, [0b10000000])].into(), true),
    ];

    for (traces, expect) in variants {
        assert_eq!(c.check(traces), expect);
    }
}
