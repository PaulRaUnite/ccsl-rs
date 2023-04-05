use crate::interpretation::boolean::Bool;
use crate::interpretation::interval::{Interval, IntervalImmediateNarrowing, StandardWidening};
use crate::interpretation::{Lattice, SequenceLimiter};
use crate::lccsl::analysis::{assume, interpret, ExecutionState, Invariant, StateWidening, Step};
use kernel::automata::Delta;
use kernel::constraints::{
    Causality, Delay, Exclusion, Intersection, Minus, Precedence, Subclocking, Union,
};
use kernel::expressions::{BooleanExpression, IntegerExpression};
use lccsl::parser::{parse_to_string, ParseError};
use map_macro::map;
use std::collections::{BTreeMap, HashMap};

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

#[test]
fn test_assume() {
    let a = "a";
    let b = "b";
    let c = "c";
    let av = BooleanExpression::var(a);
    let bv = BooleanExpression::var(b);
    let cv = BooleanExpression::var(c);
    let abv = IntegerExpression::var(Delta(a, b));
    // let bcv = IntegerExpression::var(Delta(b, c));
    let table = vec![
        (
            !av.clone(),
            (map! {}, map! {a => Bool::False}),
            (map! {}, map! {a => Bool::True}),
        ),
        (
            abv.more_eq(0) & abv.less_eq(0),
            (map! {Delta(a,b) => 0.into() }, map! {}),
            (map! {Delta(a,b) => Interval::top() }, map! {}),
        ),
        (
            abv.more_eq(0) | abv.less_eq(0),
            (map! {Delta(a,b) => Interval::top() }, map! {}),
            (map! {Delta(a,b) => Interval::bottom() }, map! {}),
        ),
        (
            abv.more(0) | abv.less(0),
            (map! {Delta(a,b) => Interval::top() }, map! {}),
            (map! {Delta(a,b) => 0.into() }, map! {}),
        ),
        (
            abv.eq(0).implies(!bv),
            (
                map! {Delta(a,b) => Interval::top() },
                map! {b => Bool::Both },
            ),
            (map! {Delta(a,b) => 0.into() }, map! {b => Bool::True}),
        ),
        (
            abv.more_eq(0).eq(abv.eq(0)),
            (map! {Delta(a,b) => (..=0).into() }, map! {}),
            (map! {Delta(a,b) => (..=0).into() }, map! {}),
        ),
        (
            (av & !cv).eq(abv.more_eq(0)).eq(abv.eq(0)),
            (
                map! {Delta(a,b) => Interval::top() },
                map! {a => Bool::Both, c => Bool::Both},
            ),
            (
                map! {Delta(a,b) => Interval::top() },
                map! {a => Bool::Both, c => Bool::Both},
            ),
        ),
    ];

    type C = &'static str;

    for case in table {
        let (inv, expected_true, expected_false): (
            BooleanExpression<Delta<C>, C>,
            (HashMap<Delta<C>, Interval<i64>>, HashMap<C, Bool>),
            (HashMap<Delta<C>, Interval<i64>>, HashMap<C, Bool>),
        ) = case;
        assert_eq!(assume(&inv, true), ExecutionState::from(expected_true));
        assert_eq!(assume(&inv, false), ExecutionState::from(expected_false));
    }
}

#[test]
fn test_assume_equivalent() {
    let a = "a";
    let b = "b";
    let av = BooleanExpression::var(a);
    let abv = IntegerExpression::var(Delta(a, b));
    let invariant1 = !abv.eq(0).implies(!av.clone());
    let invariant2 = abv.eq(0) & av;

    assert_eq!(assume(&invariant1, true), assume(&invariant2, true));
    assert_eq!(assume(&invariant1, false), assume(&invariant2, false));
}

#[test]
fn test_state_widening() {
    let a = "a";
    let b = "b";
    let table = vec![
        (
            map! {Delta(a,b) => 0.into()},
            map! {Delta(a,b) => 1.into() },
            map! {Delta(a,b) => (0..).into() },
        ),
        (
            map! {Delta(a,b) => (0..).into()},
            map! {Delta(a,b) => (3..).into() },
            map! {Delta(a,b) => (0..).into() },
        ),
        (
            map! {Delta(a,b) => (0..=3).into()},
            map! {Delta(a,b) => (2..=3).into() },
            map! {Delta(a,b) => (0..=3).into() },
        ),
    ];
    type C = &'static str;
    let mut widening = StateWidening::<C, StandardWidening<i64>>::default();

    for case in table {
        let (prev, curr, expected): (
            HashMap<Delta<C>, Interval<i64>>,
            HashMap<Delta<C>, Interval<i64>>,
            HashMap<Delta<C>, Interval<i64>>,
        ) = case;
        let prev = ExecutionState::from((prev, HashMap::new()));
        let curr = ExecutionState::from((curr, HashMap::new()));
        assert_eq!(
            widening.deduct(&prev, &curr),
            ExecutionState::from((expected, HashMap::new()))
        );
    }
}

#[test]
fn test_spec_invariant() {
    let table = vec![(
        "Precedence a < b\n Precedence b < c\nc = a $ 1",
        (
            map! {
                Delta("a", "b") => (0..=1).into(),
                Delta("b", "c") => (0..=1).into(),
                Delta("a", "c") => (0..=1).into(),
            },
            map! {
                "a" => Bool::Both,
                "b" => Bool::Both,
                "c" => Bool::Both,
            },
        ),
    )];
    let table = table
        .into_iter()
        .map(|(spec, (int, bool))| -> Result<_, ParseError> {
            Ok((
                parse_to_string(&format!(
                    "Specification test {{\nClock a,b,c [\n{}\n]\n}}",
                    spec
                ))?
                .into(),
                ExecutionState::from((
                    int.into_iter()
                        .map(|(k, v)| (Delta(k.0.to_string(), k.1.to_string()), v))
                        .collect(),
                    bool.into_iter().map(|(k, v)| (k.to_string(), v)).collect(),
                )),
            ))
        })
        .collect::<Result<Vec<_>, _>>()
        .unwrap();

    for (spec, expected) in table {
        let state = &interpret::<
            String,
            StateWidening<String, StandardWidening<i64>>,
            StateWidening<String, IntervalImmediateNarrowing<i64>>,
        >(&spec)[&Step::If];
        println!("{}", state);
        assert!(expected.subset(state));
    }
}
