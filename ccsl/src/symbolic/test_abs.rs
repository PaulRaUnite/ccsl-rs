use crate::kernel::automata::Delta;
use crate::kernel::expressions::{BooleanExpression, IntegerExpression};
use crate::lccsl::parser::{parse_to_string, ParseError};
use crate::symbolic::{assume, interpret, ExecutionState, StateWidening, Step};
use absint::boolean::Bool;
use absint::interval::{Interval, IntervalImmediateNarrowing, StandardWidening};
use absint::{Lattice, SequenceLimiter};
use map_macro::map;
use std::collections::HashMap;

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
