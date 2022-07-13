use crate::interpretation::interval::Interval;
use crate::interpretation::ValueDomain;
use crate::lccsl::automata::Delta;
use crate::lccsl::constraints::{
    Causality, Constraint, Delay, Diff, Exclusion, Infinity, Intersection, Minus, Precedence,
    Repeat, SampleOn, Specification, Subclocking, Supremum, Union,
};
use crate::lccsl::expressions::{BooleanExpression, IntegerExpression};
use derive_more::From;
use itertools::Itertools;
use std::collections::{BTreeMap, HashMap};
use std::hash::Hash;

#[derive(Debug, Clone, Default)]
struct ProgramEffects<C> {
    clocks: Vec<C>,
    counters: Vec<Delta<C>>,
    invariant: Option<BooleanExpression<Delta<C>, C>>,
}

impl<C: Eq + Hash + Clone> From<&'_ Specification<C>> for ProgramEffects<C> {
    fn from(spec: &'_ Specification<C>) -> Self {
        let mut clocks = vec![];
        let mut counters = vec![];
        let mut invariants = vec![];
        for c in spec {
            let invariant: Invariant<C> = match c {
                Constraint::Causality(c) => c.into(),
                Constraint::Precedence(c) => c.into(),
                Constraint::SubClock(c) => c.into(),
                Constraint::Exclusion(c) => c.into(),
                Constraint::Infinity(c) => c.into(),
                Constraint::Supremum(c) => c.into(),
                Constraint::Union(c) => c.into(),
                Constraint::Intersection(c) => c.into(),
                Constraint::Minus(c) => c.into(),
                Constraint::Repeat(c) => c.into(),
                Constraint::Delay(c) => c.into(),
                Constraint::SampleOn(c) => c.into(),
                Constraint::Diff(c) => c.into(),
            };
            invariant
                .0
                .leaves(&mut counters, &mut clocks, &mut vec![], &mut vec![]);
            invariants.push(invariant);
        }
        ProgramEffects {
            clocks: clocks.into_iter().unique().collect(),
            counters: counters.into_iter().unique().collect(),
            invariant: invariants.into_iter().map(|i| i.0).reduce(|x, y| x & y),
        }
    }
}

#[derive(Debug, Clone, From)]
struct Invariant<C>(BooleanExpression<Delta<C>, C>);

impl<C: Clone> From<&'_ Causality<C>> for Invariant<C> {
    fn from(c: &'_ Causality<C>) -> Self {
        IntegerExpression::var(Delta(c.left.clone(), c.right.clone()))
            .more_eq(0)
            .into()
    }
}
impl<C: Clone> From<&'_ Precedence<C>> for Invariant<C> {
    fn from(c: &'_ Precedence<C>) -> Self {
        let ab = IntegerExpression::var(Delta(c.left.clone(), c.right.clone()));
        let b = BooleanExpression::var(c.right.clone());
        ab.eq(0).implies(b).into()
    }
}

impl<C: Clone> From<&'_ Subclocking<C>> for Invariant<C> {
    fn from(c: &'_ Subclocking<C>) -> Self {
        let a = BooleanExpression::var(c.left.clone());
        let b = BooleanExpression::var(c.right.clone());
        a.implies(b).into()
    }
}

impl<C: Clone> From<&'_ Exclusion<C>> for Invariant<C> {
    fn from(c: &'_ Exclusion<C>) -> Self {
        c.clocks
            .iter()
            .tuple_combinations::<(_, _)>()
            .map(|(x, y)| {
                let x = BooleanExpression::var(x.clone());
                let y = BooleanExpression::var(y.clone());
                !(x & y)
            })
            .reduce(|x, y| x & y)
            .unwrap()
            .into()
    }
}
impl<C: Clone> From<&'_ Infinity<C>> for Invariant<C> {
    fn from(c: &'_ Infinity<C>) -> Self {
        unimplemented!()
    }
}
impl<C: Clone> From<&'_ Supremum<C>> for Invariant<C> {
    fn from(c: &'_ Supremum<C>) -> Self {
        unimplemented!()
    }
}
impl<C: Clone> From<&'_ Union<C>> for Invariant<C> {
    fn from(c: &'_ Union<C>) -> Self {
        c.args
            .iter()
            .cloned()
            .map(BooleanExpression::var)
            .reduce(|x, y| x | y)
            .map(|e| e.strictly_implies(BooleanExpression::var(c.out.clone())))
            .unwrap()
            .into()
    }
}
impl<C: Clone> From<&'_ Intersection<C>> for Invariant<C> {
    fn from(c: &'_ Intersection<C>) -> Self {
        c.args
            .iter()
            .cloned()
            .map(BooleanExpression::var)
            .reduce(|x, y| x & y)
            .map(|e| e.strictly_implies(BooleanExpression::var(c.out.clone())))
            .unwrap()
            .into()
    }
}
impl<C: Clone> From<&'_ Minus<C>> for Invariant<C> {
    fn from(c: &'_ Minus<C>) -> Self {
        let a = BooleanExpression::var(c.left.clone());
        let b = BooleanExpression::var(c.right.clone());
        let c = BooleanExpression::var(c.out.clone());
        Invariant((a & !b).eq(c))
    }
}

impl<C: Clone> From<&'_ Repeat<C>> for Invariant<C> {
    fn from(c: &'_ Repeat<C>) -> Self {
        unimplemented!()
    }
}
impl<C: Clone> From<&'_ Delay<C>> for Invariant<C> {
    fn from(c: &'_ Delay<C>) -> Self {
        let ab = IntegerExpression::var(Delta(c.base.clone(), c.out.clone()));
        let a = BooleanExpression::var(c.base.clone());
        let b = BooleanExpression::var(c.out.clone());
        let d = c.delay as i64;
        Invariant(
            ab.less_eq(d)
                & ab.more_eq(0)
                & ab.eq(d).implies(a.eq(b.clone()))
                & ab.less(d).implies(!b),
        )
    }
}
impl<C: Clone> From<&'_ SampleOn<C>> for Invariant<C> {
    fn from(c: &'_ SampleOn<C>) -> Self {
        unimplemented!()
    }
}
impl<C: Clone> From<&'_ Diff<C>> for Invariant<C> {
    fn from(c: &'_ Diff<C>) -> Self {
        unimplemented!()
    }
}
