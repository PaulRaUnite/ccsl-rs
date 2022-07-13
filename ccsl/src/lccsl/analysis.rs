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
use std::collections::{BTreeMap, BTreeSet, HashMap};
use std::hash::Hash;

#[derive(Debug, Clone, Default)]
struct ConstraintEffects<C> {
    clocks: Vec<C>,
    invariant: Option<BooleanExpression<Variable<C>>>,
}

#[derive(Debug, Clone, Default)]
struct ProgramEffects<C> {
    clocks: Vec<C>,
    counters: Vec<Delta<C>>,
    invariant: Option<BooleanExpression<Variable<C>>>,
}

// TODO: move clock variables as variables in boolean expressions
#[derive(Debug, Clone, From)]
enum Variable<C> {
    D(Delta<C>),
    V(C),
}

impl<C: Ord + Clone> From<&'_ Specification<C>> for ProgramEffects<C> {
    fn from(spec: &'_ Specification<C>) -> Self {
        let mut clocks = BTreeSet::new();
        let mut counters = BTreeSet::new();
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
            let mut variables = vec![];
            invariant.0.leaves(&mut variables, &mut vec![], &mut vec![]);
            for v in variables {
                match v {
                    Variable::D(delta) => {
                        counters.insert(delta);
                    }
                    Variable::V(clock) => {
                        clocks.insert(clock);
                    }
                }
            }
            invariants.push(invariant);
        }
        ProgramEffects {
            clocks: clocks.into_iter().collect(),
            counters: counters.into_iter().collect(),
            invariant: invariants.into_iter().map(|i| i.0).reduce(|x, y| x & y),
        }
    }
}

#[derive(Debug, Clone, From)]
struct Invariant<C>(BooleanExpression<Variable<C>>);

impl<C: Clone> From<&'_ Causality<C>> for Invariant<C> {
    fn from(c: &'_ Causality<C>) -> Self {
        IntegerExpression::var(Delta(c.left.clone(), c.right.clone()).into())
            .more_eq(0)
            .into()
    }
}
impl<C: Clone> From<&'_ Precedence<C>> for Invariant<C> {
    fn from(c: &'_ Precedence<C>) -> Self {
        let ab = IntegerExpression::var(Delta(c.left.clone(), c.right.clone()).into());
        let b = IntegerExpression::var(c.right.clone().into());
        ab.eq(0).implies(b.eq(0)).into()
    }
}

impl<C: Clone> From<&'_ Subclocking<C>> for Invariant<C> {
    fn from(c: &'_ Subclocking<C>) -> Self {
        let a = IntegerExpression::var(c.left.clone().into());
        let b = IntegerExpression::var(c.right.clone().into());
        a.eq(1).implies(b.eq(1)).into()
    }
}

impl<C: Clone> From<&'_ Exclusion<C>> for Invariant<C> {
    fn from(c: &'_ Exclusion<C>) -> Self {
        c.clocks
            .iter()
            .tuple_combinations::<(_, _)>()
            .map(|(x, y)| {
                let x = IntegerExpression::var(Variable::V(x.clone()));
                let y = IntegerExpression::var(Variable::V(y.clone()));
                !(x.eq(1) & y.eq(1))
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
            .map(|x| IntegerExpression::var(Variable::V(x)).eq(1))
            .reduce(|x, y| x | y)
            .map(|e| e.strictly_implies(IntegerExpression::var(Variable::V(c.out.clone())).eq(1)))
            .unwrap()
            .into()
    }
}
impl<C: Clone> From<&'_ Intersection<C>> for Invariant<C> {
    fn from(c: &'_ Intersection<C>) -> Self {
        c.args
            .iter()
            .cloned()
            .map(|x| IntegerExpression::var(Variable::V(x)).eq(1))
            .reduce(|x, y| x & y)
            .map(|e| e.strictly_implies(IntegerExpression::var(Variable::V(c.out.clone())).eq(1)))
            .unwrap()
            .into()
    }
}
impl<C: Clone> From<&'_ Minus<C>> for Invariant<C> {
    fn from(c: &'_ Minus<C>) -> Self {
        let a = IntegerExpression::var(Variable::V(c.left.clone()));
        let b = IntegerExpression::var(Variable::V(c.right.clone()));
        let c = IntegerExpression::var(Variable::V(c.out.clone()));
        Invariant((a.eq(1) & b.eq(0)).eq(c.eq(1)))
    }
}

impl<C: Clone> From<&'_ Repeat<C>> for Invariant<C> {
    fn from(c: &'_ Repeat<C>) -> Self {
        unimplemented!()
    }
}
impl<C: Clone> From<&'_ Delay<C>> for Invariant<C> {
    fn from(c: &'_ Delay<C>) -> Self {
        let ab = IntegerExpression::var(Variable::D(Delta(c.base.clone(), c.out.clone())));
        let a = IntegerExpression::var(Variable::V(c.base.clone()));
        let b = IntegerExpression::var(Variable::V(c.out.clone()));
        let d = c.delay as i64;
        Invariant(
            ab.less_eq(d)
                & ab.more_eq(0)
                & ab.eq(d).implies(a.eq(1).eq(b.eq(1)))
                & ab.less(d).implies(b.eq(0)),
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
