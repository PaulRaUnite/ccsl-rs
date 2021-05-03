use std::collections::BTreeSet;
use std::convert::TryFrom;
use std::fmt;
use std::fmt::{Debug, Formatter};
use std::hash::Hash;
use std::iter::once;

use derive_more::From;
use itertools::Itertools;

use crate::lccsl::automata::{Delta, LabeledTransitionSystem, State, STS};
use crate::lccsl::expressions::{BooleanExpression, IntegerExpression};
use crate::{tr, trigger, trigger_value};

#[derive(Debug, Copy, Clone, Hash)]
pub struct Coincidence<C> {
    pub left: C,
    pub right: C,
}

#[derive(Debug, Copy, Clone, Hash)]
pub struct Alternates<C> {
    pub left: C,
    pub right: C,
}

#[derive(Debug, Copy, Clone, Hash)]
pub struct Causality<C> {
    pub left: C,
    pub right: C,
    pub init: Option<usize>,
    pub max: Option<usize>,
}

#[derive(Debug, Copy, Clone, Hash)]
pub struct Precedence<C> {
    pub left: C,
    pub right: C,
    pub init: Option<usize>,
    pub max: Option<usize>,
}

#[derive(Debug, Clone, Hash)]
pub struct Exclusion<C> {
    pub clocks: BTreeSet<C>,
}

#[derive(Debug, Copy, Clone, Hash)]
pub struct Subclocking<C> {
    pub left: C,
    pub right: C,
}

#[derive(Debug, Clone, Hash)]
pub struct Union<C> {
    pub out: C,
    pub args: BTreeSet<C>,
}

#[derive(Debug, Clone, Hash)]
pub struct Intersection<C> {
    pub out: C,
    pub args: BTreeSet<C>,
}

#[derive(Debug, Clone, Hash)]
pub struct Infinity<C> {
    pub out: C,
    pub args: BTreeSet<C>,
}

#[derive(Debug, Clone, Hash)]
pub struct Supremum<C> {
    pub out: C,
    pub args: BTreeSet<C>,
}

#[derive(Debug, Copy, Clone, Hash)]
pub struct Repeat<C> {
    pub out: C,
    pub every: Option<usize>,
    pub base: C,
    pub from: Option<usize>,
    pub up_to: Option<usize>,
}

#[derive(Debug, Copy, Clone, Hash)]
pub struct Filter<C> {
    pub out: C,
    pub base: C,
    pub every: usize,
    pub from: usize,
}

#[derive(Debug, Copy, Clone, Hash)]
pub struct Delay<C> {
    pub out: C,
    pub base: C,
    pub delay: usize,
    pub on: Option<C>,
}

#[derive(Debug, Copy, Clone, Hash)]
pub struct SampleOn<C> {
    pub out: C,
    pub base: C,
    pub on: Option<C>,
}

#[derive(Debug, Copy, Clone, Hash)]
pub struct Diff<C> {
    pub out: C,
    pub base: C,
    pub from: usize,
    pub up_to: usize,
}

#[derive(Debug, Clone, Hash)]
pub struct Minus<C> {
    pub out: C,
    pub base: C,
    pub args: BTreeSet<C>,
}

#[derive(Debug, Clone, From, Hash)]
pub enum Constraint<C> {
    Causality(Causality<C>),
    Precedence(Precedence<C>),
    SubClock(Subclocking<C>),
    Exclusion(Exclusion<C>),
    Infinity(Infinity<C>),
    Supremum(Supremum<C>),
    Union(Union<C>),
    Intersection(Intersection<C>),
    Minus(Minus<C>),
    Repeat(Repeat<C>),
    Delay(Delay<C>),
}

impl<C> From<Constraint<C>> for STS<C>
where
    C: Hash + Clone + Ord + fmt::Display,
{
    fn from(c: Constraint<C>) -> Self {
        match c {
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
        }
    }
}
//
// impl<C> Constraint<C> {
//     pub fn rank(&self) -> usize {
//         match self {
//             Constraint::Causality(_) => {}
//             Constraint::Precedence(_) => {}
//             Constraint::SubClock(_) => {}
//             Constraint::Exclusion(_) => {}
//             Constraint::Infinity(_) => {}
//             Constraint::Supremum(_) => {}
//             Constraint::Union(_) => {}
//             Constraint::Intersection(_) => {}
//             Constraint::Minus(_) => {}
//             Constraint::Repeat(_) => {}
//             Constraint::Delay(_) => {}
//         }
//     }
// }

impl<C> From<Coincidence<C>> for STS<C>
where
    C: Clone + Ord + Hash + fmt::Display,
{
    fn from(c: Coincidence<C>) -> Self {
        let var = IntegerExpression::var(Delta(c.left.clone(), c.right.clone()));
        let state = State::new(0).with_invariant(var.eq(0));
        let mut system = STS::new(&c, state.clone());
        tr!(system, &state => &state, {c.left, c.right,});
        tr!(system, &state => &state, {!c.left, !c.right,});

        system
    }
}

impl<C> From<Alternates<C>> for STS<C>
where
    C: Clone + Ord + Hash + fmt::Display,
{
    fn from(c: Alternates<C>) -> Self {
        let var = IntegerExpression::var(Delta(c.left.clone(), c.right.clone()));
        let start = State::new(0).with_invariant(var.eq(0));
        let alt = State::new(1).with_invariant(var.eq(1));
        let mut system = STS::new(&c, start.clone());

        tr!(system, &start => &alt, {c.left, !c.right,});
        tr!(system, &start => &start, {!c.left, !c.right,});
        tr!(system, &alt => &start, {!c.left, c.right,});
        tr!(system, &alt => &alt, {!c.left, !c.right,});

        system
    }
}

impl<C> From<Causality<C>> for STS<C>
where
    C: Clone + Ord + Hash + fmt::Display,
{
    fn from(c: Causality<C>) -> Self {
        if c.init.is_some() || c.max.is_some() {
            todo!();
        }
        let mut system: STS<C> = Precedence {
            left: c.left.clone(),
            right: c.right.clone(),
            init: None,
            max: None,
        }
        .into();
        let start = system.initial().clone();
        tr!(system, &start => &start, {c.left, c.right,});
        system
    }
}

impl<C> From<Precedence<C>> for STS<C>
where
    C: Clone + Ord + Hash + fmt::Display,
{
    fn from(c: Precedence<C>) -> Self {
        if c.init.is_some() || c.max.is_some() {
            todo!();
        }
        let var = IntegerExpression::var(Delta(c.left.clone(), c.right.clone()));

        let start = State::new(0).with_invariant(var.eq(0));
        let next = State::new(1).with_invariant(var.more(0));
        let mut system = STS::new(&c, start.clone());
        tr!(system, &start => &next, {c.left, !c.right,});

        tr!(system, &next => &next, {c.left, !c.right,});
        tr!(system, &next => &next, {c.left, c.right,});

        tr!(system, var.eq(1), &next => &start, {!c.left, c.right,});
        tr!(system, var.more(1), &next => &next, {!c.left, c.right,});

        tr!(system, &start => &start, {!c.left, !c.right,});
        tr!(system, &next => &next, {!c.left, !c.right,});
        system
    }
}

impl<C> From<Exclusion<C>> for STS<C>
where
    C: Clone + Ord + Hash + fmt::Display,
{
    fn from(c: Exclusion<C>) -> Self {
        let start = State::new(0);
        let mut system = STS::new(&c, start.clone());

        tr!(system, &start => &start, {});
        for clock in c.clocks {
            tr!(system, &start => &start, {clock,});
        }
        system
    }
}

impl<C> From<Subclocking<C>> for STS<C>
where
    C: Clone + Ord + Hash + fmt::Display,
{
    fn from(c: Subclocking<C>) -> Self {
        let var = IntegerExpression::var(Delta(c.left.clone(), c.right.clone()));
        let start = State::new(0).with_invariant(var.more_eq(0));
        let mut system = STS::new(&c, start.clone());

        tr!(system, &start => &start, {c.left,});
        tr!(system, &start => &start, {c.left, c.right,});
        tr!(system, &start => &start, {});
        system
    }
}

impl<C> From<Union<C>> for STS<C>
where
    C: Clone + Ord + Hash + fmt::Display,
{
    fn from(c: Union<C>) -> Self {
        let mut invariant = BooleanExpression::Constant(true);
        for clock in c.args.iter() {
            let var = IntegerExpression::var(Delta(c.out.clone(), clock.clone()));
            invariant = invariant & var.more_eq(0);
        }
        let start = State::new(0).with_invariant(invariant);
        let mut system = STS::new(&c, start.clone());

        for i in 1..=c.args.len() {
            for comb in c.args.iter().combinations(i) {
                system.add_transition(
                    &start,
                    &start,
                    comb.into_iter()
                        .chain(once(&c.out))
                        .map(|c| (c.clone(), true))
                        .collect_vec(),
                )
            }
        }
        tr!(system, &start => &start, {});
        system
    }
}

impl<C> From<Intersection<C>> for STS<C>
where
    C: Clone + Ord + Hash + fmt::Display,
{
    fn from(c: Intersection<C>) -> Self {
        let mut invariant = BooleanExpression::Constant(true);
        for clock in c.args.iter() {
            let var = IntegerExpression::var(Delta(c.out.clone(), clock.clone()));
            invariant = invariant & var.less_eq(0);
        }
        let start = State::new(0).with_invariant(invariant);
        let mut system = STS::new(&c, start.clone());

        system.add_transition(
            &start,
            &start,
            c.args
                .into_iter()
                .chain(once(c.out))
                .map(|c| (c, true))
                .collect_vec(),
        );
        tr!(system, &start => &start, {});
        system
    }
}
impl<C> From<Delay<C>> for STS<C>
where
    C: Clone + Ord + Hash + fmt::Display,
{
    fn from(c: Delay<C>) -> Self {
        let var = IntegerExpression::var(Delta(c.base.clone(), c.out.clone()));
        let start = State::new(0).with_invariant(var.eq(0));
        let mut system = STS::new(&c, start.clone());

        let mut last = start.clone();
        for i in 1..=c.delay {
            let state = State::new(i).with_invariant(var.eq(i64::try_from(i).unwrap()));
            tr!(system, &last => &state, {c.base,});
            tr!(system, &state => &state, {});
            last = state;
        }
        tr!(system, &last => &last, {c.out, c.base,});
        tr!(system, &start => &start, {});
        system
    }
}

impl<C> From<Infinity<C>> for STS<C> {
    fn from(_: Infinity<C>) -> Self {
        todo!()
    }
}
impl<C> From<Supremum<C>> for STS<C> {
    fn from(_: Supremum<C>) -> Self {
        todo!()
    }
}
impl<C> From<Minus<C>> for STS<C> {
    fn from(_: Minus<C>) -> Self {
        todo!()
    }
}
impl<C> From<Diff<C>> for STS<C> {
    fn from(_: Diff<C>) -> Self {
        todo!()
    }
}
impl<C> From<SampleOn<C>> for STS<C> {
    fn from(_: SampleOn<C>) -> Self {
        todo!()
    }
}
impl<C> From<Filter<C>> for STS<C> {
    fn from(_: Filter<C>) -> Self {
        todo!()
    }
}
impl<C> From<Repeat<C>> for STS<C> {
    fn from(_: Repeat<C>) -> Self {
        todo!()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn into_automaton() {
        let a: STS<&str> = Coincidence {
            left: "a",
            right: "b",
        }
        .into();
        println!("{:?}", a);
        assert_eq!(1, 2)
    }
}

impl<C: fmt::Display> fmt::Display for Coincidence<C> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{} = {}", self.left, self.right)
    }
}

impl<C: fmt::Display> fmt::Display for Alternates<C> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{} ~ {}", self.left, self.right)
    }
}

impl<C: fmt::Display> fmt::Display for Causality<C> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if self.init.is_some() || self.max.is_some() {
            todo!();
        }
        write!(f, "{} <= {}", self.left, self.right)
    }
}

impl<C: fmt::Display> fmt::Display for Precedence<C> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if self.init.is_some() || self.max.is_some() {
            todo!();
        }
        write!(f, "{} < {}", self.left, self.right)
    }
}

impl<C: fmt::Display> fmt::Display for Exclusion<C> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "#({})", self.clocks.iter().join(","))
    }
}

impl<C: fmt::Display> fmt::Display for Subclocking<C> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{} ⊃ {}", self.left, self.right)
    }
}

impl<C: fmt::Display> fmt::Display for Union<C> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{} = ∪({})", self.out, self.args.iter().join(","))
    }
}

impl<C: fmt::Display> fmt::Display for Intersection<C> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{} = ∩({})", self.out, self.args.iter().join(","))
    }
}

impl<C: fmt::Display> fmt::Display for Delay<C> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{} = {}${}", self.out, self.base, self.delay)
    }
}
