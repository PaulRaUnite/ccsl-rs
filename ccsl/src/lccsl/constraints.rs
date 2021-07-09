use std::collections::BTreeSet;
use std::convert::TryFrom;
use std::fmt;
use std::fmt::{Debug, Display, Formatter};
use std::hash::Hash;
use std::iter::once;

use derive_more::{From, Into};
use itertools::Itertools;

use crate::lccsl::automata::{Delta, Label, STSBuilder, State, STS};
use crate::lccsl::expressions::{BooleanExpression, IntegerExpression};
use crate::{tr, trigger, trigger_value};
use std::ops::BitOr;

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

impl<C> Causality<C> {
    pub(crate) fn map<B, F>(&self, f: &mut F) -> Causality<B>
    where
        F: FnMut(&C) -> B,
    {
        Causality {
            left: f(&self.left),
            right: f(&self.right),
            init: self.init,
            max: self.max,
        }
    }
}

#[derive(Debug, Copy, Clone, Hash)]
pub struct Precedence<C> {
    pub left: C,
    pub right: C,
    pub init: Option<usize>,
    pub max: Option<usize>,
}

impl<C> Precedence<C> {
    pub(crate) fn map<B, F>(&self, f: &mut F) -> Precedence<B>
    where
        F: FnMut(&C) -> B,
    {
        Precedence {
            left: f(&self.left),
            right: f(&self.right),
            init: self.init,
            max: self.max,
        }
    }
}

#[derive(Debug, Clone, Hash)]
pub struct Exclusion<C> {
    pub clocks: BTreeSet<C>,
}

impl<C> Exclusion<C> {
    pub(crate) fn map<B, F>(&self, f: &mut F) -> Exclusion<B>
    where
        F: FnMut(&C) -> B,
        B: Ord,
    {
        Exclusion {
            clocks: self.clocks.iter().map(f).collect(),
        }
    }
}

#[derive(Debug, Copy, Clone, Hash)]
pub struct Subclocking<C> {
    pub left: C,
    pub right: C,
}

impl<C> Subclocking<C> {
    pub(crate) fn map<B, F>(&self, f: &mut F) -> Subclocking<B>
    where
        F: FnMut(&C) -> B,
    {
        Subclocking {
            left: f(&self.left),
            right: f(&self.right),
        }
    }
}
#[derive(Debug, Clone, Hash)]
pub struct Union<C> {
    pub out: C,
    pub args: BTreeSet<C>,
}

impl<C> Union<C> {
    pub(crate) fn map<B, F>(&self, f: &mut F) -> Union<B>
    where
        F: FnMut(&C) -> B,
        B: Ord,
    {
        Union {
            out: f(&self.out),
            args: self.args.iter().map(f).collect(),
        }
    }
}

#[derive(Debug, Clone, Hash)]
pub struct Intersection<C> {
    pub out: C,
    pub args: BTreeSet<C>,
}

impl<C> Intersection<C> {
    pub(crate) fn map<B, F>(&self, f: &mut F) -> Intersection<B>
    where
        F: FnMut(&C) -> B,
        B: Ord,
    {
        Intersection {
            out: f(&self.out),
            args: self.args.iter().map(f).collect(),
        }
    }
}

#[derive(Debug, Clone, Hash)]
pub struct Infinity<C> {
    pub out: C,
    pub args: BTreeSet<C>,
}

impl<C> Infinity<C> {
    pub(crate) fn map<B, F>(&self, f: &mut F) -> Infinity<B>
    where
        F: FnMut(&C) -> B,
        B: Ord,
    {
        Infinity {
            out: f(&self.out),
            args: self.args.iter().map(f).collect(),
        }
    }
}
#[derive(Debug, Clone, Hash)]
pub struct Supremum<C> {
    pub out: C,
    pub args: BTreeSet<C>,
}
impl<C> Supremum<C> {
    pub(crate) fn map<B, F>(&self, f: &mut F) -> Supremum<B>
    where
        F: FnMut(&C) -> B,
        B: Ord,
    {
        Supremum {
            out: f(&self.out),
            args: self.args.iter().map(f).collect(),
        }
    }
}

#[derive(Debug, Copy, Clone, Hash)]
pub struct Repeat<C> {
    pub out: C,
    pub every: Option<usize>,
    pub base: C,
    pub from: Option<usize>,
    pub up_to: Option<usize>,
}

impl<C> Repeat<C> {
    pub(crate) fn map<B, F>(&self, f: &mut F) -> Repeat<B>
    where
        F: FnMut(&C) -> B,
    {
        Repeat {
            out: f(&self.out),
            every: self.every,
            base: f(&self.base),
            from: self.from,
            up_to: self.up_to,
        }
    }
}
#[derive(Debug, Copy, Clone, Hash)]
pub struct Filter<C> {
    pub out: C,
    pub base: C,
    pub every: usize,
    pub from: usize,
}

impl<C> Filter<C> {
    pub(crate) fn map<B, F>(&self, f: &mut F) -> Filter<B>
    where
        F: FnMut(&C) -> B,
    {
        Filter {
            out: f(&self.out),
            every: self.every,
            base: f(&self.base),
            from: self.from,
        }
    }
}
#[derive(Debug, Copy, Clone, Hash)]
pub struct Delay<C> {
    pub out: C,
    pub base: C,
    pub delay: usize,
    pub on: Option<C>,
}

impl<C> Delay<C> {
    pub(crate) fn map<B, F>(&self, f: &mut F) -> Delay<B>
    where
        F: FnMut(&C) -> B,
    {
        Delay {
            out: f(&self.out),
            base: f(&self.base),
            delay: self.delay,
            on: self.on.as_ref().map(f),
        }
    }
}
#[derive(Debug, Copy, Clone, Hash)]
pub struct SampleOn<C> {
    pub out: C,
    pub base: C,
    pub on: C,
}

impl<C> SampleOn<C> {
    pub(crate) fn map<B, F>(&self, f: &mut F) -> SampleOn<B>
    where
        F: FnMut(&C) -> B,
    {
        SampleOn {
            out: f(&self.out),
            base: f(&self.base),
            on: f(&self.on),
        }
    }
}
#[derive(Debug, Copy, Clone, Hash)]
pub struct Diff<C> {
    pub out: C,
    pub base: C,
    pub from: usize,
    pub up_to: usize,
}

impl<C> Diff<C> {
    pub(crate) fn map<B, F>(&self, f: &mut F) -> Diff<B>
    where
        F: FnMut(&C) -> B,
    {
        Diff {
            out: f(&self.out),
            base: f(&self.base),
            from: self.from,
            up_to: self.up_to,
        }
    }
}
#[derive(Debug, Clone, Hash)]
pub struct Minus<C> {
    pub out: C,
    pub base: C,
    pub args: BTreeSet<C>,
}

impl<C> Minus<C> {
    pub(crate) fn map<B, F>(&self, f: &mut F) -> Minus<B>
    where
        F: FnMut(&C) -> B,
        B: Ord,
    {
        Minus {
            out: f(&self.out),
            base: f(&self.base),
            args: self.args.iter().map(f).collect(),
        }
    }
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
    SampleOn(SampleOn<C>),
    Diff(Diff<C>),
}

impl<C> From<&'_ Constraint<C>> for STSBuilder<C>
where
    C: Hash + Clone + Ord + fmt::Display,
{
    fn from(c: &Constraint<C>) -> Self {
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
            Constraint::SampleOn(c) => c.into(),
            Constraint::Diff(c) => c.into()
        }
    }
}

impl<C> From<&'_ Coincidence<C>> for STSBuilder<C>
where
    C: Clone + Ord + Hash + fmt::Display,
{
    fn from(c: &Coincidence<C>) -> Self {
        let var = IntegerExpression::var(Delta(c.left.clone(), c.right.clone()));
        let state = State::new(0).with_invariant(var.eq(0));
        let mut system = STSBuilder::new(&c, state.clone());
        tr!(system, &state => &state, {c.left, c.right,});
        tr!(system, &state => &state, {!c.left, !c.right,});

        system
    }
}

impl<C> From<&'_ Alternates<C>> for STSBuilder<C>
where
    C: Clone + Ord + Hash + fmt::Display,
{
    fn from(c: &Alternates<C>) -> Self {
        let var = IntegerExpression::var(Delta(c.left.clone(), c.right.clone()));
        let start = State::new(0).with_invariant(var.eq(0));
        let alt = State::new(1).with_invariant(var.eq(1));
        let mut system = STSBuilder::new(&c, start.clone());

        tr!(system, &start => &alt, {c.left, !c.right,});
        tr!(system, &start => &start, {!c.left, !c.right,});
        tr!(system, &alt => &start, {!c.left, c.right,});
        tr!(system, &alt => &alt, {!c.left, !c.right,});

        system
    }
}

impl<C> From<&'_ Causality<C>> for STSBuilder<C>
where
    C: Clone + Ord + Hash + fmt::Display,
{
    fn from(c: &Causality<C>) -> Self {
        if c.init.is_some() || c.max.is_some() {
            //TODO: causality max-init
        }
        let mut system: STSBuilder<C> = (&Precedence {
            left: c.left.clone(),
            right: c.right.clone(),
            init: None,
            max: None,
        })
            .into();
        let start = system.initial().clone();
        tr!(system, &start => &start, {c.left, c.right,});
        system
    }
}

impl<C> From<&'_ Precedence<C>> for STSBuilder<C>
where
    C: Clone + Ord + Hash + fmt::Display,
{
    fn from(c: &Precedence<C>) -> Self {
        if c.init.is_some() || c.max.is_some() {
            //TODO: precedence max-init
        }
        let var = IntegerExpression::var(Delta(c.left.clone(), c.right.clone()));

        let start = State::new(0).with_invariant(var.eq(0));
        let next = State::new(1).with_invariant(var.more(0));
        let mut system = STSBuilder::new(&c, start.clone());
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

impl<C> From<&'_ Exclusion<C>> for STSBuilder<C>
where
    C: Clone + Ord + Hash + fmt::Display,
{
    fn from(c: &Exclusion<C>) -> Self {
        let start = State::new(0);
        let mut system: STSBuilder<C> = STSBuilder::new(&c, start.clone());

        tr!(system, &start => &start, {});
        for clock in &c.clocks {
            tr!(system, &start => &start, {clock.clone(),});
        }
        system
    }
}

impl<C> From<&'_ Subclocking<C>> for STSBuilder<C>
where
    C: Clone + Ord + Hash + fmt::Display,
{
    fn from(c: &Subclocking<C>) -> Self {
        let var = IntegerExpression::var(Delta(c.right.clone(), c.left.clone()));
        let start = State::new(0).with_invariant(var.more_eq(0));
        let mut system = STSBuilder::new(&c, start.clone());

        tr!(system, &start => &start, {c.right,});
        tr!(system, &start => &start, {c.left, c.right,});
        tr!(system, &start => &start, {});
        system
    }
}

impl<C> From<&'_ Union<C>> for STSBuilder<C>
where
    C: Clone + Ord + Hash + fmt::Display,
{
    fn from(c: &Union<C>) -> Self {
        let mut invariant = BooleanExpression::Constant(true);
        for clock in c.args.iter() {
            let var = IntegerExpression::var(Delta(c.out.clone(), clock.clone()));
            invariant = invariant & var.more_eq(0);
        }
        let start = State::new(0).with_invariant(invariant);
        let mut system = STSBuilder::new(&c, start.clone());

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

impl<C> From<&'_ Intersection<C>> for STSBuilder<C>
where
    C: Clone + Ord + Hash + fmt::Display,
{
    fn from(c: &Intersection<C>) -> Self {
        let mut invariant = BooleanExpression::Constant(true);
        for clock in c.args.iter() {
            let var = IntegerExpression::var(Delta(c.out.clone(), clock.clone()));
            invariant = invariant & var.less_eq(0);
        }
        let start = State::new(0).with_invariant(invariant);
        let mut system = STSBuilder::new(&c, start.clone());

        system.add_transition(
            &start,
            &start,
            c.args
                .iter()
                .chain(once(&c.out))
                .map(|c| (c.clone(), true))
                .collect_vec(),
        );
        tr!(system, &start => &start, {});
        system
    }
}
impl<C> From<&'_ Delay<C>> for STSBuilder<C>
where
    C: Clone + Ord + Hash + fmt::Display,
{
    fn from(c: &Delay<C>) -> Self {
        let var = IntegerExpression::var(Delta(c.base.clone(), c.out.clone()));
        let start = State::new(0).with_invariant(var.eq(0));
        let mut system = STSBuilder::new(&c, start.clone());

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

impl<C> From<&'_ Infinity<C>> for STSBuilder<C> {
    fn from(_: &Infinity<C>) -> Self {
        todo!()
    }
}

impl<C> From<&'_ Supremum<C>> for STSBuilder<C> {
    fn from(_: &Supremum<C>) -> Self {
        todo!()
    }
}
impl<C> From<&'_ Minus<C>> for STSBuilder<C> {
    fn from(_: &Minus<C>) -> Self {
        todo!()
    }
}
impl<C> From<&'_ Diff<C>> for STSBuilder<C> {
    fn from(_: &Diff<C>) -> Self {
        todo!()
    }
}
impl<C> From<&'_ SampleOn<C>> for STSBuilder<C> {
    fn from(_: &SampleOn<C>) -> Self {
        todo!()
    }
}
impl<C> From<&'_ Filter<C>> for STSBuilder<C> {
    fn from(_: &Filter<C>) -> Self {
        todo!()
    }
}
impl<C> From<&'_ Repeat<C>> for STSBuilder<C> {
    fn from(_: &Repeat<C>) -> Self {
        todo!()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn into_automaton() {
        let a: STSBuilder<&str> = (&Coincidence {
            left: "a",
            right: "b",
        })
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
        match (self.init, self.max) {
            (Some(init), Some(max)) => write!(
                f,
                "{} <= (init: {}, max: {}) {}",
                self.left, init, max, self.right
            ),
            (Some(init), None) => write!(f, "{} <= (init: {}) {}", self.left, init, self.right),
            (None, Some(max)) => write!(f, "{} <= (max: {}) {}", self.left, max, self.right),
            (None, None) => write!(f, "{} <= {}", self.left, self.right),
        }
    }
}

impl<C: fmt::Display> fmt::Display for Precedence<C> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match (self.init, self.max) {
            (Some(init), Some(max)) => write!(
                f,
                "{} < (init: {}, max: {}) {}",
                self.left, init, max, self.right
            ),
            (Some(init), None) => write!(f, "{} < (init: {}) {}", self.left, init, self.right),
            (None, Some(max)) => write!(f, "{} < (max: {}) {}", self.left, max, self.right),
            (None, None) => write!(f, "{} < {}", self.left, self.right),
        }
    }
}

impl<C: fmt::Display> fmt::Display for Exclusion<C> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "#({})", self.clocks.iter().join(","))
    }
}

impl<C: fmt::Display> fmt::Display for Subclocking<C> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{} ⊂ {}", self.left, self.right)
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

impl<C, L> From<&'_ Constraint<C>> for STS<C, L>
where
    C: Clone + Ord + Hash + fmt::Display,
    L: Label<C>,
    for<'c, 'd> &'c L: BitOr<&'d L, Output = L>,
{
    fn from(v: &Constraint<C>) -> Self {
        let builder: STSBuilder<C> = v.into();
        builder.into()
    }
}

#[derive(Debug, From, Into, Clone)]
pub struct Specification<C>(Vec<Constraint<C>>);

impl<C> Constraint<C> {
    pub fn map<B, F>(&self, f: &mut F) -> Constraint<B>
    where
        F: FnMut(&C) -> B,
        B: Ord,
    {
        match self {
            Constraint::Causality(c) => c.map(f).into(),
            Constraint::Precedence(c) => c.map(f).into(),
            Constraint::SubClock(c) => c.map(f).into(),
            Constraint::Exclusion(c) => c.map(f).into(),
            Constraint::Infinity(c) => c.map(f).into(),
            Constraint::Supremum(c) => c.map(f).into(),
            Constraint::Union(c) => c.map(f).into(),
            Constraint::Intersection(c) => c.map(f).into(),
            Constraint::Minus(c) => c.map(f).into(),
            Constraint::Repeat(c) => c.map(f).into(),
            Constraint::Delay(c) => c.map(f).into(),
            Constraint::SampleOn(c) => c.map(f).into(),
            Constraint::Diff(c) => c.map(f).into(),
        }
    }

    pub fn rank(&self) -> usize {
        match self {
            // TODO: define more clearly the ranking
            Constraint::Causality(_) => 2,
            Constraint::Precedence(_) => 2,
            Constraint::SubClock(_) => 1,
            Constraint::Exclusion(_) => 0,
            Constraint::Infinity(_) => 3,
            Constraint::Supremum(_) => 3,
            Constraint::Union(_) => 2,
            Constraint::Intersection(_) => 0,
            Constraint::Minus(_) => 2,
            Constraint::Repeat(_) => 5,
            Constraint::Delay(_) => 0,
            Constraint::SampleOn(_) => 1,
            Constraint::Diff(_) => 1,
        }
    }

    pub fn clocks(&self) -> Vec<&C> {
        match self {
            Constraint::Causality(c) => vec![&c.left, &c.right],
            Constraint::Precedence(c) => vec![&c.left, &c.right],
            Constraint::SubClock(c) => vec![&c.left, &c.right],
            Constraint::Exclusion(c) => c.clocks.iter().collect_vec(),
            Constraint::Infinity(c) => c.args.iter().chain(once(&c.out)).collect_vec(),
            Constraint::Supremum(c) => c.args.iter().chain(once(&c.out)).collect_vec(),
            Constraint::Union(c) => c.args.iter().chain(once(&c.out)).collect_vec(),
            Constraint::Intersection(c) => c.args.iter().chain(once(&c.out)).collect_vec(),
            Constraint::Minus(c) => c.args.iter().chain(once(&c.out)).collect_vec(),
            Constraint::Repeat(c) => vec![&c.out, &c.base],
            Constraint::Delay(c) => {
                c.on.iter()
                    .chain(once(&c.out))
                    .chain(once(&c.base))
                    .collect_vec()
            }
            Constraint::SampleOn(c) => vec![&c.out, &c.base, &c.on],
            Constraint::Diff(c) => vec![&c.out, &c.base],
        }
    }

    pub fn to_lccsl(&self) -> String
    where
        C: Display,
    {
        match self {
            Constraint::Causality(c) => format!("Precedence {} <= {}", c.left, c.right),
            Constraint::Precedence(c) => format!("Precedence {} < {}", c.left, c.right),
            Constraint::SubClock(c) => format!("SubClocking {} <- {}", c.left, c.right),
            Constraint::Exclusion(c) => format!("Exclusion {}", c.clocks.iter().join(" # ")),
            Constraint::Infinity(c) => format!("Let {} be inf({})", c.out, c.args.iter().join(",")),
            Constraint::Supremum(c) => format!("Let {} be sup({})", c.out, c.args.iter().join(",")),
            Constraint::Union(c) => format!("Let {} be {}", c.out, c.args.iter().join(" + ")),
            Constraint::Intersection(c) => {
                format!("Let {} be {}", c.out, c.args.iter().join(" * "))
            }
            Constraint::Minus(c) => {
                format!(
                    "Let {} be {} - {}",
                    c.out,
                    c.base,
                    c.args.iter().join(" - ")
                )
            }
            Constraint::Repeat(c) => format!(
                "repeat {} every {} {} {} {}",
                c.out,
                c.every.map_or("".to_owned(), |v| v.to_string()),
                c.base,
                c.from.map_or("".to_owned(), |v| format!("from {}", v)),
                c.up_to.map_or("".to_owned(), |v| format!("upTo {}", v))
            ),
            Constraint::Delay(c) => format!("{} = {} $ {}", c.out, c.base, c.delay),
            Constraint::SampleOn(c) => format!("{} = {} sampleOn {}", c.out, c.base, c.on),
            Constraint::Diff(c) => format!("{} = {} [{}, {}]", c.out, c.base, c.from, c.up_to),
        }
    }
}

impl<C> Specification<C> {
    pub fn map<B, F>(&self, f: &mut F) -> Specification<B>
    where
        F: FnMut(&C) -> B,
        B: Ord,
    {
        Specification(self.0.iter().map(move |c| c.map(f)).collect())
    }
}
