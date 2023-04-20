use std::collections::BTreeSet;
use std::convert::TryFrom;
use std::fmt;
use std::fmt::{Debug, Display, Formatter};
use std::hash::Hash;
use std::iter::once;

use derive_more::{From, Into};
use itertools::Itertools;

use crate::kernel::automata::label::Label;
use crate::kernel::automata::{Delta, STSBuilder, State, STS};
use crate::kernel::expressions::{BooleanExpression, IntegerExpression};
use crate::{tr, trigger, trigger_value};
use std::cmp::max;
use std::ops::BitOr;
use std::slice::Iter;
use std::vec::IntoIter;

#[derive(Debug, Copy, Clone, Hash)]
pub struct Coincidence<C> {
    pub left: C,
    pub right: C,
}

impl<C> Coincidence<C> {
    pub(crate) fn map<B, F>(&self, mut f: F) -> Coincidence<B>
    where
        F: FnMut(&C) -> B,
    {
        Coincidence {
            left: f(&self.left),
            right: f(&self.right),
        }
    }
}

#[derive(Debug, Copy, Clone, Hash)]
pub struct Alternates<C> {
    pub left: C,
    pub right: C,
}

#[derive(Debug, Copy, Clone, Hash)]
pub struct Causality<C> {
    pub cause: C,
    pub effect: C,
    pub init: Option<usize>,
    pub max: Option<usize>,
}

impl<C> Causality<C> {
    pub(crate) fn map<B, F>(&self, mut f: F) -> Causality<B>
    where
        F: FnMut(&C) -> B,
    {
        Causality {
            cause: f(&self.cause),
            effect: f(&self.effect),
            init: self.init,
            max: self.max,
        }
    }
}

#[derive(Debug, Copy, Clone, Hash)]
pub struct Precedence<C> {
    pub cause: C,
    pub effect: C,
    pub init: Option<usize>,
    pub max: Option<usize>,
}

impl<C> Precedence<C> {
    pub(crate) fn map<B, F>(&self, mut f: F) -> Precedence<B>
    where
        F: FnMut(&C) -> B,
    {
        Precedence {
            cause: f(&self.cause),
            effect: f(&self.effect),
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
    pub(crate) fn map<B, F>(&self, f: F) -> Exclusion<B>
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
    pub sub: C,
    pub sup: C,
}

impl<C> Subclocking<C> {
    pub(crate) fn map<B, F>(&self, mut f: F) -> Subclocking<B>
    where
        F: FnMut(&C) -> B,
    {
        Subclocking {
            sub: f(&self.sub),
            sup: f(&self.sup),
        }
    }
}
#[derive(Debug, Clone, Hash)]
pub struct Union<C> {
    pub out: C,
    pub args: BTreeSet<C>,
}

impl<C> Union<C> {
    pub(crate) fn map<B, F>(&self, mut f: F) -> Union<B>
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
    pub(crate) fn map<B, F>(&self, mut f: F) -> Intersection<B>
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

#[derive(Debug, Copy, Clone, Hash)]
pub struct Infimum<C> {
    pub out: C,
    pub left: C,
    pub right: C,
}

impl<C> Infimum<C> {
    pub(crate) fn map<B, F>(&self, mut f: F) -> Infimum<B>
    where
        F: FnMut(&C) -> B,
        B: Ord,
    {
        Infimum {
            out: f(&self.out),
            left: f(&self.left),
            right: f(&self.right),
        }
    }
}
#[derive(Debug, Clone, Hash)]
pub struct Supremum<C> {
    pub out: C,
    pub left: C,
    pub right: C,
}
impl<C> Supremum<C> {
    pub(crate) fn map<B, F>(&self, mut f: F) -> Supremum<B>
    where
        F: FnMut(&C) -> B,
        B: Ord,
    {
        Supremum {
            out: f(&self.out),
            left: f(&self.left),
            right: f(&self.right),
        }
    }
}

#[derive(Debug, Copy, Clone, Hash)]
pub struct Repeat<C> {
    pub out: C,
    pub every: usize,
    pub base: C,
    pub from: Option<usize>,
    pub up_to: Option<usize>,
}

impl<C> Repeat<C> {
    pub(crate) fn map<B, F>(&self, mut f: F) -> Repeat<B>
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
pub struct Delay<C> {
    pub out: C,
    pub trigger: C,
    pub delay: usize,
    pub on: Option<C>, // to be defaulted to trigger if omitted
}

impl<C> Delay<C> {
    pub(crate) fn map<B, F>(&self, mut f: F) -> Delay<B>
    where
        F: FnMut(&C) -> B,
    {
        Delay {
            out: f(&self.out),
            trigger: f(&self.trigger),
            delay: self.delay,
            on: self.on.as_ref().map(f),
        }
    }
}
#[derive(Debug, Copy, Clone, Hash)]
pub struct SampleOn<C> {
    pub out: C,
    pub base: C,
    pub trigger: C,
}

impl<C> SampleOn<C> {
    pub(crate) fn map<B, F>(&self, mut f: F) -> SampleOn<B>
    where
        F: FnMut(&C) -> B,
    {
        SampleOn {
            out: f(&self.out),
            base: f(&self.base),
            trigger: f(&self.trigger),
        }
    }
}
#[derive(Debug, Copy, Clone, Hash)]
pub struct Slice<C> {
    pub out: C,
    pub base: C,
    pub from: usize,
    pub up_to: Option<usize>,
}

impl<C> Slice<C> {
    pub(crate) fn map<B, F>(&self, mut f: F) -> Slice<B>
    where
        F: FnMut(&C) -> B,
    {
        Slice {
            out: f(&self.out),
            base: f(&self.base),
            from: self.from,
            up_to: self.up_to,
        }
    }
}
#[derive(Debug, Copy, Clone, Hash)]
pub struct Minus<C> {
    pub out: C,
    pub left: C,
    pub right: C,
}

impl<C> Minus<C> {
    pub(crate) fn map<B, F>(&self, mut f: F) -> Minus<B>
    where
        F: FnMut(&C) -> B,
        B: Ord,
    {
        Minus {
            out: f(&self.out),
            left: f(&self.left),
            right: f(&self.right),
        }
    }
}

#[derive(Debug, Clone, From, Hash)]
pub enum Constraint<C> {
    Coincidence(Coincidence<C>),
    Causality(Causality<C>),
    Precedence(Precedence<C>),
    SubClock(Subclocking<C>),
    Exclusion(Exclusion<C>),
    Infimum(Infimum<C>),
    Supremum(Supremum<C>),
    Union(Union<C>),
    Intersection(Intersection<C>),
    Minus(Minus<C>),
    Repeat(Repeat<C>),
    Delay(Delay<C>),
    SampleOn(SampleOn<C>),
    Diff(Slice<C>),
}

impl<C> From<&'_ Constraint<C>> for STSBuilder<C>
where
    C: Hash + Clone + Ord + Display,
{
    fn from(c: &Constraint<C>) -> Self {
        c.map_ref_into()
    }
}

impl<C> From<&'_ Coincidence<C>> for STSBuilder<C>
where
    C: Clone + Ord + Hash + Display,
{
    fn from(c: &Coincidence<C>) -> Self {
        let var = IntegerExpression::var(Delta(c.left.clone(), c.right.clone()));
        let state = State::new(0).with_invariant(var.eq(0));
        let mut system = STSBuilder::new(c, state.clone());
        tr!(system, &state => &state, {c.left, c.right,});

        system
    }
}

impl<C> From<&'_ Alternates<C>> for STSBuilder<C>
where
    C: Clone + Ord + Hash + Display,
{
    fn from(c: &Alternates<C>) -> Self {
        let var = IntegerExpression::var(Delta(c.left.clone(), c.right.clone()));
        let start = State::new(0).with_invariant(var.eq(0));
        let alt = State::new(1).with_invariant(var.eq(1));
        let mut system = STSBuilder::new(c, start.clone());

        tr!(system, &start => &alt, {c.left, !c.right,});
        tr!(system, &alt => &start, {!c.left, c.right,});

        system
    }
}

impl<C> From<&'_ Causality<C>> for STSBuilder<C>
where
    C: Clone + Ord + Hash + Display,
{
    fn from(c: &Causality<C>) -> Self {
        let mut system: STSBuilder<C> = (&Precedence {
            cause: c.cause.clone(),
            effect: c.effect.clone(),
            init: c.init,
            max: c.max,
        })
            .into();
        let start = State::new(0);
        tr!(system, &start => &start, {c.cause, c.effect,});
        system.set_name(c);
        system
    }
}

impl<C> From<&'_ Precedence<C>> for STSBuilder<C>
where
    C: Clone + Ord + Hash + Display,
{
    fn from(c: &Precedence<C>) -> Self {
        let var = IntegerExpression::var(Delta(c.cause.clone(), c.effect.clone()));
        if c.init.is_some() && c.max.is_none() {
            panic!("init should be paired by max");
        }
        if let Some(max) = c.max {
            if max == 0 {
                panic!("max cannot be zero");
            }
            if c.init.unwrap_or(0) > max {
                panic!("init cannot be bigger than max");
            }
        }
        let init = State::new(c.init.unwrap_or(0));
        let mut system = STSBuilder::new(&c, init);
        let last = max(c.init, c.max).unwrap_or(1);
        let mut prev = State::new(0);
        for i in 1..=(last - 1) {
            let next = State::new(i);
            tr!(system, &prev => &next, {c.cause, !c.effect,});
            tr!(system, &next => &next, {c.cause, c.effect,});
            tr!(system, &next => &prev, {!c.cause, c.effect,});
            prev = next;
        }
        let last = State::new(last);
        if c.max.is_none() {
            tr!(system, &last => &last, {c.cause, !c.effect,});
            tr!(system, var.eq(1), &last => &prev, {!c.cause, c.effect,});
            tr!(system, var.more(1), &last => &last, {!c.cause, c.effect,});
        } else {
            tr!(system, &last => &prev, {!c.cause, c.effect,});
        }
        tr!(system, &prev => &last, {c.cause, !c.effect,});
        tr!(system, &last => &last, {c.cause, c.effect,});

        system
    }
}

impl<C> From<&'_ Exclusion<C>> for STSBuilder<C>
where
    C: Clone + Ord + Hash + Display,
{
    fn from(c: &Exclusion<C>) -> Self {
        let start = State::new(0);
        let mut system: STSBuilder<C> = STSBuilder::new(&c, start.clone());

        for clock in &c.clocks {
            tr!(system, &start => &start, {clock.clone(),});
        }
        system
    }
}

impl<C> From<&'_ Subclocking<C>> for STSBuilder<C>
where
    C: Clone + Ord + Hash + Display,
{
    fn from(c: &Subclocking<C>) -> Self {
        let var = IntegerExpression::var(Delta(c.sup.clone(), c.sub.clone()));
        let start = State::new(0).with_invariant(var.more_eq(0));
        let mut system = STSBuilder::new(&c, start.clone());

        tr!(system, &start => &start, {c.sup,});
        tr!(system, &start => &start, {c.sub, c.sup,});
        system
    }
}

impl<C> From<&'_ Union<C>> for STSBuilder<C>
where
    C: Clone + Ord + Hash + Display,
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
        system
    }
}

impl<C> From<&'_ Intersection<C>> for STSBuilder<C>
where
    C: Clone + Ord + Hash + Display,
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
        system
    }
}
impl<C> From<&'_ Delay<C>> for STSBuilder<C>
where
    C: Clone + Ord + Hash + Display,
{
    fn from(c: &Delay<C>) -> Self {
        let var = IntegerExpression::var(Delta(c.trigger.clone(), c.out.clone()));
        let start = State::new(0).with_invariant(var.eq(0));
        let mut system = STSBuilder::new(&c, start.clone());

        let mut last = start.clone();
        for i in 1..=c.delay {
            let state = State::new(i).with_invariant(var.eq(i64::try_from(i).unwrap()));
            tr!(system, &last => &state, {c.trigger,});
            last = state;
        }
        tr!(system, &last => &last, {c.out, c.trigger,});
        system
    }
}

impl<C> From<&'_ Infimum<C>> for STSBuilder<C>
where
    C: Clone + Ord + Hash + Display,
{
    fn from(c: &Infimum<C>) -> Self {
        let var = IntegerExpression::var(Delta(c.left.clone(), c.right.clone()));
        let minus = State::new(0);
        let zero = State::new(1);
        let plus = State::new(2);
        let mut system = STSBuilder::new(&c, zero.clone());
        tr!(system, &zero => &zero, {c.left, c.right, c.out,});
        tr!(system, &zero => &plus, {c.left, !c.right, c.out,});
        tr!(system, &zero => &minus, {!c.left, c.right, c.out,});

        tr!(system, &plus => &plus, {c.left, c.right, c.out,});
        tr!(system, &plus => &plus, {c.left, !c.right, c.out,});
        tr!(system, var.more(1), &plus => &plus, {!c.left, c.right, !c.out,});
        tr!(system, var.eq(1), &plus => &zero, {!c.left, c.right, !c.out,});

        tr!(system, &minus => &minus, {c.left, c.right, c.out,});
        tr!(system, &minus => &minus, {!c.left, c.right, c.out,});
        tr!(system, var.less(-1), &minus => &minus, {c.left, !c.right, !c.out,});
        tr!(system, var.eq(-1), &minus => &zero, {c.left, !c.right, !c.out,});
        system
    }
}

impl<C> From<&'_ Supremum<C>> for STSBuilder<C>
where
    C: Clone + Ord + Hash + Display,
{
    fn from(c: &Supremum<C>) -> Self {
        let var = IntegerExpression::var(Delta(c.left.clone(), c.right.clone()));
        let minus = State::new(0);
        let zero = State::new(1);
        let plus = State::new(2);
        let mut system = STSBuilder::new(&c, zero.clone());
        tr!(system, &zero => &zero, {c.left, c.right, c.out,});
        tr!(system, &zero => &plus, {c.left, !c.right, !c.out,});
        tr!(system, &zero => &minus, {!c.left, c.right, !c.out,});
        tr!(system, &plus => &plus, {c.left, c.right, !c.out,});
        tr!(system, &plus => &plus, {c.left, !c.right, !c.out,});
        tr!(system, var.more(1), &plus => &plus, {!c.left, c.right, c.out,});
        tr!(system, var.eq(1), &plus => &zero, {!c.left, c.right, c.out,});
        tr!(system, &minus => &minus, {c.left, c.right, !c.out,});
        tr!(system, &minus => &minus, {!c.left, c.right, !c.out,});
        tr!(system, var.less(-1), &minus => &minus, {c.left, !c.right, c.out,});
        tr!(system, var.eq(-1), &minus => &zero, {c.left, !c.right, c.out,});
        system
    }
}
impl<C> From<&'_ Minus<C>> for STSBuilder<C>
where
    C: Clone + Ord + Hash + Display,
{
    fn from(c: &Minus<C>) -> Self {
        let s = State::new(0);
        let mut system = STSBuilder::new(&c, s.clone());
        tr!(system, &s => &s, {c.out, c.left, !c.right,});
        tr!(system, &s => &s, {!c.out, c.left, c.right,});
        tr!(system, &s => &s, {!c.out, !c.left, c.right,});
        system
    }
}
impl<C> From<&'_ Slice<C>> for STSBuilder<C>
where
    C: Clone + Ord + Hash + Display,
{
    fn from(c: &Slice<C>) -> Self {
        let mut sts: STSBuilder<C> = (&Repeat {
            out: c.out.clone(),
            every: 1,
            base: c.base.clone(),
            from: Some(c.from),
            up_to: c.up_to,
        })
            .into();
        sts.set_name(c.to_string());
        sts
    }
}

impl<C> From<&'_ SampleOn<C>> for STSBuilder<C>
where
    C: Clone + Ord + Hash + Display,
{
    fn from(c: &SampleOn<C>) -> Self {
        let s1 = State::new(0);
        let s2 = State::new(1);
        let mut system = STSBuilder::new(&c, s1.clone());
        tr!(system, &s1 => &s1, {c.base, !c.trigger, !c.out,});
        tr!(system, &s1 => &s1, {c.base, c.trigger, c.out,});
        tr!(system, &s1 => &s2, {!c.base, c.trigger, !c.out,});
        tr!(system, &s2 => &s2, {!c.base, c.trigger, !c.out,});
        tr!(system, &s2 => &s2, {c.base, c.trigger, c.out,});
        tr!(system, &s2 => &s1, {c.base, !c.trigger, c.out,});
        system
    }
}

impl<C> From<&'_ Repeat<C>> for STSBuilder<C>
where
    C: Clone + Ord + Hash + Display,
{
    fn from(c: &Repeat<C>) -> Self {
        let from = c.from.unwrap_or(0);
        let up_to = c.up_to.unwrap_or(from);
        let delay = max(from, up_to);
        if delay == 0 && up_to > 0 {
            panic!("cannot represent as STS: repeat conflicts with up_to");
        }
        if c.up_to.is_some() {
            unimplemented!("difficult to express with automata");
        }
        let start = State::new(0);
        let mut system = STSBuilder::new(&c, start.clone());
        let mut repeat_state = start;
        for i in 1..=delay {
            let temp = State::new(i);
            tr!(system, &repeat_state => &temp, {c.base,  !c.out,});
            repeat_state = temp;
        }
        for i in from..delay {
            let temp = State::new(i);
            tr!(system, &temp => &repeat_state, {c.base,  !c.out,});
        }
        let mut prev = repeat_state.clone();
        for i in 1..=c.every - 1 {
            let temp = State::new(delay + i);
            tr!(system, &prev => &temp, {c.base, !c.out,});
            prev = temp;
        }
        tr!(system, &prev => &repeat_state, {c.base,  c.out,});
        system
    }
}

impl<C: Display> Display for Coincidence<C> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{} = {}", self.left, self.right)
    }
}

impl<C: Display> Display for Alternates<C> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{} ~ {}", self.left, self.right)
    }
}

impl<C: Display> Display for Causality<C> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match (self.init, self.max) {
            (Some(init), Some(max)) => write!(
                f,
                "{} <= (init: {}, max: {}) {}",
                self.cause, init, max, self.effect
            ),
            (Some(init), None) => write!(f, "{} <= (init: {}) {}", self.cause, init, self.effect),
            (None, Some(max)) => write!(f, "{} <= (max: {}) {}", self.cause, max, self.effect),
            (None, None) => write!(f, "{} <= {}", self.cause, self.effect),
        }
    }
}

impl<C: Display> Display for Precedence<C> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match (self.init, self.max) {
            (Some(init), Some(max)) => write!(
                f,
                "{} < (init: {}, max: {}) {}",
                self.cause, init, max, self.effect
            ),
            (Some(init), None) => write!(f, "{} < (init: {}) {}", self.cause, init, self.effect),
            (None, Some(max)) => write!(f, "{} < (max: {}) {}", self.cause, max, self.effect),
            (None, None) => write!(f, "{} < {}", self.cause, self.effect),
        }
    }
}

impl<C: Display> Display for Exclusion<C> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "#({})", self.clocks.iter().join(","))
    }
}

impl<C: Display> Display for Subclocking<C> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{} ⊂ {}", self.sub, self.sup)
    }
}

impl<C: Display> Display for Union<C> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{} = ∪({})", self.out, self.args.iter().join(","))
    }
}

impl<C: Display> Display for Minus<C> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{} = {} - {}", self.out, self.left, self.right)
    }
}

impl<C: Display> Display for Intersection<C> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{} = ∩({})", self.out, self.args.iter().join(","))
    }
}

impl<C: Display> Display for Delay<C> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{} = {}${}", self.out, self.trigger, self.delay)
    }
}

impl<C: Display> Display for Infimum<C> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{} = inf({},{})", self.out, self.left, self.right)
    }
}
impl<C: Display> Display for Supremum<C> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{} = sup({},{})", self.out, self.left, self.right)
    }
}
impl<C: Display> Display for SampleOn<C> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{} = {} sampledOn {}", self.out, self.trigger, self.base)
    }
}
impl<C: Display> Display for Slice<C> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{} = {} [{}, {}]",
            self.out,
            self.base,
            self.from,
            self.up_to
                .as_ref()
                .map_or("∞".to_owned(), ToString::to_string)
        )
    }
}
impl<C: Display> Display for Repeat<C> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "repeat {} every {} on {} from {:?} to {:?}",
            self.out, self.every, self.base, self.from, self.up_to
        )
    }
}

impl<C: Display> Display for Constraint<C> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Constraint::Coincidence(c) => write!(f, "{}", c),
            Constraint::Causality(c) => write!(f, "{}", c),
            Constraint::Precedence(c) => write!(f, "{}", c),
            Constraint::SubClock(c) => write!(f, "{}", c),
            Constraint::Exclusion(c) => write!(f, "{}", c),
            Constraint::Infimum(c) => write!(f, "{}", c),
            Constraint::Supremum(c) => write!(f, "{}", c),
            Constraint::Union(c) => write!(f, "{}", c),
            Constraint::Intersection(c) => write!(f, "{}", c),
            Constraint::Minus(c) => write!(f, "{}", c),
            Constraint::Repeat(c) => write!(f, "{}", c),
            Constraint::Delay(c) => write!(f, "{}", c),
            Constraint::SampleOn(c) => write!(f, "{}", c),
            Constraint::Diff(c) => write!(f, "{}", c),
        }
    }
}

impl<C, L> From<&'_ Constraint<C>> for STS<C, L>
where
    C: Clone + Ord + Hash + Display,
    L: Label<C>,
    for<'c, 'd> &'c L: BitOr<&'d L, Output = L>,
{
    fn from(v: &Constraint<C>) -> Self {
        let builder: STSBuilder<C> = v.into();
        builder.into()
    }
}

#[derive(Debug, From, Into, Clone)]
pub struct Specification<C>(pub Vec<Constraint<C>>);

impl<C> Constraint<C> {
    pub fn map_clocks<B, F>(&self, f: F) -> Constraint<B>
    where
        F: FnMut(&C) -> B,
        B: Ord,
    {
        match self {
            Constraint::Coincidence(c) => c.map(f).into(),
            Constraint::Causality(c) => c.map(f).into(),
            Constraint::Precedence(c) => c.map(f).into(),
            Constraint::SubClock(c) => c.map(f).into(),
            Constraint::Exclusion(c) => c.map(f).into(),
            Constraint::Infimum(c) => c.map(f).into(),
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

    pub fn clocks(&self) -> Vec<&C> {
        match self {
            Constraint::Coincidence(c) => vec![&c.left, &c.right],
            Constraint::Causality(c) => vec![&c.cause, &c.effect],
            Constraint::Precedence(c) => vec![&c.cause, &c.effect],
            Constraint::SubClock(c) => vec![&c.sub, &c.sup],
            Constraint::Exclusion(c) => c.clocks.iter().collect_vec(),
            Constraint::Infimum(c) => vec![&c.out, &c.left, &c.right],
            Constraint::Supremum(c) => vec![&c.out, &c.left, &c.right],
            Constraint::Union(c) => c.args.iter().chain(once(&c.out)).collect_vec(),
            Constraint::Intersection(c) => c.args.iter().chain(once(&c.out)).collect_vec(),
            Constraint::Minus(c) => vec![&c.out, &c.left, &c.right],
            Constraint::Repeat(c) => vec![&c.out, &c.base],
            Constraint::Delay(c) => {
                c.on.iter()
                    .chain(once(&c.out))
                    .chain(once(&c.trigger))
                    .collect_vec()
            }
            Constraint::SampleOn(c) => vec![&c.out, &c.trigger, &c.base],
            Constraint::Diff(c) => vec![&c.out, &c.base],
        }
    }

    pub fn map_into<T>(self) -> T
    where
        T: From<Coincidence<C>>
            + From<Causality<C>>
            + From<Precedence<C>>
            + From<Subclocking<C>>
            + From<Exclusion<C>>
            + From<Infimum<C>>
            + From<Supremum<C>>
            + From<Union<C>>
            + From<Intersection<C>>
            + From<Minus<C>>
            + From<Repeat<C>>
            + From<Delay<C>>
            + From<SampleOn<C>>
            + From<Slice<C>>,
    {
        match self {
            Constraint::Coincidence(c) => c.into(),
            Constraint::Causality(c) => c.into(),
            Constraint::Precedence(c) => c.into(),
            Constraint::SubClock(c) => c.into(),
            Constraint::Exclusion(c) => c.into(),
            Constraint::Infimum(c) => c.into(),
            Constraint::Supremum(c) => c.into(),
            Constraint::Union(c) => c.into(),
            Constraint::Intersection(c) => c.into(),
            Constraint::Minus(c) => c.into(),
            Constraint::Repeat(c) => c.into(),
            Constraint::Delay(c) => c.into(),
            Constraint::SampleOn(c) => c.into(),
            Constraint::Diff(c) => c.into(),
        }
    }
    pub fn map_ref_into<T>(&self) -> T
    where
        for<'a> T: From<&'a Coincidence<C>>
            + From<&'a Causality<C>>
            + From<&'a Precedence<C>>
            + From<&'a Subclocking<C>>
            + From<&'a Exclusion<C>>
            + From<&'a Infimum<C>>
            + From<&'a Supremum<C>>
            + From<&'a Union<C>>
            + From<&'a Intersection<C>>
            + From<&'a Minus<C>>
            + From<&'a Repeat<C>>
            + From<&'a Delay<C>>
            + From<&'a SampleOn<C>>
            + From<&'a Slice<C>>,
    {
        match self {
            Constraint::Coincidence(c) => c.into(),
            Constraint::Causality(c) => c.into(),
            Constraint::Precedence(c) => c.into(),
            Constraint::SubClock(c) => c.into(),
            Constraint::Exclusion(c) => c.into(),
            Constraint::Infimum(c) => c.into(),
            Constraint::Supremum(c) => c.into(),
            Constraint::Union(c) => c.into(),
            Constraint::Intersection(c) => c.into(),
            Constraint::Minus(c) => c.into(),
            Constraint::Repeat(c) => c.into(),
            Constraint::Delay(c) => c.into(),
            Constraint::SampleOn(c) => c.into(),
            Constraint::Diff(c) => c.into(),
        }
    }
}

impl<C> Specification<C> {
    pub fn map_clocks<B, F>(&self, mut f: F) -> Specification<B>
    where
        F: FnMut(&C) -> B,
        B: Ord,
    {
        Specification(self.0.iter().map(move |c| c.map_clocks(|c| f(c))).collect())
    }

    pub fn iter(&self) -> Iter<'_, Constraint<C>> {
        self.into_iter()
    }
}

impl<C> IntoIterator for Specification<C> {
    type Item = Constraint<C>;
    type IntoIter = IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl<'a, C> IntoIterator for &'a Specification<C> {
    type Item = &'a Constraint<C>;
    type IntoIter = Iter<'a, Constraint<C>>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.iter()
    }
}
