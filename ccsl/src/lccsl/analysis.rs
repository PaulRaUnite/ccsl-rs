use crate::interpretation::boolean::Bool;
use crate::interpretation::interval::Interval;
use crate::interpretation::{Lattice, Prec, Succ};
use crate::lccsl::automata::Delta;
use crate::lccsl::constraints::{
    Causality, Constraint, Delay, Diff, Exclusion, Infinity, Intersection, Minus, Precedence,
    Repeat, SampleOn, Specification, Subclocking, Supremum, Union,
};
use crate::lccsl::expressions::{
    BooleanComparisonKind, BooleanExpression, IntegerComparisonKind, IntegerExpression,
};
use derive_more::From;
use itertools::Itertools;
use std::collections::btree_map::Entry;
use std::collections::{BTreeMap, HashMap};
use std::fmt::Debug;
use std::hash::Hash;
use std::ops::{BitAnd, BitOr, Deref, Not};

#[derive(Debug, Clone)]
struct ProgramEffects<C> {
    clocks: Vec<C>,
    counters: Vec<Delta<C>>,
    invariant: Invariant<C>,
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
            clocks: clocks
                .into_iter()
                .chain(
                    counters
                        .iter()
                        .flat_map(|c| [c.0.clone(), c.1.clone()].into_iter()),
                )
                .unique()
                .collect(),
            counters: counters.into_iter().unique().collect(),
            invariant: Invariant(
                invariants
                    .into_iter()
                    .map(|i| i.0)
                    .reduce(|x, y| x & y)
                    .unwrap(),
            ),
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
        (ab.more_eq(0) & ab.eq(0).implies(!b)).into()
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
    fn from(_: &'_ Infinity<C>) -> Self {
        unimplemented!()
    }
}
impl<C: Clone> From<&'_ Supremum<C>> for Invariant<C> {
    fn from(_: &'_ Supremum<C>) -> Self {
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
            .map(|e| e.eq(BooleanExpression::var(c.out.clone())))
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
            .map(|e| e.eq(BooleanExpression::var(c.out.clone())))
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
    fn from(_: &'_ Repeat<C>) -> Self {
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
    fn from(_: &'_ SampleOn<C>) -> Self {
        unimplemented!()
    }
}
impl<C: Clone> From<&'_ Diff<C>> for Invariant<C> {
    fn from(_: &'_ Diff<C>) -> Self {
        unimplemented!()
    }
}

impl<C: Ord + Clone + Debug> Invariant<C> {
    pub(crate) fn check<const N: usize>(&self, traces: BTreeMap<C, [u8; N]>) -> bool {
        let mut counters = vec![];
        let mut clocks = vec![];
        self.0
            .leaves(&mut counters, &mut clocks, &mut vec![], &mut vec![]);
        for c in clocks.iter() {
            if !traces.contains_key(c) {
                panic!("trace for clock {:?} is not supplied", c)
            }
        }
        let mut counters: BTreeMap<Delta<C>, i64> = counters.into_iter().map(|c| (c, 0)).collect();
        for i in 0..N {
            for j in 0..8 {
                if !self
                    .0
                    .eval(&|c| counters[c], &|c| (traces[c][i] & 1 << j) != 0)
                {
                    return false;
                }
                for (k, v) in &mut counters {
                    let a = ((traces[&k.0][i] & 1 << j) != 0) as i64;
                    let b = ((traces[&k.1][i] & 1 << j) != 0) as i64;
                    *v += a;
                    *v -= b;
                }
            }
        }

        true
    }
}

#[cfg(test)]
mod test;

#[derive(Debug, Copy, Clone, Default, Eq, PartialEq, Hash)]
enum Step {
    #[default]
    Init,
    LoopHead,
    ClockInput,
    CounterUpdate,
    Exit,
}

// TODO: any better memory layout for the task?
#[derive(Debug, PartialEq, Clone, Default)]
pub struct ExecutionState<C> {
    integers: BTreeMap<Delta<C>, Interval<i64>>,
    booleans: BTreeMap<C, Bool>,
}

impl<C> ExecutionState<C> {
    fn new() -> Self {
        Self {
            integers: BTreeMap::new(),
            booleans: BTreeMap::new(),
        }
    }
}

fn execute<C>(spec: &Specification<C>) -> HashMap<Step, ExecutionState<C>>
where
    C: Ord + Hash + Clone,
{
    let program: ProgramEffects<C> = spec.into();
    let (pos, neg) = program.invariant.abstractify();
    let mut prev = init_state();
    let mut curr = init_state();
    for _ in 0..100 {
        for step in &[
            Step::Init,
            Step::LoopHead,
            Step::ClockInput,
            Step::CounterUpdate,
            Step::Exit,
        ] {
            match step {
                Step::Init => {
                    let state = curr.get_mut(step).unwrap();
                    if state.integers.is_empty() {
                        for counter in &program.counters {
                            state.integers.insert(counter.clone(), 0.into());
                        }
                    }
                }
                Step::LoopHead => {
                    let body = curr.get(&Step::CounterUpdate).unwrap();
                    let init = curr.get(&Step::Init).unwrap();
                    curr.insert(*step, body.clone() | init.clone());
                }
                Step::ClockInput => {
                    let mut input = ExecutionState::new();
                    for clock in &program.clocks {
                        input.booleans.insert(clock.clone(), Bool::Both);
                    }
                    curr.insert(*step, curr[&Step::LoopHead].clone() | input);
                }
                Step::CounterUpdate => {
                    let input = curr.get(&Step::ClockInput).unwrap();
                    let mut filtered_input = input.clone() & pos.clone();
                    for counter in &program.counters {
                        let x = filtered_input.booleans[&counter.0];
                        let y = filtered_input.booleans[&counter.0];
                        let mut x_y = filtered_input.integers[counter];
                        x_y = x_y + x.into();
                        x_y = x_y - y.into();
                        filtered_input.integers.insert(counter.clone(), x_y);
                    }
                    curr.insert(*step, filtered_input);
                }
                Step::Exit => {
                    let input = curr.get(&Step::ClockInput).unwrap();
                    let exit = curr.get(&Step::Exit).unwrap();
                    let filtered_input = input.clone() & neg.clone();
                    curr.insert(*step, exit.clone() | filtered_input);
                }
            }
        }
        if prev == curr {
            break;
        }
        prev = curr.clone();
    }
    curr
}

fn init_state<C>() -> HashMap<Step, ExecutionState<C>> {
    HashMap::from_iter(
        [
            Step::Init,
            Step::LoopHead,
            Step::ClockInput,
            Step::CounterUpdate,
            Step::Exit,
        ]
        .into_iter()
        .map(|s| (s, ExecutionState::new())),
    )
}

pub fn stop_condition<C>(spec: &Specification<C>) -> ExecutionState<C>
where
    C: Clone + Ord + Hash,
{
    execute(spec).get(&Step::Exit).unwrap().clone()
}

impl<C: Ord + Clone> Invariant<C> {
    fn abstractify(&self) -> (ExecutionState<C>, ExecutionState<C>) {
        (assume(&self.0, true), assume(&self.0, false))
    }
}

fn assume<C: Ord + Clone>(e: &BooleanExpression<Delta<C>, C>, value: bool) -> ExecutionState<C> {
    match e {
        BooleanExpression::IntegerBinary { kind, left, right } => {
            let (kind, v, c) = match (&left.deref(), &right.deref()) {
                (IntegerExpression::Variable(v), IntegerExpression::Constant(c)) => (*kind, v, *c),
                (IntegerExpression::Constant(c), IntegerExpression::Variable(v)) => {
                    (kind.inverse(), v, *c)
                }
                _ => unimplemented!(),
            };
            let interval: Interval<i64> = match kind {
                IntegerComparisonKind::Equal => c.into(),
                IntegerComparisonKind::Less => (..=c.prec()).into(),
                IntegerComparisonKind::LessEq => (..=c).into(),
                IntegerComparisonKind::More => (c.succ()..).into(),
                IntegerComparisonKind::MoreEq => (c..).into(),
            };
            let mut state = ExecutionState::new();
            state.integers.insert(v.clone(), interval);
            state
        }
        BooleanExpression::BooleanBinary { kind, left, right } => match kind {
            BooleanComparisonKind::And => {
                let res = assume(&left, true) & assume(&right, true);
                if !value {
                    !res
                } else {
                    res
                }
            }
            BooleanComparisonKind::Or => {
                let res = assume(&left, false) | assume(&right, false);
                if value {
                    !res
                } else {
                    res
                }
            }
            BooleanComparisonKind::Xor => {
                (assume(&left, true) & assume(&right, false))
                    | (assume(&left, false) & assume(&right, true))
            }
            BooleanComparisonKind::Eq => {
                (assume(&left, true) & assume(&right, true))
                    | (assume(&left, false) & assume(&right, false))
            }
        },
        BooleanExpression::Not(e) => assume(&e, !value),
        BooleanExpression::Constant(_) => panic!("should not be present"),
        BooleanExpression::Variable(v) => {
            let mut s = ExecutionState::new();
            s.booleans.insert(v.clone(), value.into());
            s
        }
    }
}

impl<C: Ord> BitOr for ExecutionState<C> {
    type Output = Self;

    fn bitor(mut self, rhs: Self) -> Self::Output {
        for (k, v) in rhs.booleans {
            match self.booleans.entry(k) {
                Entry::Vacant(vac) => {
                    vac.insert(v);
                }
                Entry::Occupied(mut occ) => occ.get_mut().union_inplace(&v),
            }
        }
        for (k, v) in rhs.integers {
            match self.integers.entry(k) {
                Entry::Vacant(vac) => {
                    vac.insert(v);
                }
                Entry::Occupied(mut occ) => occ.get_mut().union_inplace(&v),
            }
        }
        self
    }
}

impl<C: Ord> BitAnd for ExecutionState<C> {
    type Output = Self;

    fn bitand(mut self, rhs: Self) -> Self::Output {
        for (k, v) in rhs.booleans {
            match self.booleans.entry(k) {
                Entry::Vacant(vac) => {
                    vac.insert(v);
                }
                Entry::Occupied(mut occ) => occ.get_mut().intersection_inplace(&v),
            }
        }
        for (k, v) in rhs.integers {
            match self.integers.entry(k) {
                Entry::Vacant(vac) => {
                    vac.insert(v);
                }
                Entry::Occupied(mut occ) => occ.get_mut().intersection_inplace(&v),
            }
        }
        self
    }
}

impl<C: Ord> Not for ExecutionState<C> {
    type Output = Self;

    fn not(mut self) -> Self::Output {
        self.booleans.values_mut().for_each(|v| *v = !*v);
        self.integers.values_mut().for_each(|v| *v = !*v);
        self
    }
}
