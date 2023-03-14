use crate::interpretation::boolean::Bool;
use crate::interpretation::interval::Interval;
use crate::interpretation::{Lattice, Prec, SequenceLimiter, Succ};
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
use std::cmp::Ordering;
use std::collections::{BTreeMap, HashMap};
use std::fmt::{Debug, Display, Formatter};
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
            let invariant: Invariant<C> = c.into();
            invariant.0.leaves(&mut counters, &mut clocks);
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
pub struct Invariant<C>(pub BooleanExpression<Delta<C>, C>);

impl<C: Clone> From<&'_ Constraint<C>> for Invariant<C> {
    fn from(value: &'_ Constraint<C>) -> Self {
        match value {
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
        }
    }
}
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
        Invariant(ab.more_eq(0) & ((ab.eq(d) & a.eq(b.clone())) | (ab.less(d) & !b)))
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
        self.0.leaves(&mut counters, &mut clocks);
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
pub enum Step {
    #[default]
    Init,
    LoopHead,
    Update,
    If,
    Exit,
}

impl Display for Step {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Step::Init => "Init",
            Step::LoopHead => "LoopHead",
            Step::Update => "CounterUpdate",
            Step::If => "If",
            Step::Exit => "Exit",
        };
        f.pad(s)
    }
}

// TODO: any better memory layout for the task?
#[derive(Debug, PartialEq, Clone, Default)]
pub struct ExecutionState<C> {
    integers: BTreeMap<Delta<C>, Interval<i64>>,
    booleans: BTreeMap<C, Bool>,
}

impl<C: Ord> From<(HashMap<Delta<C>, Interval<i64>>, HashMap<C, Bool>)> for ExecutionState<C> {
    fn from((i, b): (HashMap<Delta<C>, Interval<i64>>, HashMap<C, Bool>)) -> Self {
        Self {
            integers: i.into_iter().collect(),
            booleans: b.into_iter().collect(),
        }
    }
}

impl<C: Display> Display for ExecutionState<C> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for (k, v) in &self.integers {
            write!(f, "{}: {}, ", k, v)?;
        }
        for (k, v) in &self.booleans {
            write!(f, "{}: {}, ", k, v)?;
        }

        Ok(())
    }
}

impl<C> ExecutionState<C> {
    fn new() -> Self {
        Self {
            integers: BTreeMap::new(),
            booleans: BTreeMap::new(),
        }
    }
}

pub fn interpret<C, W, N>(spec: &Specification<C>) -> HashMap<Step, ExecutionState<C>>
where
    C: Ord + Hash + Clone + Display + Debug,
    W: SequenceLimiter<Domain = ExecutionState<C>> + Default,
    N: SequenceLimiter<Domain = ExecutionState<C>> + Default,
{
    let program: ProgramEffects<C> = spec.into();
    let (pos, neg) = program.invariant.abstractify();
    let curr = init_state();
    let prev = HashMap::new();
    let curr = loop_state::<C, W>(&program, &pos, &neg, prev, curr);
    loop_state::<C, N>(&program, &pos, &neg, curr.clone(), curr)
}

fn loop_state<C, L>(
    program: &ProgramEffects<C>,
    pos: &ExecutionState<C>,
    neg: &ExecutionState<C>,
    mut prev: HashMap<Step, ExecutionState<C>>,
    mut curr: HashMap<Step, ExecutionState<C>>,
) -> HashMap<Step, ExecutionState<C>>
where
    C: Ord + Hash + Clone + Display + Debug,
    L: SequenceLimiter<Domain = ExecutionState<C>> + Default,
{
    let mut limiter = L::default();
    loop {
        for step in &[
            Step::Init,
            Step::LoopHead,
            Step::Update,
            Step::If,
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
                    let body = curr.get(&Step::Update).unwrap();
                    let init = curr.get(&Step::Init).unwrap();
                    let mut update = ExecutionState::new();
                    for clock in &program.clocks {
                        update.booleans.insert(clock.clone(), Bool::Both);
                    }
                    let head = body.clone() | init.clone() | update;
                    curr.insert(
                        *step,
                        prev.get(step)
                            .map(|prev| limiter.deduct(prev, &head))
                            .unwrap_or(head),
                    );
                }
                Step::Update => {
                    let mut state = curr.get(&Step::LoopHead).unwrap().clone();
                    for counter in &program.counters {
                        let x = state.booleans[&counter.0].into();
                        let y = state.booleans[&counter.0].into();
                        let mut x_y = state.integers[counter];
                        x_y = x_y + x;
                        x_y = x_y - y;
                        state.integers.insert(counter.clone(), x_y);
                    }
                    curr.insert(*step, state);
                }
                Step::If => {
                    let input = curr.get(&Step::Update).unwrap();
                    let filtered_input = input.clone() & pos.clone();
                    curr.insert(*step, filtered_input);
                }
                Step::Exit => {
                    let input = curr.get(&Step::Update).unwrap();
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
            Step::Update,
            Step::If,
            Step::Exit,
        ]
        .into_iter()
        .map(|s| (s, ExecutionState::new())),
    )
}

pub fn stop_condition<C, W, N>(spec: &Specification<C>) -> ExecutionState<C>
where
    C: Clone + Ord + Hash + Display + Debug,
    W: SequenceLimiter<Domain = ExecutionState<C>> + Default,
    N: SequenceLimiter<Domain = ExecutionState<C>> + Default,
{
    interpret::<C, W, N>(spec).get(&Step::Exit).unwrap().clone()
}

impl<C: Ord + Clone + Display + Debug> Invariant<C> {
    fn abstractify(&self) -> (ExecutionState<C>, ExecutionState<C>) {
        (assume(&self.0, true), assume(&self.0, false))
    }
}

pub fn assume<C: Ord + Clone + Display + Debug>(
    e: &BooleanExpression<Delta<C>, C>,
    expected: bool,
) -> ExecutionState<C> {
    match e {
        BooleanExpression::IntegerBinary { kind, left, right } => {
            let (kind, v, c) = match (&left.deref(), &right.deref()) {
                (IntegerExpression::Variable(v), IntegerExpression::Constant(c)) => (*kind, v, *c),
                (IntegerExpression::Constant(c), IntegerExpression::Variable(v)) => {
                    (kind.inverse(), v, *c)
                }
                _ => unimplemented!(),
            };
            let mut interval: Interval<i64> = match kind {
                IntegerComparisonKind::Equal => c.into(),
                IntegerComparisonKind::Less => (..=c.prec()).into(),
                IntegerComparisonKind::LessEq => (..=c).into(),
                IntegerComparisonKind::More => (c.succ()..).into(),
                IntegerComparisonKind::MoreEq => (c..).into(),
            };
            if !expected {
                interval = !interval;
            }
            let mut state = ExecutionState::new();
            state.integers.insert(v.clone(), interval);
            state
        }
        BooleanExpression::BooleanBinary { kind, left, right } => match kind {
            BooleanComparisonKind::And => {
                if expected {
                    assume(left, true).intersection_complemented(assume(right, true))
                } else {
                    assume(left, false).union_complemented(assume(right, false))
                }
            }
            BooleanComparisonKind::Or => {
                if expected {
                    assume(left, true).union_complemented(assume(right, true))
                } else {
                    assume(left, false).intersection_complemented(assume(right, false))
                }
            }
            BooleanComparisonKind::Xor => assume(left, true)
                .intersection_complemented(assume(right, false))
                .union_complemented(
                    assume(left, false).intersection_complemented(assume(right, true)),
                ),
            BooleanComparisonKind::Eq => assume(left, true)
                .intersection_complemented(assume(right, true))
                .union_complemented(
                    assume(left, false).intersection_complemented(assume(right, false)),
                ),
        },
        BooleanExpression::Not(e) => assume(e, !expected),
        BooleanExpression::Constant(_) => panic!("should not be present"),
        BooleanExpression::Variable(v) => {
            let mut s = ExecutionState::new();
            s.booleans.insert(v.clone(), expected.into());
            s
        }
    }
}

impl<C: Ord + Clone> ExecutionState<C> {
    fn combine(
        &self,
        rhs: &Self,
        bool_default: Bool,
        bool_op: impl Fn(Bool, Bool) -> Bool,
        int_default: Interval<i64>,
        int_op: impl Fn(Interval<i64>, Interval<i64>) -> Interval<i64>,
    ) -> ExecutionState<C> {
        let booleans = self
            .booleans
            .keys()
            .chain(rhs.booleans.keys())
            .map(|k| {
                let x = self.booleans.get(k).copied().unwrap_or(bool_default);
                let y = rhs.booleans.get(k).copied().unwrap_or(bool_default);
                (k.clone(), bool_op(x, y))
            })
            .collect();
        let integers = self
            .integers
            .keys()
            .chain(rhs.integers.keys())
            .map(|k| {
                let x = self.integers.get(k).copied().unwrap_or(int_default);
                let y = rhs.integers.get(k).copied().unwrap_or(int_default);
                (k.clone(), int_op(x, y))
            })
            .collect();

        Self { booleans, integers }
    }
}

impl<C: Ord + Clone> BitOr for ExecutionState<C> {
    type Output = Self;

    fn bitor(mut self, rhs: Self) -> Self::Output {
        self.union_inplace(&rhs);
        self
    }
}

impl<C: Ord + Clone> BitAnd for ExecutionState<C> {
    type Output = Self;

    fn bitand(mut self, rhs: Self) -> Self::Output {
        self.intersection_inplace(&rhs);
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

#[derive(Debug, Clone)]
pub struct StateWidening<C, W> {
    integers: BTreeMap<Delta<C>, W>,
}

impl<S, W> Default for StateWidening<S, W> {
    fn default() -> Self {
        Self {
            integers: BTreeMap::new(),
        }
    }
}

impl<C: Ord + Clone + Hash + Display, W> SequenceLimiter for StateWidening<C, W>
where
    W: SequenceLimiter<Domain = Interval<i64>> + Default,
{
    type Domain = ExecutionState<C>;

    fn deduct(&mut self, prev: &Self::Domain, next: &Self::Domain) -> Self::Domain {
        //println!("{}, {}", prev, next);
        let mut result = ExecutionState::new();
        result.booleans = next.booleans.clone();
        let keys = prev
            .integers
            .keys()
            .chain(next.integers.keys())
            .cloned()
            .unique()
            .collect_vec();
        for k in keys {
            let prev = prev.integers.get(&k).copied().unwrap_or(Interval::Bottom);
            let next = next.integers.get(&k).copied().unwrap_or(Interval::Bottom);
            let widening = self
                .integers
                .entry(k.clone())
                .or_insert_with(Default::default);
            result.integers.insert(k, widening.deduct(&prev, &next));
        }

        result
    }
}

impl<C: Ord + Clone> ExecutionState<C> {
    fn union_complemented(self, rhs: Self) -> Self {
        self.combine(
            &rhs,
            Bool::Both,
            BitOr::bitor,
            Interval::top(),
            BitOr::bitor,
        )
    }
    fn intersection_complemented(self, rhs: Self) -> Self {
        self.combine(
            &rhs,
            Bool::Both,
            BitAnd::bitand,
            Interval::top(),
            BitAnd::bitand,
        )
    }
}

impl<C: PartialEq> PartialOrd for ExecutionState<C> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        todo!()
    }
}

impl<C: Clone + Ord> Lattice for ExecutionState<C> {
    fn subset(&self, rhs: &Self) -> bool {
        self.integers
            .iter()
            .all(|(k, x)| rhs.integers.get(k).map(|y| x.subset(y)).unwrap_or(true))
            & self
                .booleans
                .iter()
                .all(|(k, x)| rhs.booleans.get(k).map(|y| x.subset(y)).unwrap_or(true))
    }

    fn union_inplace(&mut self, rhs: &Self) {
        *self = self.combine(
            rhs,
            Bool::Neither,
            BitOr::bitor,
            Interval::bottom(),
            BitOr::bitor,
        )
    }

    fn intersection_inplace(&mut self, rhs: &Self) {
        *self = self.combine(
            rhs,
            Bool::Neither,
            BitAnd::bitand,
            Interval::bottom(),
            BitAnd::bitand,
        )
    }
}
