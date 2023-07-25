use crate::kernel::automata::Delta;
use crate::kernel::constraints::{
    Causality, Coincidence, Delay, Exclusion, Infimum, Intersection, Minus, Precedence, Repeat,
    SampleOn, Slice, Subclocking, Supremum, Union,
};
use crate::kernel::expressions::{BooleanExpression, IntegerExpression};
use crate::lccsl::parser::Specification;
use derive_more::From;
use itertools::Itertools;
use map_macro::{map, set};
use std::collections::{HashMap, HashSet};
use std::fmt::{Display, Formatter};
use std::hash::Hash;
use std::ops::{BitAnd, BitOr};

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum Variable<C> {
    Clock(C),
    Delta(Delta<C>),
    Generic(String),
}

impl<C: Display> Display for Variable<C> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Variable::Clock(c) => write!(f, "{}", c),
            Variable::Delta(d) => write!(f, "d_{}_{}", d.0, d.1),
            Variable::Generic(g) => write!(f, "{}", g),
        }
    }
}

#[derive(Debug, Copy, Clone, From, Eq, PartialEq)]
pub enum Constant {
    Bool(bool),
    Int(i64),
}

#[derive(Debug, Clone, From, Eq, PartialEq)]
pub enum Expression<C> {
    Bool(BooleanExpression<Variable<C>, Variable<C>>),
    Int(IntegerExpression<Variable<C>, Variable<C>>),
}

impl<C> Expression<C> {
    pub fn map<NC>(
        &self,
        mut f: impl FnMut(&Variable<C>) -> Variable<NC> + Clone,
    ) -> Expression<NC> {
        match self {
            Expression::Bool(e) => {
                Expression::Bool(e.map(&mut f.clone(), &mut f, &mut |c| *c, &mut |c| *c))
            }
            Expression::Int(e) => {
                Expression::Int(e.map(&mut f.clone(), &mut f, &mut |c| *c, &mut |c| *c))
            }
        }
    }
}
#[derive(Debug, Clone)]
pub struct TransitionSystem<C> {
    pub states: HashMap<Variable<C>, (Constant, Expression<C>)>,
    pub inputs: HashSet<Variable<C>>,
    pub restriction: BooleanExpression<Variable<C>, Variable<C>>,
}

impl<C> Specification<C> {
    pub fn delta_counter_ts(&self) -> TransitionSystem<C> {
        todo!()
    }
    pub fn index_counter_ts(&self) -> TransitionSystem<C> {
        todo!()
    }
}

/// a = b
impl<C: Clone + Eq + Hash> From<&'_ Coincidence<C>> for TransitionSystem<C> {
    fn from(c: &'_ Coincidence<C>) -> Self {
        let a = BooleanExpression::var(c.left.clone());
        let b = BooleanExpression::var(c.right.clone());
        a.eq(b).into()
    }
}

/// ((d(a,b) = init and b) => a) and (d(a,b) = max => (a=>b))
impl<C: Clone + Eq + Hash> From<&'_ Causality<C>> for TransitionSystem<C> {
    fn from(c: &'_ Causality<C>) -> Self {
        let a_v = Variable::Clock(c.cause.clone());
        let a_e = BooleanExpression::var(a_v.clone());
        let b_v = Variable::Clock(c.effect.clone());
        let b_e = BooleanExpression::var(b_v.clone());
        let ab = Variable::Delta(Delta(c.cause.clone(), c.effect.clone()));

        let ab_e = IntegerExpression::var(ab.clone());
        TransitionSystem {
            states: map! {ab.clone() => (Constant::Int(c.init.unwrap_or(0) as i64), default_counter_transition(&ab).1)},
            inputs: set! {a_v, b_v},
            restriction: (ab_e.eq(0) & b_e.clone()).implies(a_e.clone())
                & c.max.map_or(BooleanExpression::Constant(true), |max| {
                    ab_e.eq(max as i64).implies(a_e.implies(b_e))
                }),
        }
    }
}

/// (d(a,b) = 0 => !b) and (d(a,b) = max => (a=>b))
impl<C: Clone + Eq + Hash> From<&'_ Precedence<C>> for TransitionSystem<C> {
    fn from(c: &'_ Precedence<C>) -> Self {
        let a_v = Variable::Clock(c.cause.clone());
        let a_e = BooleanExpression::var(a_v.clone());
        let b_v = Variable::Clock(c.effect.clone());
        let b_e = BooleanExpression::var(b_v.clone());
        let ab = Variable::Delta(Delta(c.cause.clone(), c.effect.clone()));

        let ab_e = IntegerExpression::var(ab.clone());
        TransitionSystem {
            states: map! {ab.clone() => (Constant::Int(c.init.unwrap_or(0) as i64), default_counter_transition(&ab).1)},
            inputs: set! {a_v, b_v},
            restriction: (ab_e.eq(0).implies(!b_e.clone()))
                & c.max.map_or(BooleanExpression::Constant(true), |max| {
                    ab_e.eq(max as i64).implies(a_e.implies(b_e))
                }),
        }
    }
}

/// a => b
impl<C: Clone + Eq + Hash> From<&'_ Subclocking<C>> for TransitionSystem<C> {
    fn from(c: &'_ Subclocking<C>) -> Self {
        let a = BooleanExpression::var(c.sub.clone());
        let b = BooleanExpression::var(c.sup.clone());
        a.implies(b).into()
    }
}

/// join and (forall a,b: a nand b)
impl<C: Clone + Eq + Hash> From<&'_ Exclusion<C>> for TransitionSystem<C> {
    fn from(c: &'_ Exclusion<C>) -> Self {
        c.clocks
            .iter()
            .tuple_combinations::<(_, _)>()
            .map(|(x, y)| {
                let x = BooleanExpression::var(x.clone());
                let y = BooleanExpression::var(y.clone());
                !(x & y)
            })
            .reduce(BitAnd::bitand)
            .unwrap()
            .into()
    }
}

/// (d(a,b) >= 0 and a) or (d(a,b) <= 0 and b) = i
impl<C: Clone + Eq + Hash> From<&'_ Infimum<C>> for TransitionSystem<C> {
    fn from(c: &'_ Infimum<C>) -> Self {
        let a = BooleanExpression::var(c.left.clone());
        let b = BooleanExpression::var(c.right.clone());
        let i = BooleanExpression::var(c.out.clone());
        let ab = Delta(c.left.clone(), c.right.clone());
        let counter = IntegerExpression::var(ab);
        ((counter.more_eq(0) & a) | (counter.less_eq(0) & b))
            .eq(i)
            .into()
    }
}

/// (d(a,b) >= 0 and b) or (d(a,b) <= 0 and a) = s
impl<C: Clone + Eq + Hash> From<&'_ Supremum<C>> for TransitionSystem<C> {
    fn from(c: &'_ Supremum<C>) -> Self {
        let a = BooleanExpression::var(c.left.clone());
        let b = BooleanExpression::var(c.right.clone());
        let s = BooleanExpression::var(c.out.clone());
        let ab = Delta(c.left.clone(), c.right.clone());
        let counter = IntegerExpression::var(ab);

        ((counter.more_eq(0) & b) | (counter.less_eq(0) & a))
            .eq(s)
            .into()
    }
}

/// out = (c0 or ... or cn)
impl<C: Clone + Eq + Hash> From<&'_ Union<C>> for TransitionSystem<C> {
    fn from(c: &'_ Union<C>) -> Self {
        let out = BooleanExpression::var(c.out.clone());
        c.args
            .iter()
            .cloned()
            .map(BooleanExpression::var)
            .reduce(BitOr::bitor)
            .map(|e| e.eq(out))
            .unwrap()
            .into()
    }
}
/// out = (c0 and ... and cn)
impl<C: Clone + Eq + Hash> From<&'_ Intersection<C>> for TransitionSystem<C> {
    fn from(c: &'_ Intersection<C>) -> Self {
        let out = BooleanExpression::var(c.out.clone());
        c.args
            .iter()
            .cloned()
            .map(BooleanExpression::var)
            .reduce(BitAnd::bitand)
            .map(|e| e.eq(out))
            .unwrap()
            .into()
    }
}

/// (a and not b) = c
impl<C: Clone + Eq + Hash> From<&'_ Minus<C>> for TransitionSystem<C> {
    fn from(c: &'_ Minus<C>) -> Self {
        let a = BooleanExpression::var(c.left.clone());
        let b = BooleanExpression::var(c.right.clone());
        let c = BooleanExpression::var(c.out.clone());
        (a & !b).eq(c).into()
    }
}

/// ((counter = period) => (base = out)) and ((counter < period) => not out)
/// counter = if counter = period then 0 else if base then counter + 1 else counter
impl<C: Clone + Eq + Hash> From<&'_ Repeat<C>> for TransitionSystem<C> {
    fn from(c: &'_ Repeat<C>) -> Self {
        if c.from.is_some() || c.up_to.is_some() {
            unimplemented!("slicing in repeat");
        }
        let base_v = Variable::Clock(c.base.clone());
        let base_e = BooleanExpression::var(base_v.clone());
        let out_v = Variable::Clock(c.out.clone());
        let out_e = BooleanExpression::var(out_v.clone());
        let counter_v = Variable::Generic("r".to_owned());
        let counter = IntegerExpression::var(counter_v.clone());

        let every = c.every - 1;
        TransitionSystem {
            states: map! {counter_v => (0.into(), (counter.eq(c.every as i64).if_then_else(0,counter.clone()) + base_e.if_then_else(1, 0)).into())},
            inputs: set! {base_v,out_v},
            restriction: counter.eq(every as i64) & (base_e.eq(out_e.clone()))
                | !counter.eq(every as i64) & (!out_e),
        }
    }
}

/// (d(a,b) = delay and a=b) or (d(a,b) < d and not b)
/// check the correctness with deadline pattern: end < start $ 10 on ms --- ternary delay should tick after
impl<C: Clone + Eq + Hash> From<&'_ Delay<C>> for TransitionSystem<C> {
    fn from(c: &'_ Delay<C>) -> Self {
        let base_v = c.on.as_ref().unwrap_or(&c.trigger).clone();
        let base = BooleanExpression::var(Variable::Clock(base_v));
        let trigger = BooleanExpression::var(Variable::Clock(c.trigger.clone()));
        let counter_v = Variable::Generic("g_d".to_owned()); // FIXME: internally declared variables can conflict with inputs
        let counter = IntegerExpression::var(counter_v.clone());
        let out = BooleanExpression::var(Variable::Clock(c.out.clone()));
        let delay = c.delay as i64;
        let buffer_v = Variable::Generic("s_buf".to_owned());
        let buffer = BooleanExpression::var(buffer_v.clone());
        let sample_out = (buffer.clone() | trigger.clone()) & base.clone();

        TransitionSystem {
            states: map! {
                        buffer_v => (Constant::Bool(false), (((buffer.clone() | trigger.clone())&!base.clone()) | (buffer&trigger&base)).into()),
                        counter_v => (Constant::Int(0),(counter.clone() + sample_out.if_then_else(1, 0) - out.if_then_else(1, 0)).into(),
            ),
                    },
            inputs: [&c.trigger, &c.out]
                .into_iter()
                .chain(c.on.as_ref())
                .cloned()
                .map(Variable::Clock)
                .collect(),
            restriction: counter.more_eq(delay).implies(sample_out.eq(out.clone()))
                & counter.less(delay).implies(!out),
        }
    }
}

impl<C: Clone + Eq + Hash> From<&'_ SampleOn<C>> for TransitionSystem<C> {
    fn from(c: &'_ SampleOn<C>) -> Self {
        (&Delay {
            out: c.out.clone(),
            trigger: c.trigger.clone(),
            delay: 0,
            on: Some(c.base.clone()),
        })
            .into()
    }
}

impl<C: Clone + Eq + Hash> From<&'_ Slice<C>> for TransitionSystem<C> {
    fn from(_: &'_ Slice<C>) -> Self {
        unimplemented!()
    }
}

impl<C: Clone + Eq + Hash> From<BooleanExpression<Delta<C>, C>> for TransitionSystem<C> {
    fn from(value: BooleanExpression<Delta<C>, C>) -> Self {
        let mut ints = HashSet::new();
        let mut bools = HashSet::new();
        value.visit(&mut ints, &mut bools);

        TransitionSystem {
            inputs: bools
                .into_iter()
                .chain(ints.iter().flat_map(|d| [d.0.clone(), d.1.clone()]))
                .map(Variable::Clock)
                .collect(),
            states: ints
                .iter()
                .map(|delta_counter| {
                    let variable = Variable::Delta(delta_counter.clone());
                    let transition = default_counter_transition(&variable);
                    (variable, transition)
                })
                .collect(),
            restriction: value.map(
                &mut |v| Variable::Delta(v.clone()),
                &mut |v| Variable::Clock(v.clone()),
                &mut |c| *c,
                &mut |c| *c,
            ),
        }
    }
}

/// Synchronizes transition systems.
impl<C: Clone + Eq + Hash + Display> FromIterator<TransitionSystem<C>> for TransitionSystem<C> {
    fn from_iter<T: IntoIterator<Item = TransitionSystem<C>>>(iter: T) -> Self {
        iter.into_iter()
            .enumerate()
            .reduce(
                |(_, mut acc): (usize, TransitionSystem<C>),
                 (i, new): (usize, TransitionSystem<C>)| {
                    acc.inputs.extend(new.inputs);
                    let conflict_variables = new
                        .states
                        .iter()
                        .filter(|(var, state)| {
                            let s = if let Some(old) = acc.states.get(var) {
                                old != *state
                            } else {
                                false
                            };
                            let i = acc.inputs.contains(var);
                            s || i
                        })
                        .collect_vec();
                    let remapping: HashMap<&Variable<C>, Variable<C>> = conflict_variables
                        .iter()
                        .map(|(var, _)| (*var, Variable::Generic(format!("{}_{}", &var, i))))
                        .collect(); // FIXME: should be able to rename variables retroactively, as further constraints may introduce clocks named the same way, while clocks names cannot be changed
                    let remap = |v: &Variable<C>| {
                        if let Some(nv) = remapping.get(v) {
                            nv.clone()
                        } else {
                            v.clone()
                        }
                    };
                    acc.states.extend(
                        new.states
                            .iter()
                            .map(|(var, (init, next))| (remap(var), (*init, next.map(remap)))),
                    );

                    acc.restriction = acc.restriction
                        & new.restriction.map(
                            &mut remap.clone(),
                            &mut remap.clone(),
                            &mut |c| *c,
                            &mut |c| *c,
                        );
                    (0, acc)
                },
            )
            .unwrap()
            .1
    }
}

fn default_counter_transition<C: Clone>(v: &Variable<C>) -> (Constant, Expression<C>) {
    let d = match v {
        Variable::Delta(d) => d,
        _ => panic!("only allowed for delta counters"),
    };
    (
        Constant::Int(0),
        (IntegerExpression::var(Variable::Delta(d.clone()))
            + BooleanExpression::var(Variable::Clock(d.0.clone())).if_then_else(1, 0)
            - BooleanExpression::var(Variable::Clock(d.1.clone())).if_then_else(1, 0))
        .into(),
    )
}
