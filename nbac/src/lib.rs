#[macro_use]
extern crate derive_more;

use ccsl::symbolic::ts::{Constant, TransitionSystem};
use itertools::Itertools;
use std::borrow::Cow;
use std::collections::BTreeMap;
use std::fmt::{Debug, Display, Formatter};
use std::ops::{BitAnd, BitOr};

#[derive(Debug, Clone, Ord, PartialOrd, Eq, PartialEq)]
pub enum VariableDeclaration<V> {
    Bool(V),
    Integer(V),
    //Real(String),
}

impl<V> VariableDeclaration<V> {
    fn bool(bool: impl Into<V>) -> Self {
        Self::Bool(bool.into())
    }
    fn int(int: impl Into<V>) -> Self {
        Self::Integer(int.into())
    }
}

impl<V> VariableDeclaration<V> {
    pub fn variable(&self) -> &V {
        match self {
            VariableDeclaration::Bool(v) => v,
            VariableDeclaration::Integer(v) => v,
        }
    }
}

pub mod expr;
pub use expr::*;

#[derive(Debug, Clone)]
pub struct Spec<V> {
    pub states: Vec<VariableDeclaration<V>>,
    pub inputs: Vec<VariableDeclaration<V>>,
    pub locals: Vec<VariableDeclaration<V>>,
    pub definitions: BTreeMap<V, Expression<V>>,
    pub transitions: BTreeMap<V, Expression<V>>,
    pub assertion: Option<BooleanExpression<V>>,
    pub initial_state: BooleanExpression<V>,
    pub final_state: BooleanExpression<V>,
}

impl<V: Display + Ord> Display for Spec<V> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        fn declarations<V: Display + Ord>(
            f: &mut Formatter<'_>,
            group: &str,
            declarations: &Vec<VariableDeclaration<V>>,
        ) -> std::fmt::Result {
            if declarations.is_empty() {
                return Ok(());
            }
            let declarations = declarations.iter().sorted().collect_vec();
            writeln!(f, "{}", group)?;
            for s in declarations {
                let (v, type_str) = match s {
                    VariableDeclaration::Bool(v) => (v, "bool"),
                    VariableDeclaration::Integer(v) => (v, "int"),
                };
                writeln!(f, "    {}: {};", v, type_str)?;
            }
            Ok(())
        }
        declarations(f, "state", &self.states)?;
        declarations(f, "input", &self.inputs)?;
        declarations(f, "local", &self.locals)?;
        if !self.definitions.is_empty() {
            writeln!(f, "definition")?;
            for (v, expr) in &self.definitions {
                writeln!(f, "    {} = {};", v, expr)?;
            }
        }
        if !self.transitions.is_empty() {
            writeln!(f, "transition")?;
            for (v, expr) in &self.transitions {
                writeln!(f, "    {}' = {};", v, expr)?;
            }
        }
        if let Some(assertion) = &self.assertion {
            writeln!(f, "assertion {};", assertion)?;
        }
        writeln!(f, "initial {};", self.initial_state)?;
        write!(f, "final {};", self.final_state)?;

        Ok(())
    }
}

impl<V> Spec<V> {
    pub fn new(
        initial_state: impl Into<BooleanExpression<V>>,
        final_state: impl Into<BooleanExpression<V>>,
    ) -> Self {
        Self {
            states: vec![],
            inputs: vec![],
            locals: vec![],
            definitions: BTreeMap::new(),
            transitions: BTreeMap::new(),
            assertion: None,
            initial_state: initial_state.into(),
            final_state: final_state.into(),
        }
    }

    pub fn transit(&mut self, var: impl Into<V>, expr: impl Into<Expression<V>>)
    where
        V: Display + Ord,
    {
        let expr = expr.into();
        println!("added {}", &expr);
        let prev = self.transitions.insert(var.into(), expr);
        if let Some(prev) = prev {
            println!("  replaces {prev}");
        }
    }
}

type Variable = Cow<'static, str>;
impl<C: Display + Debug> From<TransitionSystem<C>> for Spec<Variable> {
    fn from(value: TransitionSystem<C>) -> Self {
        let initial = BooleanExpression::var("init");
        let ok = BooleanExpression::var("ok");
        let mut nbac_spec = Spec::<Variable>::new(initial.clone(), !(initial.clone() | ok));
        nbac_spec.states.push(VariableDeclaration::bool("init"));
        nbac_spec.states.push(VariableDeclaration::bool("ok"));
        nbac_spec.transit("init", BooleanExpression::from(false));

        nbac_spec.inputs.extend(
            value
                .inputs
                .into_iter()
                .map(|v| VariableDeclaration::bool(v.to_string())),
        );
        nbac_spec
            .states
            .extend(value.states.iter().map(|(v, (init, _))| match init {
                Constant::Bool(_) => VariableDeclaration::bool(v.to_string()),
                Constant::Int(_) => VariableDeclaration::int(v.to_string()),
            }));
        let mut f = |v: &ccsl::symbolic::ts::Variable<C>| Variable::Owned(v.to_string());
        for (var, (init, next)) in value.states.into_iter() {
            match (init, next) {
                (Constant::Bool(init), ccsl::symbolic::ts::Expression::Bool(next)) => {
                    nbac_spec.transit(
                        f(&var),
                        initial.if_then_elseb(
                            BooleanExpression::Constant(init),
                            next.map_var(&mut f.clone(), &mut f.clone()),
                        ),
                    );
                }
                (Constant::Int(init), ccsl::symbolic::ts::Expression::Int(next)) => {
                    nbac_spec.transit(
                        f(&var),
                        initial.if_then_else(
                            IntegerExpression::Constant(init),
                            next.map_var(&mut f.clone(), &mut f.clone()),
                        ),
                    );
                }
                (init, next) => panic!("typing error in {:?} and {:?}", init, next),
            }
        }
        nbac_spec.assertion =
            Some(initial | value.restriction.map_var(&mut f.clone(), &mut f).into());
        nbac_spec
    }
}

pub fn add_boundness_goal(mut spec: Spec<Variable>, bound: u32) -> Spec<Variable> {
    let ok = BooleanExpression::var("ok".to_owned());
    let init = BooleanExpression::var("init".to_owned());
    spec.transit(
        "ok".to_owned(),
        init.if_then_elseb(
            true,
            spec.states
                .iter()
                .filter_map(|s| match s {
                    VariableDeclaration::Bool(_) => None,
                    VariableDeclaration::Integer(v) => {
                        let v = IntegerExpression::var(v.clone());
                        Some(v.less_eq(bound as i64) & v.more_eq(-(bound as i64)))
                    }
                })
                .reduce(BitAnd::bitand)
                .map_or(ok.clone(), |v| ok & v),
        ),
    );
    spec
}
pub fn add_deadlock_goal(mut spec: Spec<Variable>, step_limit: u32) -> Spec<Variable> {
    let ok = BooleanExpression::var("ok".to_owned());
    let init = BooleanExpression::var("init".to_owned());
    let clocks = spec
        .inputs
        .iter()
        .map(|d| d.variable())
        .cloned()
        .collect_vec();
    for v in &clocks {
        let dead_str = format!("dead_{}", v);
        let kill_str = format!("kill_{}", v);

        spec.inputs
            .push(VariableDeclaration::bool(kill_str.clone()));
        spec.states
            .push(VariableDeclaration::bool(dead_str.clone()));
        let dead = BooleanExpression::var(dead_str.clone());
        let kill = BooleanExpression::var(kill_str);
        spec.transit(dead_str, init.if_then_elseb(false, dead | kill));
    }

    let lock_count = IntegerExpression::var("lock_count".to_owned());
    spec.transit(
        "lock_count".to_owned(),
        init.if_then_else(
            0,
            (clocks
                .iter()
                .map(|v| BooleanExpression::var(format!("dead_{}", v)))
                .reduce(BitOr::bitor)
                .unwrap()
                & !clocks
                    .iter()
                    .map(|v| BooleanExpression::var(format!("dead_{}", v)))
                    .reduce(BitAnd::bitand)
                    .unwrap()) // TODO: not sure
            .if_then_else(lock_count.clone() + 1i64.into(), lock_count.clone()),
        ),
    );
    spec.states.push(VariableDeclaration::int("lock_count"));
    spec.assertion = Some(match spec.assertion.unwrap() {
        BooleanExpression::BooleanBinary { kind, left, right } => {
            BooleanExpression::BooleanBinary {
                kind,
                left: Box::new(
                    *left
                        & clocks
                            .iter()
                            .map(|c| {
                                BooleanExpression::var(format!("dead_{}", c))
                                    .implies(BooleanExpression::var(c.to_string()))
                            })
                            .reduce(BitAnd::bitand)
                            .unwrap(),
                ),
                right,
            }
        }
        _ => panic!("expected boolean or"),
    });
    spec.transit(
        "ok".to_owned(),
        init.if_then_elseb(true, ok & lock_count.less_eq(step_limit as i64)),
    );
    spec
}
