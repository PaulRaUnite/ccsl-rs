#[macro_use]
extern crate derive_more;

use itertools::Itertools;
use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use std::hash::Hash;

#[derive(Debug, Clone, Ord, PartialOrd, Eq, PartialEq)]
pub enum StateDeclaration<V> {
    Bool(V),
    Integer(V),
    //Real(String),
}

impl<V> StateDeclaration<V> {
    pub fn variable(&self) -> &V {
        match self {
            StateDeclaration::Bool(v) => v,
            StateDeclaration::Integer(v) => v,
        }
    }
}

pub mod expr;
pub use expr::*;

#[derive(Debug, Clone)]
pub struct Spec<V> {
    pub states: Vec<StateDeclaration<V>>,
    pub inputs: Vec<StateDeclaration<V>>,
    pub locals: Vec<StateDeclaration<V>>,
    pub definitions: HashMap<V, Expression<V>>,
    pub transitions: HashMap<V, Expression<V>>,
    pub assertion: Option<BooleanExpression<V>>,
    pub initial_state: BooleanExpression<V>,
    pub final_state: BooleanExpression<V>,
}

impl<V: Display + Ord> Display for Spec<V> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        // TODO: sort group declarations by type?
        fn declarations<V: Display + Ord>(
            f: &mut Formatter<'_>,
            group: &str,
            declarations: &Vec<StateDeclaration<V>>,
        ) -> std::fmt::Result {
            if declarations.is_empty() {
                return Ok(());
            }
            let declarations = declarations.iter().sorted().collect_vec();
            writeln!(f, "{}", group)?;
            for s in declarations {
                let (v, type_str) = match s {
                    StateDeclaration::Bool(v) => (v, "bool"),
                    StateDeclaration::Integer(v) => (v, "int"),
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
            definitions: HashMap::new(),
            transitions: HashMap::new(),
            assertion: None,
            initial_state: initial_state.into(),
            final_state: final_state.into(),
        }
    }

    pub fn transit(&mut self, var: V, expr: impl Into<Expression<V>>)
    where
        V: Display + Hash + Eq,
    {
        let expr = expr.into();
        println!("added {}", &expr);
        let prev = self.transitions.insert(var, expr);
        if let Some(prev) = prev {
            println!("  replaces {prev}");
        }
    }
}
