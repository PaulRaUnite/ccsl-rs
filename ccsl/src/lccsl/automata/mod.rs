use crate::lccsl::expressions::{BooleanExpression, Switch};
use itertools::Itertools;
use label::*;
use std::cmp::Ordering;
use std::collections::{BTreeMap, BTreeSet, HashMap};
use std::fmt;
use std::fmt::{Debug, Formatter};
use std::hash::{Hash, Hasher};
use std::iter::{once, FromIterator};
use std::ops::{BitOr, Index, Range};
use std::sync::Arc;

pub mod label;

pub trait Guard<V, C> {
    fn eval<'a>(&'a self, state: &'a dyn Index<&'a V, Output = C>) -> bool;
}

#[derive(Debug, Copy, Clone, PartialOrd, PartialEq, Eq, Ord, Hash)]
pub struct Delta<C>(pub C, pub C);

impl<C: fmt::Display> fmt::Display for Delta<C> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "δ({}, {})", self.0, self.1)
    }
}
#[derive(Debug)]
pub struct State<C> {
    pub id: usize,
    pub invariant: Option<Arc<BooleanExpression<Delta<C>>>>,
}

impl<C> Clone for State<C> {
    fn clone(&self) -> Self {
        Self {
            id: self.id,
            invariant: self.invariant.clone(),
        }
    }
}

impl<C> PartialEq for State<C> {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl<C> PartialOrd for State<C> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.id.partial_cmp(&other.id)
    }
}

impl<C> Ord for State<C> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.id.cmp(&other.id)
    }
}

impl<C> Hash for State<C> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.id.hash(state);
    }
}

impl<C> Eq for State<C> {}

impl<C: fmt::Display> fmt::Display for State<C> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.id)?;
        if let Some(g) = &self.invariant {
            write!(f, "[{}]", g)
        } else {
            Ok(())
        }
    }
}

impl<C> State<C> {
    pub const fn new(id: usize) -> State<C> {
        Self {
            id,
            invariant: None,
        }
    }
    pub fn with_invariant(self, guard: BooleanExpression<Delta<C>>) -> Self {
        Self {
            id: self.id,
            invariant: Some(Arc::new(guard)),
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub struct MergedTransition<'a, C, L> {
    pub from: StateRef,
    pub label: &'a L,
    pub switch: &'a Switch<Expr<C>, State<C>>,
}

impl<C, L: fmt::Display> fmt::Display for MergedTransition<'_, C, L> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.label)
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_cyclic_spec() {
        // a < b < c; c = a $ 1
    }
}

pub type Expr<C> = BooleanExpression<Delta<C>>;

#[derive(Debug, Clone)]
pub struct STSBuilder<C> {
    name: String,
    states: BTreeSet<State<C>>,
    clocks: BTreeSet<C>,
    transitions: HashMap<State<C>, BTreeMap<BTreeSet<C>, Switch<Expr<C>, State<C>>>>,
    initial_state: State<C>,
}

impl<C> STSBuilder<C>
where
    C: Eq + Hash + Clone + Ord,
{
    pub fn new(name: impl ToString, initial: State<C>) -> Self {
        let mut states = BTreeSet::new();
        states.insert(initial.clone());
        Self {
            name: name.to_string(),
            states,
            clocks: Default::default(),
            transitions: HashMap::new(),
            initial_state: initial,
        }
    }

    pub fn set_name(&mut self, name: impl ToString) {
        self.name = name.to_string();
    }

    pub fn initial(&self) -> &State<C> {
        &self.initial_state
    }

    fn add_transition_with_guard_private<I>(
        &mut self,
        from: &State<C>,
        to: &State<C>,
        label: I,
        guard: Option<Expr<C>>,
    ) where
        I: IntoIterator<Item = (C, bool)>,
        for<'a> &'a I: IntoIterator<Item = &'a (C, bool)>,
    {
        {
            self.states.insert(from.clone());
            self.states.insert(to.clone());
            let label = &label;
            self.clocks
                .extend(label.into_iter().map(|(c, _)| c.clone()));
        }
        let label =
            BTreeSet::from_iter(
                label
                    .into_iter()
                    .filter_map(|(c, b): (C, bool)| if b { Some(c) } else { None }),
            );
        let switch = self
            .transitions
            .entry(from.clone())
            .or_insert_with(Default::default)
            .entry(label)
            .or_insert_with(Switch::new);
        if let Some(guard) = guard {
            switch.add_variant(guard, to.clone())
        } else {
            switch.add_default_variant(to.clone())
        }
    }
    pub fn add_transition<I>(&mut self, from: &State<C>, to: &State<C>, label: I)
    where
        I: IntoIterator<Item = (C, bool)>,
        for<'a> &'a I: IntoIterator<Item = &'a (C, bool)>,
    {
        self.add_transition_with_guard_private(from, to, label, None);
    }
    pub fn add_transition_with_guard<I>(
        &mut self,
        from: &State<C>,
        to: &State<C>,
        label: I,
        guard: Expr<C>,
    ) where
        I: IntoIterator<Item = (C, bool)>,
        for<'a> &'a I: IntoIterator<Item = &'a (C, bool)>,
    {
        self.add_transition_with_guard_private(from, to, label, Some(guard));
    }

    pub fn transitions_len(&self, from: &State<C>) -> usize {
        self.transitions.get(from).map(|h| h.len()).unwrap_or(0)
    }
}

impl<C> fmt::Display for STSBuilder<C> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}

#[macro_use]
mod macros {
    // TODO: replace with vec!
    #[macro_export]
    macro_rules! trigger_value {
        ($container:ident, !$e:expr, $($tail:tt)*) => {
            $container.push(((&$e).clone(), false));
            trigger_value!($container, $($tail)*)
        };
        ($container:ident, $e:expr, $($tail:tt)*) => {
            $container.push(((&$e).clone(), true));
            trigger_value!($container, $($tail)*)
        };
        ($container:ident,) => {}
    }

    #[macro_export]
    macro_rules! trigger {
        ($($tts:tt)*) => {{
            let mut container = Vec::new();
            trigger_value!{container, $($tts)*};
            container
        }};
    }

    #[macro_export]
    macro_rules! tr {
        ($s:expr, $from:expr => $to:expr, {}) => {
            $s.add_transition($from, $to, vec![]);
        };
        ($s:expr, $guard:expr, $from:expr => $to:expr, $($tts:tt)*) => {
            $s.add_transition_with_guard($from, $to, trigger!$($tts)*, $guard);
        };
        ($s:expr, $from:expr => $to:expr, $($tts:tt)*) => {
            $s.add_transition($from, $to, trigger!$($tts)*);
        };
    }
}

// TODO: generalize so that static containers can be used
#[derive(Debug, Clone)]
pub struct STS<C, L> {
    name: String,
    states: Vec<Option<Arc<Expr<C>>>>,
    states_to_trans: Vec<Range<usize>>,
    clocks: BTreeSet<C>,
    transitions: Vec<(L, Switch<Expr<C>, State<C>>)>,
    initial_state: usize,
    empty_transition: (L, Switch<Expr<C>, State<C>>),
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct StateRef(pub usize);

impl<C: Ord + Clone, L: Label<C>> From<STSBuilder<C>> for STS<C, L>
where
    for<'a, 'b> &'a L: BitOr<&'b L, Output = L>,
{
    fn from(system: STSBuilder<C>) -> Self {
        let name = system.name;
        let clocks = system.clocks;
        let index: HashMap<_, _> = system
            .states
            .into_iter()
            .enumerate()
            .map(|(i, v)| (v, i))
            .collect();
        let initial_state = *index.get(&system.initial_state).unwrap();
        let mut state_invariants = vec![None; index.len()];
        for (s, i) in index.iter() {
            state_invariants[*i] = s.invariant.clone();
        }

        let mut transitions =
            Vec::with_capacity(system.transitions.values().map(|t| t.len()).sum());
        let mut states_to_trans = vec![0..0; system.transitions.len()];
        for (state, tr) in system.transitions {
            let start = transitions.len();
            let finish = start + tr.len();
            states_to_trans[*index.get(&state).unwrap()] = start..finish;
            transitions.extend(
                tr.into_iter()
                    .map(|(label, s)| ((&clocks, &label).into(), s)),
            );
        }
        let empty_transition: (L, Switch<_, _>) =
            ((&clocks, &BTreeSet::new()).into(), Switch::new());

        Self {
            name,
            states: state_invariants,
            states_to_trans,
            clocks,
            transitions,
            initial_state,
            empty_transition,
        }
    }
}

impl<C, L> STS<C, L> {
    pub fn initial(&self) -> StateRef {
        StateRef(self.initial_state)
    }

    pub fn states(&self) -> impl Iterator<Item = StateRef> + '_ + Clone + Debug {
        (0..self.states.len()).map(StateRef)
    }
    pub fn full_states(&self) -> impl Iterator<Item = State<C>> + '_ + Clone {
        self.states.iter().enumerate().map(|(i, inv)| State {
            id: i,
            invariant: inv.clone(),
        })
    }

    pub fn transitions(
        &self,
        state: StateRef,
    ) -> impl Iterator<Item = MergedTransition<C, L>> + Clone {
        self.transitions
            .get(self.states_to_trans[state.0].clone())
            .unwrap()
            .iter()
            .chain(once(&self.empty_transition)) //TODO: switch should point to the state
            .map(move |(label, switch)| MergedTransition {
                from: state,
                label,
                switch,
            })
    }

    pub fn clocks(&self) -> &BTreeSet<C> {
        &self.clocks
    }

    pub fn transitions_len(&self, from: StateRef) -> usize {
        self.states_to_trans[from.0].len()
    }
}

impl<C, L: Clone + Hash + Eq> STS<C, L> {
    pub fn squish(self) -> Self {
        let transitions = self
            .transitions
            .into_iter()
            .map(|(label, _)| label)
            .unique()
            .map(|label| {
                let mut switch = Switch::new();
                switch.add_default_variant(State::new(0));
                (label, switch)
            })
            .collect_vec();
        Self {
            name: self.name,
            states: vec![None],
            states_to_trans: vec![0..transitions.len()],
            clocks: self.clocks,
            transitions,
            initial_state: 0,
            empty_transition: self.empty_transition,
        }
    }
}

impl<C, L> fmt::Display for STS<C, L> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}

impl<'a, C, L> From<&'a STS<C, L>> for petgraph::Graph<State<C>, &'a L> {
    fn from(system: &'a STS<C, L>) -> Self {
        let mut graph = petgraph::Graph::new();
        let nodes: HashMap<_, _> = system
            .full_states()
            .map(|s| (s.id, graph.add_node(s)))
            .collect();
        for s in system.states() {
            for t in system.transitions(s) {
                for (_, to) in t.switch.variants() {
                    graph.add_edge(
                        *nodes.get(&t.from.0).unwrap(),
                        *nodes.get(&to.id).unwrap(),
                        t.label,
                    );
                }
            }
        }
        graph
    }
}
