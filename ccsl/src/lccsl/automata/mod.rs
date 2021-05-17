use crate::lccsl::expressions::{BooleanExpression, Switch};
use itertools::Itertools;
use std::cmp::Ordering;
use std::collections::{BTreeMap, BTreeSet, HashMap};
use std::fmt;
use std::fmt::{Debug, Formatter};
use std::hash::{Hash, Hasher};
use std::iter::repeat;
use std::iter::FromIterator;
use std::ops::{Index, Range};
use std::sync::Arc;

pub trait Guard<D> {
    fn eval<'a>(&'a self, state: &'a dyn Index<&'a D, Output = i64>) -> bool;
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
            id: self.id.clone(),
            invariant: self.invariant.clone(),
        }
    }
}

impl<C> PartialEq for State<C> {
    fn eq(&self, other: &Self) -> bool {
        &self.id == &other.id
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
        &self.id.hash(state);
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
            invariant: Option::None,
        }
    }
    pub fn with_invariant(self, guard: BooleanExpression<Delta<C>>) -> Self {
        Self {
            id: self.id,
            invariant: Some(Arc::new(guard)),
        }
    }
}

#[derive(Debug, Clone)]
pub struct MergedTransition<'a, C> {
    pub from: StateRef,
    pub label: ClockLabel<'a, C>,
    pub switch: &'a Switch<Expr<C>, State<C>>,
}

impl<C: fmt::Display> fmt::Display for MergedTransition<'_, C> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", &self.label)
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct ClockLabel<'a, C> {
    pub present: &'a BTreeSet<C>,
    pub clocks: &'a BTreeSet<C>,
}

impl<C: Ord + Hash> ClockLabel<'_, C> {
    pub fn has_conflict(&self, rhs: &Self) -> bool {
        for v in self.clocks.intersection(&rhs.clocks) {
            if self.present.get(v) != rhs.present.get(v) {
                return true;
            }
        }
        false
    }

    pub fn has_conflict_with_map(&self, rhs: &HashMap<C, bool>) -> bool {
        for c in self.clocks {
            if let Some(v) = rhs.get(c) {
                if self.present.contains(c) != *v {
                    return true;
                }
            }
        }
        false
    }
}

#[cfg(test)]
mod tests {
    use crate::lccsl::automata::ClockLabel;
    use std::collections::{BTreeSet, HashMap};
    use std::iter::FromIterator;

    #[test]
    fn conflicts() {
        let c1 = BTreeSet::from_iter(vec!["a", "b"]);
        let c2 = BTreeSet::from_iter(vec!["b", "c"]);

        let v1 = BTreeSet::from_iter(vec!["a", "b"]);
        let v2 = BTreeSet::from_iter(vec!["b"]);

        let lab1 = ClockLabel {
            present: &v1,
            clocks: &c1,
        };
        let lab2 = ClockLabel {
            present: &v2,
            clocks: &c2,
        };

        assert_eq!(lab1.has_conflict(&lab2), false);
        assert_eq!(lab2.has_conflict(&lab1), false);
    }
    #[test]
    fn conflict_with_map() {
        let m = HashMap::from_iter(vec![("a", true), ("b", false)]);
        let c2 = BTreeSet::from_iter(vec!["b", "c"]);

        let v2 = BTreeSet::from_iter(vec!["c"]);

        let lab2 = ClockLabel {
            present: &v2,
            clocks: &c2,
        };

        assert_eq!(lab2.has_conflict_with_map(&m), false);
        assert_eq!(
            lab2.has_conflict_with_map(&HashMap::from_iter(vec![("a", true), ("b", true)])),
            true
        );
    }
    #[test]
    fn conflict_with_empty() {
        let c1 = BTreeSet::from_iter(vec!["a", "b"]);
        let c2 = BTreeSet::from_iter(vec!["b", "c"]);

        let v1 = BTreeSet::from_iter(vec!["a", "b"]);
        let v2 = BTreeSet::from_iter(vec![]);

        let lab1 = ClockLabel {
            present: &v1,
            clocks: &c1,
        };
        let lab2 = ClockLabel {
            present: &v2,
            clocks: &c2,
        };

        assert_eq!(lab1.has_conflict(&lab2), true);
        assert_eq!(lab2.has_conflict(&lab1), true);
    }

    #[test]
    fn conflict_no_common_clocks() {
        let c1 = BTreeSet::from_iter(vec!["a", "b"]);
        let c2 = BTreeSet::from_iter(vec!["c", "d"]);

        let v1 = BTreeSet::from_iter(vec!["a", "b"]);
        let v2 = BTreeSet::from_iter(vec!["c", "d"]);

        let lab1 = ClockLabel {
            present: &v1,
            clocks: &c1,
        };
        let lab2 = ClockLabel {
            present: &v2,
            clocks: &c2,
        };

        assert_eq!(lab1.has_conflict(&lab2), false);
        assert_eq!(lab2.has_conflict(&lab1), false);
    }
}

impl<C: fmt::Display> fmt::Display for ClockLabel<'_, C> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "({})", self.present.iter().join("."))
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
            .or_insert_with(|| Default::default())
            .entry(label)
            .or_insert_with(|| Switch::new());
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

#[derive(Debug, Clone)]
pub struct STS<C> {
    name: String,
    states: Vec<Option<Arc<Expr<C>>>>,
    states_to_trans: Vec<Range<usize>>,
    clocks: BTreeSet<C>,
    transitions: Vec<(BTreeSet<C>, Switch<Expr<C>, State<C>>)>,
    initial_state: usize,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct StateRef(usize);

impl<C> From<STSBuilder<C>> for STS<C> {
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
            transitions.extend(tr);
        }

        Self {
            name,
            states: state_invariants,
            states_to_trans,
            clocks,
            transitions,
            initial_state,
        }
    }
}

impl<C> STS<C> {
    pub fn initial(&self) -> StateRef {
        StateRef(self.initial_state)
    }

    pub fn states<'a>(&'a self) -> impl Iterator<Item = StateRef> + 'a + Clone + Debug {
        (0..self.states.len()).map(|i| StateRef(i))
    }
    pub fn full_states<'a>(&'a self) -> impl Iterator<Item = State<C>> + 'a + Clone {
        self.states.iter().enumerate().map(|(i, inv)| State {
            id: i,
            invariant: inv.clone(),
        })
    }

    pub fn transitions(
        &self,
        state: StateRef,
    ) -> impl Iterator<Item = MergedTransition<C>> + Clone {
        self.transitions
            .get(self.states_to_trans[state.0].clone())
            .unwrap()
            .into_iter()
            .map(move |(label, switch)| MergedTransition {
                from: state.clone(),
                label: ClockLabel {
                    present: label,
                    clocks: &self.clocks,
                },
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

impl<C: Clone + Hash + Eq> STS<C> {
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
        }
    }
}

impl<C> fmt::Display for STS<C> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}

impl<'a, C> From<&'a STS<C>> for petgraph::Graph<State<C>, ClockLabel<'a, C>>
where
    C: Clone,
{
    fn from(system: &'a STS<C>) -> Self {
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
                        t.label.clone(),
                    );
                }
            }
        }
        graph
    }
}
