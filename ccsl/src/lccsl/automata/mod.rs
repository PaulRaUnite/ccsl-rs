use crate::lccsl::expressions::{BooleanExpression, Switch};
use itertools::Itertools;
use sorted_iter::SortedIterator;
use std::cmp::Ordering;
use std::collections::{BTreeMap, BTreeSet, HashMap};
use std::fmt;
use std::fmt::{Debug, Formatter};
use std::hash::{Hash, Hasher};
use std::iter::repeat;
use std::iter::FromIterator;
use std::marker::PhantomData;
use std::ops::Index;
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
pub struct State<G> {
    pub id: usize,
    pub invariant: Option<Arc<G>>,
}

impl<G> Clone for State<G> {
    fn clone(&self) -> Self {
        Self {
            id: self.id.clone(),
            invariant: self.invariant.clone(),
        }
    }
}

impl<G> PartialEq for State<G> {
    fn eq(&self, other: &Self) -> bool {
        &self.id == &other.id
    }
}

impl<G> PartialOrd for State<G> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.id.partial_cmp(&other.id)
    }
}

impl<G> Ord for State<G> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.id.cmp(&other.id)
    }
}

impl<G> Hash for State<G> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        &self.id.hash(state);
    }
}

impl<G> Eq for State<G> {}

impl<G: fmt::Display> fmt::Display for State<G> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.id)?;
        if let Some(g) = &self.invariant {
            write!(f, "[{}]", g)
        } else {
            Ok(())
        }
    }
}

impl<G> State<G> {
    pub const fn new(id: usize) -> State<G> {
        Self {
            id,
            invariant: Option::None,
        }
    }
    pub fn with_invariant(self, guard: G) -> Self {
        Self {
            id: self.id,
            invariant: Some(Arc::new(guard)),
        }
    }
}

#[derive(Debug, Clone)]
pub struct MergedTransition<'a, C, G> {
    pub from: State<G>,
    pub label: ClockLabel<'a, C>,
    pub switch: &'a Switch<G, State<G>>,
}

impl<C: fmt::Display, G: fmt::Display> fmt::Display for MergedTransition<'_, C, G> {
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
        for v in self.clocks.iter().intersection(rhs.clocks.iter()) {
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

#[derive(Debug, Clone)]
pub struct LabeledTransitionSystem<C, D, G> {
    name: String,
    states: BTreeSet<State<G>>,
    clocks: BTreeSet<C>,
    transitions: HashMap<State<G>, BTreeMap<BTreeSet<C>, Switch<G, State<G>>>>,
    initial_state: State<G>,
    _phantom: PhantomData<D>,
}

pub type STS<C> = LabeledTransitionSystem<C, Delta<C>, BooleanExpression<Delta<C>>>;

impl<C, D, G> LabeledTransitionSystem<C, D, G>
where
    C: Eq + Hash + Clone + Ord,
    G: Guard<D> + Default,
{
    pub fn new(name: impl ToString, initial: State<G>) -> Self {
        let mut states = BTreeSet::new();
        states.insert(initial.clone());
        Self {
            name: name.to_string(),
            states,
            clocks: Default::default(),
            transitions: HashMap::new(),
            initial_state: initial,
            _phantom: PhantomData,
        }
    }

    pub fn initial(&self) -> &State<G> {
        &self.initial_state
    }

    fn add_transition_with_guard_private<I>(
        &mut self,
        from: &State<G>,
        to: &State<G>,
        label: I,
        guard: Option<G>,
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
    pub fn add_transition<I>(&mut self, from: &State<G>, to: &State<G>, label: I)
    where
        I: IntoIterator<Item = (C, bool)>,
        for<'a> &'a I: IntoIterator<Item = &'a (C, bool)>,
    {
        self.add_transition_with_guard_private(from, to, label, None);
    }
    pub fn add_transition_with_guard<I>(
        &mut self,
        from: &State<G>,
        to: &State<G>,
        label: I,
        guard: G,
    ) where
        I: IntoIterator<Item = (C, bool)>,
        for<'a> &'a I: IntoIterator<Item = &'a (C, bool)>,
    {
        self.add_transition_with_guard_private(from, to, label, Some(guard));
    }

    pub fn states(&self) -> &BTreeSet<State<G>> {
        &self.states
    }

    pub fn transitions<'a>(
        &'a self,
        state: &'a State<G>,
    ) -> impl Iterator<Item = MergedTransition<'a, C, G>> + Clone {
        self.transitions.get(&state).into_iter().flat_map(
            move |trans: &'a BTreeMap<_, Switch<G, State<G>>>| {
                trans
                    .iter()
                    .zip(repeat((self, state)))
                    .map(|((label, switch), (s, state))| MergedTransition {
                        from: state.clone(),
                        label: ClockLabel {
                            present: label,
                            clocks: &s.clocks,
                        },
                        switch,
                    })
            },
        )
    }

    pub fn clocks(&self) -> &BTreeSet<C> {
        &self.clocks
    }

    pub fn squish(self) -> Self {
        let state = State::new(0);
        let mut system = LabeledTransitionSystem::new(self.name, state.clone());

        system.clocks = self.clocks;
        for label in self
            .transitions
            .into_iter()
            .map(|(_, t)| t.into_iter().map(|(k, v)| k))
            .flatten()
        {
            let map = system
                .transitions
                .entry(state.clone())
                .or_insert_with(|| Default::default());
            map.entry(label).or_insert_with(|| {
                let mut s = Switch::new();
                s.add_default_variant(state.clone());
                s
            });
        }
        system
    }

    pub fn transitions_len(&self, from: &State<G>) -> usize {
        self.transitions.get(from).map(|h| h.len()).unwrap_or(0)
    }
}

impl<'a, C, D, G> From<&'a LabeledTransitionSystem<C, D, G>>
    for petgraph::Graph<&'a State<G>, ClockLabel<'a, C>>
where
    C: Eq + Hash + Clone + Ord,
    G: Guard<D> + Default,
{
    fn from(system: &'a LabeledTransitionSystem<C, D, G>) -> Self {
        let mut graph = petgraph::Graph::new();
        let nodes: HashMap<_, _> = system
            .states
            .iter()
            .map(|s| (s.id, graph.add_node(s)))
            .collect();
        for s in system.transitions.keys() {
            for t in system.transitions(s) {
                for (g, to) in t.switch.variants() {
                    graph.add_edge(
                        *nodes.get(&t.from.id).unwrap(),
                        *nodes.get(&to.id).unwrap(),
                        t.label.clone(),
                    );
                }
            }
        }
        graph
    }
}

impl<C, D, G: Guard<D>> fmt::Display for LabeledTransitionSystem<C, D, G> {
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
