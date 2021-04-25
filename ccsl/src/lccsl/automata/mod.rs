use crate::lccsl::expressions::{BooleanExpression, Switch};
use itertools::Itertools;
use std::collections::{BTreeSet, HashMap, HashSet};
use std::fmt;
use std::fmt::{Debug, Formatter};
use std::hash::{Hash, Hasher};
use std::iter::repeat;
use std::iter::FromIterator;
use std::marker::PhantomData;
use std::ops::Index;
use std::rc::Rc;

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
pub struct State<G>
where
    G: ?Sized,
{
    pub id: usize,
    pub invariant: Option<Rc<G>>,
}

impl<G: ?Sized> Clone for State<G> {
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
            invariant: Some(Rc::new(guard)),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Transition<'a, C, G> {
    pub from: State<G>,
    pub to: State<G>,
    pub label: ClockLabel<'a, C>,
    pub guard: Option<Rc<G>>,
}

impl<C: fmt::Display, G: fmt::Display> fmt::Display for Transition<'_, C, G> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", &self.label)?;
        if let Some(g) = &self.guard {
            write!(f, " [")?;
            write!(f, "{}", g)?;
            write!(f, "]")
        } else {
            Ok(())
        }
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct ClockLabel<'a, C> {
    pub present: &'a BTreeSet<C>,
    pub clocks: &'a BTreeSet<C>,
}

impl<C: Ord + Hash> ClockLabel<'_, C> {
    pub fn has_conflict(&self, rhs: &Self) -> bool {
        if let Some(first) = self.clocks.intersection(&rhs.clocks).next() {
            self.present
                .range(first..)
                .zip_longest(rhs.present.range(first..))
                .any(|pair| pair.both().map(|(l, r)| l != r).unwrap_or(true))
        } else {
            false
        }
    }

    pub fn has_conflict_with_map(&self, rhs: &HashMap<C, bool>) -> bool {
        for c in self.present {
            if let Some(ok) = rhs.get(c) {
                if !ok {
                    return false;
                }
            }
        }
        false
    }
}

impl<C: fmt::Display> fmt::Display for ClockLabel<'_, C> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "(")?;

        let last: usize = self.present.len() - 1;
        for (i, clock) in self.present.iter().enumerate() {
            write!(f, "{}", clock)?;
            if i != last {
                write!(f, ".")?;
            }
        }
        write!(f, ")")
    }
}

#[derive(Debug, Clone)]
pub struct LabeledTransitionSystem<C, D, G>
where
    G: Guard<D>,
{
    name: String,
    states: HashSet<State<G>>,
    clocks: BTreeSet<C>,
    transitions: HashMap<State<G>, HashMap<BTreeSet<C>, Switch<G, State<G>>>>,
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
        let mut states = HashSet::with_capacity(1);
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
            .or_insert_with(|| HashMap::new())
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

    pub fn states(&self) -> &HashSet<State<G>> {
        &self.states
    }

    pub fn transitions<'a>(
        &'a self,
        state: &'a State<G>,
    ) -> impl Iterator<Item = Transition<'a, C, G>> + Clone {
        self.transitions
            .get(&state)
            .into_iter()
            .flat_map(move |trans: &'a HashMap<_, _>| {
                trans
                    .iter()
                    .zip(repeat((self, state)))
                    .flat_map(|((label, switch), (s, state))| {
                        switch.variants().map(move |(g, to)| Transition {
                            from: state.clone(),
                            to: to.clone(),
                            label: ClockLabel {
                                present: label,
                                clocks: &s.clocks,
                            },
                            guard: None,
                        })
                    })
            })
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
                .or_insert_with(|| HashMap::new());
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
    for petgraph::Graph<&'a State<G>, Transition<'a, C, G>>
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
                graph.add_edge(
                    *nodes.get(&t.from.id).unwrap(),
                    *nodes.get(&t.to.id).unwrap(),
                    t,
                );
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
            $container.push(($e, false));
            trigger_value!($container, $($tail)*)
        };
        ($container:ident, $e:expr, $($tail:tt)*) => {
            $container.push(($e, true));
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
