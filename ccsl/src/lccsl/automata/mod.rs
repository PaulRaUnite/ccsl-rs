use crate::lccsl::expressions::BooleanExpression;
use std::cmp::Ordering;
use std::collections::{BTreeMap, HashMap, HashSet};
use std::fmt;
use std::fmt::{Debug, Formatter};
use std::hash::{Hash, Hasher};
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
pub struct Transition<L, G> {
    pub from: State<G>,
    pub to: State<G>,
    pub label: L,
    pub guard: Option<Rc<G>>,
}

impl<L: fmt::Display, G: fmt::Display> fmt::Display for Transition<L, G> {
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

impl<L, G> Transition<L, G> {
    const fn new(from: State<G>, to: State<G>, label: L) -> Self {
        Self {
            from,
            to,
            label,
            guard: Option::None,
        }
    }

    fn with_guard(self, guard: G) -> Self {
        Self {
            guard: Some(Rc::new(guard)),
            ..self
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct ClockLabel<C> {
    map: BTreeMap<C, bool>,
}

impl<C: Ord> ClockLabel<C> {
    pub fn empty() -> Self {
        Self {
            map: BTreeMap::new(),
        }
    }
}

impl<C: Ord> FromIterator<(C, bool)> for ClockLabel<C> {
    fn from_iter<T: IntoIterator<Item = (C, bool)>>(iter: T) -> Self {
        Self {
            map: FromIterator::from_iter(iter.into_iter().filter(|(_, b)| *b)),
        }
    }
}

impl<'a, C: Ord + Clone> FromIterator<(&'a C, bool)> for ClockLabel<C> {
    fn from_iter<T: IntoIterator<Item = (&'a C, bool)>>(iter: T) -> Self {
        Self {
            map: FromIterator::from_iter(
                iter.into_iter()
                    .filter(|(_, b)| *b)
                    .map(|(c, b)| (c.clone(), b)),
            ),
        }
    }
}

impl<C: Ord> PartialOrd<Self> for ClockLabel<C> {
    fn partial_cmp(&self, other: &ClockLabel<C>) -> Option<Ordering> {
        self.map.partial_cmp(&other.map)
    }
}

impl<C: Ord> Ord for ClockLabel<C> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.map.cmp(&other.map)
    }
}

impl<C: fmt::Display> fmt::Display for ClockLabel<C> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "(")?;

        let last: usize = self.map.len() - 1;
        for (i, (clock, present)) in self.map.iter().enumerate() {
            if !present {
                write!(f, "!")?;
            }
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
    clocks: HashSet<C>,
    transitions: Vec<Transition<ClockLabel<C>, G>>,
    initial_state: State<G>,
    _phantom: PhantomData<D>,
}

pub type STS<C> = LabeledTransitionSystem<C, Delta<C>, BooleanExpression<Delta<C>>>;

impl<C, D, G> LabeledTransitionSystem<C, D, G>
where
    C: Eq + Hash + Clone,
    G: Guard<D>,
{
    pub fn new(name: impl ToString, initial: State<G>) -> Self {
        let mut states = HashSet::with_capacity(1);
        states.insert(initial.clone());
        Self {
            name: name.to_string(),
            states,
            clocks: Default::default(),
            transitions: vec![],
            initial_state: initial,
            _phantom: PhantomData,
        }
    }

    pub fn initial(&self) -> &State<G> {
        &self.initial_state
    }

    pub fn add_transition(&mut self, from: &State<G>, to: &State<G>, label: ClockLabel<C>) {
        self.states.insert(from.clone());
        self.states.insert(to.clone());
        self.clocks
            .extend(label.map.clone().into_iter().map(|(c, _)| c));
        self.transitions
            .push(Transition::new(from.clone(), to.clone(), label))
    }
    pub fn add_transition_with_guard(
        &mut self,
        from: &State<G>,
        to: &State<G>,
        label: ClockLabel<C>,
        guard: G,
    ) {
        self.states.insert(from.clone());
        self.states.insert(to.clone());
        self.clocks
            .extend(label.map.clone().into_iter().map(|(c, _)| c));
        self.transitions
            .push(Transition::new(from.clone(), to.clone(), label).with_guard(guard))
    }

    pub fn states(&self) -> &HashSet<State<G>> {
        &self.states
    }

    pub fn transitions(
        &self,
        state: State<G>,
    ) -> impl Iterator<Item = &Transition<ClockLabel<C>, G>> + Clone {
        self.transitions.iter().filter(move |t| t.from == state)
    }

    pub fn clocks(&self) -> &HashSet<C> {
        &self.clocks
    }

    pub fn squish(self) -> Self {
        let state = State::new(0);
        let mut system = LabeledTransitionSystem::new(self.name, state.clone());

        system.clocks = self.clocks;
        system.transitions = self
            .transitions
            .into_iter()
            .map(|t| t.label)
            .collect::<HashSet<_>>()
            .into_iter()
            .map(|label| Transition::new(state.clone(), state.clone(), label))
            .collect();
        system
    }

    pub fn transitions_len(&self) -> usize {
        self.transitions.len()
    }
}

impl<C, D, G> LabeledTransitionSystem<C, D, G>
where
    C: Clone + Hash + Ord + PartialEq,
    G: Guard<D>,
{
    pub fn full_transitions<'a>(
        &'a self,
        state: &'a State<G>,
    ) -> impl Iterator<Item = HashMap<&'a C, bool>> + Clone {
        self.transitions
            .iter()
            .filter(move |t| state == &t.from)
            .map(move |t| {
                let mut map = HashMap::with_capacity(self.clocks.len());
                for c in self.clocks.iter() {
                    map.insert(c, *t.label.map.get(c).unwrap_or(&false));
                }
                map
            })
    }
}

impl<C, D, G> From<LabeledTransitionSystem<C, D, G>>
    for petgraph::Graph<State<G>, Transition<ClockLabel<C>, G>>
where
    C: Clone,
    G: Guard<D>,
{
    fn from(system: LabeledTransitionSystem<C, D, G>) -> Self {
        let mut graph = petgraph::Graph::new();
        let nodes: HashMap<_, _> = system
            .states
            .into_iter()
            .map(|s| (s.id, graph.add_node(s)))
            .collect();
        for t in system.transitions.into_iter() {
            let Transition { from, to, .. } = &t;
            graph.add_edge(
                *nodes.get(&from.id).unwrap(),
                *nodes.get(&to.id).unwrap(),
                t,
            );
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
            std::iter::Iterator::collect(container.into_iter())
        }};
    }

    #[macro_export]
    macro_rules! tr {
        ($s:expr, $from:expr => $to:expr, {}) => {
            $s.add_transition($from, $to, ClockLabel::empty());
        };
        ($s:expr, $guard:expr, $from:expr => $to:expr, $($tts:tt)*) => {
            $s.add_transition_with_guard($from, $to, trigger!$($tts)*, $guard);
        };
        ($s:expr, $from:expr => $to:expr, $($tts:tt)*) => {
            $s.add_transition($from, $to, trigger!$($tts)*);
        };
    }
}
