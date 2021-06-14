use crate::lccsl::expressions::{BooleanExpression, Switch};
use bitmaps::Bitmap;
use itertools::{repeat_n, Itertools};
use num::Integer;
use roaring::RoaringBitmap;
use std::cmp::{max, Ordering};
use std::collections::{BTreeMap, BTreeSet, HashMap};
use std::fmt;
use std::fmt::{Debug, Formatter};
use std::hash::{Hash, Hasher};
use std::iter::FromIterator;
use std::ops::{BitOr, Index, Range};
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

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct ClockLabelClassic<C>(BTreeMap<C, bool>);

impl<C: Ord + Clone> Label<C> for ClockLabelClassic<C> {
    fn has_conflict(&self, rhs: &Self) -> bool {
        for (v, b) in &self.0 {
            match rhs.0.get(v) {
                Some(rb) if rb != b => return true,
                _ => {}
            }
        }
        false
    }

    fn with_capacity_hint(_: usize) -> Self {
        Self(BTreeMap::new())
    }
}

impl<'a, 'b, C: Ord + Clone> From<(&'a BTreeSet<C>, &'b BTreeSet<C>)> for ClockLabelClassic<C> {
    fn from((clocks, label): (&'a BTreeSet<C>, &'b BTreeSet<C>)) -> Self {
        Self(
            clocks
                .iter()
                .map(|c| (c, false))
                .chain(label.iter().map(|c| (c, true)))
                .map(|(c, b)| (c.clone(), b))
                .collect(),
        )
    }
}

impl<C: Ord> FromIterator<(C, bool)> for ClockLabelClassic<C> {
    fn from_iter<T: IntoIterator<Item = (C, bool)>>(iter: T) -> Self {
        ClockLabelClassic(iter.into_iter().collect())
    }
}

impl<C: Ord> Default for ClockLabelClassic<C> {
    fn default() -> Self {
        ClockLabelClassic(BTreeMap::new())
    }
}

impl<C: Clone + Ord> ClockLabelClassic<C> {
    pub fn new(clocks: &BTreeSet<C>, present: &BTreeSet<C>) -> Self {
        Self(
            clocks
                .iter()
                .map(|c| (c.clone(), false))
                .chain(present.iter().map(|c| (c.clone(), true)))
                .collect(),
        )
    }
}

impl<'a, 'b, C: Ord + Clone> BitOr<&'b ClockLabelClassic<C>> for &'a ClockLabelClassic<C> {
    type Output = ClockLabelClassic<C>;

    fn bitor(self, rhs: &'b ClockLabelClassic<C>) -> Self::Output {
        ClockLabelClassic(
            self.0
                .iter()
                .map(|(c, b)| (c.clone(), *b))
                .chain(rhs.0.iter().map(|(c, b)| (c.clone(), *b)))
                .collect(),
        )
    }
}

#[cfg(test)]
mod tests {
    use crate::lccsl::automata::{
        ClockLabelClassic, DynBitmapLabel, Label, RoaringBitmapLabel, StaticBitmapLabel,
    };

    fn invariants<L: Label<u32>>() -> impl Iterator<Item = (L, L, bool)> {
        let tests = vec![
            (
                vec![(0, true), (1, false)],
                vec![(1, true), (2, true)],
                true,
            ), // conflict intersection of clocks
            (
                vec![(0, true), (1, false)],
                vec![(1, false), (2, true)],
                false,
            ), // non conflict intersection of clocks
            (vec![(0, true), (1, false)], vec![], false), // one label is empty, no conflict
            (
                vec![(0, true), (1, false)],
                vec![(2, true), (3, true)],
                false,
            ), // no intersection of clocks, no conflict
            (
                vec![(0, true), (1, false), (2, true)],
                vec![(0, true), (1, false)],
                false,
            ), // clocks subset, conflict in the end
            (
                vec![(0, true), (1, false), (2, false)],
                vec![(1, false), (2, false)],
                false,
            ), // clocks subset, conflict in the end
        ];
        tests.into_iter().map(|(left, right, conflict)| {
            (
                left.into_iter().collect(),
                right.into_iter().collect(),
                conflict,
            )
        })
    }

    #[test]
    fn test_classic() {
        for (left, right, exp) in invariants::<ClockLabelClassic<u32>>() {
            assert_eq!(left.has_conflict(&right), exp);
        }
    }

    #[test]
    fn test_roaring_bitmap() {
        for (left, right, exp) in invariants::<RoaringBitmapLabel>() {
            assert_eq!(left.has_conflict(&right), exp);
        }
    }
    #[test]
    fn test_static_bitmap() {
        for (left, right, exp) in invariants::<StaticBitmapLabel>() {
            println!("{:?},{:?}", left, right);
            assert_eq!(left.has_conflict(&right), exp);
        }
    }
    #[test]
    fn test_dynamic_bitmap() {
        for (left, right, exp) in invariants::<DynBitmapLabel>() {
            println!("{:?},{:?}", left, right);
            assert_eq!(left.has_conflict(&right), exp);
        }
    }
}

impl<C: fmt::Display> fmt::Display for ClockLabelClassic<C> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "({})",
            self.0
                .iter()
                .filter_map(|(c, b)| if *b { Some(c) } else { None })
                .join(".")
        )
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
pub struct STS<C, L> {
    name: String,
    states: Vec<Option<Arc<Expr<C>>>>,
    states_to_trans: Vec<Range<usize>>,
    clocks: BTreeSet<C>,
    transitions: Vec<(L, Switch<Expr<C>, State<C>>)>,
    initial_state: usize,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct StateRef(usize);

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

impl<C, L> STS<C, L> {
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
    ) -> impl Iterator<Item = MergedTransition<C, L>> + Clone {
        self.transitions
            .get(self.states_to_trans[state.0].clone())
            .unwrap()
            .into_iter()
            .map(move |(label, switch)| MergedTransition {
                from: state.clone(),
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
                        t.label.clone(),
                    );
                }
            }
        }
        graph
    }
}

pub trait Label<C>:
    for<'a, 'b> From<(&'a BTreeSet<C>, &'b BTreeSet<C>)> + FromIterator<(C, bool)> + Default
where
    C: Clone + Ord,
{
    fn has_conflict(&self, rhs: &Self) -> bool;

    fn with_capacity_hint(_size: usize) -> Self {
        Self::default()
    }
}

#[derive(Debug, Clone, PartialEq, Default)]
pub struct RoaringBitmapLabel {
    clocks: RoaringBitmap,
    selection: RoaringBitmap,
}

impl Eq for RoaringBitmapLabel {}

impl FromIterator<(u32, bool)> for RoaringBitmapLabel {
    fn from_iter<T: IntoIterator<Item = (u32, bool)>>(iter: T) -> Self {
        let mut clocks = RoaringBitmap::new();
        let mut selection = RoaringBitmap::new();
        for (i, b) in iter {
            clocks.push(i);
            if b {
                selection.push(i);
            }
        }
        RoaringBitmapLabel { clocks, selection }
    }
}

impl Label<u32> for RoaringBitmapLabel {
    fn has_conflict(&self, rhs: &Self) -> bool {
        !((&self.clocks & &rhs.clocks) & (&self.selection ^ &rhs.selection)).is_empty()
    }
}

impl<'a, 'b> BitOr<&'b RoaringBitmapLabel> for &'a RoaringBitmapLabel {
    type Output = RoaringBitmapLabel;

    fn bitor(self, rhs: &'b RoaringBitmapLabel) -> Self::Output {
        RoaringBitmapLabel {
            clocks: &self.clocks | &rhs.clocks,
            selection: &self.selection | &self.selection,
        }
    }
}

impl Hash for RoaringBitmapLabel {
    fn hash<H: Hasher>(&self, state: &mut H) {
        for x in self.clocks.iter().chain(self.selection.iter()) {
            state.write_u32(x);
        }
    }
}

impl<'a, 'b> From<(&'a BTreeSet<u32>, &'b BTreeSet<u32>)> for RoaringBitmapLabel {
    fn from((clocks, label): (&'a BTreeSet<u32>, &'b BTreeSet<u32>)) -> Self {
        Self {
            clocks: clocks.iter().map(|v| *v).collect(),
            selection: label.iter().map(|v| *v).collect(),
        }
    }
}

#[derive(Clone, Eq, PartialEq, Default, Hash)]
pub struct StaticBitmapLabel {
    clocks: Bitmap<64>,
    selection: Bitmap<64>,
}

impl Debug for StaticBitmapLabel {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "SB {:#010b} < {:#010b}",
            self.clocks.as_value(),
            self.selection.as_value()
        )
    }
}

impl Label<u32> for StaticBitmapLabel {
    fn has_conflict(&self, rhs: &Self) -> bool {
        !((self.clocks & rhs.clocks) & (self.selection ^ rhs.selection)).is_empty()
    }
}
impl FromIterator<(u32, bool)> for StaticBitmapLabel {
    fn from_iter<T: IntoIterator<Item = (u32, bool)>>(iter: T) -> Self {
        let mut clocks = Bitmap::new();
        let mut selection = Bitmap::new();
        for (i, b) in iter {
            clocks.set(i as usize, true);
            if b {
                selection.set(i as usize, true);
            }
        }
        StaticBitmapLabel { clocks, selection }
    }
}

impl<'a, 'b> From<(&'a BTreeSet<u32>, &'b BTreeSet<u32>)> for StaticBitmapLabel {
    fn from((clocks, label): (&'a BTreeSet<u32>, &'b BTreeSet<u32>)) -> Self {
        let mut c = Bitmap::new();
        let mut s = Bitmap::new();
        for i in clocks {
            c.set(*i as usize, true);
        }
        for i in label {
            s.set(*i as usize, true);
        }
        StaticBitmapLabel {
            clocks: c,
            selection: s,
        }
    }
}

impl<'a, 'b> BitOr<&'b StaticBitmapLabel> for &'a StaticBitmapLabel {
    type Output = StaticBitmapLabel;

    fn bitor(self, rhs: &'b StaticBitmapLabel) -> Self::Output {
        StaticBitmapLabel {
            clocks: self.clocks | rhs.clocks,
            selection: self.selection | rhs.selection,
        }
    }
}

#[derive(Clone, Eq, PartialEq, Default, Hash)]
pub struct DynBitmapLabel {
    clocks: Vec<u8>,
    selection: Vec<u8>,
}

impl Debug for DynBitmapLabel {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        for x in &self.clocks {
            writeln!(f, "{:#010b}", x)?;
        }
        writeln!(f, ", ")?;
        for x in &self.selection {
            writeln!(f, "{:#010b}", x)?;
        }
        Ok(())
    }
}

impl Label<u32> for DynBitmapLabel {
    fn has_conflict(&self, rhs: &Self) -> bool {
        let left = self.clocks.iter().zip(self.selection.iter());
        let right = rhs.clocks.iter().zip(rhs.selection.iter());
        let (long, short, diff) = if self.clocks.len() > rhs.clocks.len() {
            (left, right, self.clocks.len() - rhs.clocks.len())
        } else {
            (right, left, rhs.clocks.len() - self.clocks.len())
        };
        long.zip(short.chain(repeat_n((&0u8, &0u8), diff)))
            .any(|((lc, ls), (rc, rs))| ((lc & rc) & (ls ^ rs)) != 0)
    }
}

impl<'a, 'b> BitOr<&'b DynBitmapLabel> for &'a DynBitmapLabel {
    type Output = DynBitmapLabel;

    fn bitor(self, rhs: &'b DynBitmapLabel) -> Self::Output {
        let size = max(self.clocks.len(), rhs.clocks.len());
        let mut clocks: Vec<u8> = Vec::with_capacity(size);
        let mut selection: Vec<u8> = Vec::with_capacity(size);
        clocks.extend(
            self.clocks
                .iter()
                .chain(repeat_n(&0u8, size - self.clocks.len())),
        );
        selection.extend(
            self.selection
                .iter()
                .chain(repeat_n(&0u8, size - self.selection.len())),
        );

        for (c, r) in clocks.iter_mut().zip(rhs.clocks.iter()) {
            *c |= r;
        }
        for (c, r) in selection.iter_mut().zip(rhs.selection.iter()) {
            *c |= r;
        }

        DynBitmapLabel { clocks, selection }
    }
}

impl<'a, 'b> From<(&'a BTreeSet<u32>, &'b BTreeSet<u32>)> for DynBitmapLabel {
    fn from((left, right): (&'a BTreeSet<u32>, &'b BTreeSet<u32>)) -> Self {
        let (mut size, rem) = (max(
            left.iter().max().unwrap_or(&0),
            right.iter().max().unwrap_or(&0),
        ) + 1)
            .div_rem(&8);
        if rem != 0 {
            size += 1;
        }
        let size = size as usize;
        let mut clocks = vec![0u8; size];
        let mut selection = vec![0u8; size];
        for i in left {
            let (index, offset) = i.div_rem(&8u32);
            clocks[index as usize] |= (1 << offset) as u8;
        }
        for i in right {
            let (index, offset) = i.div_rem(&8u32);
            selection[index as usize] |= (1 << offset) as u8;
        }
        Self { clocks, selection }
    }
}

impl FromIterator<(u32, bool)> for DynBitmapLabel {
    fn from_iter<T: IntoIterator<Item = (u32, bool)>>(iter: T) -> Self {
        let mut clocks = BTreeSet::new();
        let mut selection = BTreeSet::new();
        for (c, b) in iter {
            clocks.insert(c);
            if b {
                selection.insert(c);
            }
        }
        (&clocks, &selection).into()
    }
}
