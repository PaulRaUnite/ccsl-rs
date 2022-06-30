use bitmaps::Bitmap;
use itertools::{repeat_n, Itertools};
use num::Integer;
use roaring::RoaringBitmap;
use std::cmp::max;
use std::collections::{BTreeMap, BTreeSet};
use std::fmt;
use std::fmt::{Debug, Formatter};
use std::hash::{Hash, Hasher};
use std::ops::BitOr;

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
            selection: &self.selection | &rhs.selection,
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
            clocks: clocks.iter().copied().collect(),
            selection: label.iter().copied().collect(),
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
            "({})",
            (self.clocks & self.selection).into_iter().join(".")
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

#[cfg(test)]
mod tests {
    use super::*;

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
            assert_eq!(left.has_conflict(&right), exp);
        }
    }
    #[test]
    fn test_dynamic_bitmap() {
        for (left, right, exp) in invariants::<DynBitmapLabel>() {
            assert_eq!(left.has_conflict(&right), exp);
        }
    }
}
