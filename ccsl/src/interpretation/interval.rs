use std::cmp::Ordering;
use std::ops::{Add, Deref, RangeFrom, RangeInclusive, RangeToInclusive, Sub};

use crate::interpretation::{Lattice, ValueDomain};

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum LeftBound<T> {
    Infinity,
    Bound(T),
}
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum RightBound<T> {
    Infinity,
    Bound(T),
}

impl<T: Add<Output = T>> Add for LeftBound<T> {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        use LeftBound::*;
        match (self, rhs) {
            (Infinity, _) | (_, Infinity) => Infinity,
            (Bound(a), Bound(b)) => Bound(a + b),
        }
    }
}

impl<T: Add<Output = T>> Add for RightBound<T> {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        use RightBound::*;
        match (self, rhs) {
            (Infinity, _) | (_, Infinity) => Infinity,
            (Bound(a), Bound(b)) => Bound(a + b),
        }
    }
}
impl<T: Sub<Output = T>> Sub for LeftBound<T> {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        use LeftBound::*;
        match (self, rhs) {
            (Infinity, _) | (_, Infinity) => Infinity,
            (Bound(a), Bound(b)) => Bound(a - b),
        }
    }
}

impl<T: Sub<Output = T>> Sub for RightBound<T> {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        use RightBound::*;
        match (self, rhs) {
            (Infinity, _) | (_, Infinity) => Infinity,
            (Bound(a), Bound(b)) => Bound(a - b),
        }
    }
}

impl<T: PartialOrd> PartialOrd for LeftBound<T> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        use LeftBound::*;
        match (self, other) {
            (Infinity, Infinity) => Some(Ordering::Equal),
            (Infinity, Bound(_)) => Some(Ordering::Less),
            (Bound(_), Infinity) => Some(Ordering::Greater),
            (Bound(a), Bound(b)) => a.partial_cmp(b),
        }
    }
}

impl<T: Ord> Ord for LeftBound<T> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.partial_cmp(other).unwrap()
    }
}
impl<T: Ord> Ord for RightBound<T> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.partial_cmp(other).unwrap()
    }
}

impl<T: PartialEq> PartialEq<RightBound<T>> for LeftBound<T> {
    fn eq(&self, other: &RightBound<T>) -> bool {
        match (self, other) {
            (LeftBound::Infinity, _) | (_, RightBound::Infinity) => false,
            (LeftBound::Bound(a), RightBound::Bound(b)) => a == b,
        }
    }
}
impl<T: PartialOrd> PartialOrd<RightBound<T>> for LeftBound<T> {
    fn partial_cmp(&self, other: &RightBound<T>) -> Option<Ordering> {
        match (self, other) {
            (LeftBound::Infinity, _) | (LeftBound::Bound(_), RightBound::Infinity) => {
                Some(Ordering::Less)
            }
            (LeftBound::Bound(a), RightBound::Bound(b)) => a.partial_cmp(b),
        }
    }
}

impl<T: PartialOrd> PartialOrd for RightBound<T> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        use RightBound::*;
        match (self, other) {
            (Infinity, Infinity) => Some(Ordering::Equal),
            (Infinity, Bound(_)) => Some(Ordering::Greater),
            (Bound(_), Infinity) => Some(Ordering::Less),
            (Bound(a), Bound(b)) => a.partial_cmp(b),
        }
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Default)]
pub enum Interval<T> {
    #[default]
    Bottom,
    Bound(LeftBound<T>, RightBound<T>),
}

impl<T> PartialOrd for Interval<T>
where
    T: PartialOrd,
{
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
            (Interval::Bottom, Interval::Bottom) => Some(Ordering::Equal),
            (Interval::Bottom, _) => Some(Ordering::Less),
            (_, Interval::Bottom) => Some(Ordering::Greater),
            (Interval::Bound(a, b), Interval::Bound(c, d)) => {
                let inside = a >= c && b <= d;
                let outside = a <= c && b >= d;
                match (inside, outside) {
                    (true, true) => Some(Ordering::Equal),
                    (true, false) => Some(Ordering::Less),
                    (false, true) => Some(Ordering::Greater),
                    (false, false) => None,
                }
            }
        }
    }
}

impl<T> Lattice for Interval<T>
where
    T: Clone + Ord,
{
    fn subset(&self, rhs: &Self) -> bool {
        self.partial_cmp(rhs)
            .map(|c| c == Ordering::Less || c == Ordering::Equal)
            .unwrap_or(false)
    }

    fn union_inplace(&mut self, rhs: &Self) {
        match (self.deref(), rhs) {
            (_, Interval::Bottom) => {}
            (Interval::Bound(a, b), Interval::Bound(c, d)) => {
                *self = Interval::Bound(a.min(c).clone(), b.max(d).clone())
            }
            (Interval::Bottom, _) => *self = rhs.clone(),
        }
    }

    fn intersection_inplace(&mut self, rhs: &Self) {
        match (self.deref(), rhs) {
            (Interval::Bottom, _) | (_, Interval::Bottom) => *self = Interval::Bottom,
            (Interval::Bound(a, b), Interval::Bound(c, d)) => {
                let x = a.max(c);
                let y = b.min(d);
                *self = if x <= y {
                    Interval::Bound(x.clone(), y.clone())
                } else {
                    Interval::Bottom
                };
            }
        }
    }
}

impl<T> Add for Interval<T>
where
    T: Add<Output = T>,
{
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        use Interval::*;
        match (self, rhs) {
            (Bottom, _) | (_, Bottom) => Bottom,
            (Bound(a, b), Bound(c, d)) => Bound(a + c, b + d),
        }
    }
}
impl<T> Sub for Interval<T>
where
    T: Sub<Output = T>,
{
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        use Interval::*;
        match (self, rhs) {
            (Bottom, _) | (_, Bottom) => Bottom,
            (Bound(a, b), Bound(c, d)) => Bound(a - c, b - d),
        }
    }
}

impl<T: Clone> From<T> for Interval<T> {
    fn from(v: T) -> Self {
        Interval::Bound(LeftBound::Bound(v.clone()), RightBound::Bound(v))
    }
}

impl<T: Clone> From<RangeInclusive<T>> for Interval<T> {
    fn from(range: RangeInclusive<T>) -> Self {
        Interval::Bound(
            LeftBound::Bound(range.start().clone()),
            RightBound::Bound(range.end().clone()),
        )
    }
}
impl<T: Clone> From<RangeToInclusive<T>> for Interval<T> {
    fn from(range: RangeToInclusive<T>) -> Self {
        Interval::Bound(LeftBound::Infinity, RightBound::Bound(range.end))
    }
}
impl<T: Clone> From<RangeFrom<T>> for Interval<T> {
    fn from(range: RangeFrom<T>) -> Self {
        Interval::Bound(LeftBound::Bound(range.start), RightBound::Infinity)
    }
}

// TODO: need specialization
// impl<T> From<RangeFull> for Interval<T> {
//     fn from(range: RangeFull) -> Self {
//         Interval::Bound(LeftBound::Infinity, RightBound::Infinity)
//     }
// }

impl<T> Interval<T> {
    const fn top() -> Self {
        Interval::Bound(LeftBound::Infinity, RightBound::Infinity)
    }

    const fn bottom() -> Self {
        Interval::Bottom
    }
}
impl<T> ValueDomain for Interval<T>
where
    T: Clone + Add<Output = T> + Sub<Output = T> + Ord,
{
    type C = T;
}

pub type IntegerInterval = Interval<i64>;

#[cfg(test)]
mod test {
    use crate::interpretation::Lattice;
    use std::cmp::Ordering;

    use super::IntegerInterval;

    #[test]
    fn test_add_interval() {
        let a: IntegerInterval = (..=3).into();
        let b: IntegerInterval = (2..=6).into();
        let c: IntegerInterval = (5..).into();
        let d: IntegerInterval = IntegerInterval::top();

        assert_eq!(a + b, (..=9).into());
        assert_eq!(b + c, (7..).into());
        assert_eq!(c + d, IntegerInterval::top());
        assert_eq!(a + d, IntegerInterval::top());
    }
    #[test]
    fn test_sub_interval() {
        let a: IntegerInterval = (..=3).into();
        let b: IntegerInterval = (2..=6).into();
        let c: IntegerInterval = (5..).into();
        let d: IntegerInterval = IntegerInterval::top();

        assert_eq!(a - b, (..=-3).into());
        assert_eq!(b - c, (-3..).into());
        assert_eq!(c - d, IntegerInterval::top());
        assert_eq!(a - d, IntegerInterval::top());
    }

    #[test]
    fn test_subset() {
        let i11: IntegerInterval = 1.into();
        let i24: IntegerInterval = (2..=4).into();
        let i12: IntegerInterval = (1..=2).into();
        let b: IntegerInterval = IntegerInterval::bottom();
        let t: IntegerInterval = IntegerInterval::top();

        assert_eq!(b.partial_cmp(&t), Some(Ordering::Less));
        assert_eq!(b.partial_cmp(&i11), Some(Ordering::Less));
        assert_eq!(b.partial_cmp(&i24), Some(Ordering::Less));
        assert_eq!(b.partial_cmp(&i12), Some(Ordering::Less));
        assert_eq!(b.partial_cmp(&b), Some(Ordering::Equal));

        assert_eq!(t.partial_cmp(&i11), Some(Ordering::Greater));
        assert_eq!(t.partial_cmp(&i24), Some(Ordering::Greater));
        assert_eq!(t.partial_cmp(&i12), Some(Ordering::Greater));
        assert_eq!(t.partial_cmp(&t), Some(Ordering::Equal));

        assert_eq!(i11.partial_cmp(&i24), None);
        assert_eq!(i11.partial_cmp(&i12), Some(Ordering::Less));

        assert_eq!(i24.partial_cmp(&i12), None);
        assert_eq!(i24.partial_cmp(&i24), Some(Ordering::Equal));
    }

    #[test]
    fn test_intersection() {
        let a: IntegerInterval = (2..=5).into();
        let b: IntegerInterval = (..=3).into();
        let c: IntegerInterval = (4..).into();

        assert_eq!(a.intersection(&b), (2..=3).into());
        assert_eq!(a.intersection(&c), (4..=5).into());
        assert_eq!(b.intersection(&c), IntegerInterval::bottom());
    }
    #[test]
    fn test_union() {
        let a: IntegerInterval = (2..=5).into();
        let b: IntegerInterval = (..=3).into();
        let c: IntegerInterval = (4..).into();

        assert_eq!(a.union(&b), (..=5).into());
        assert_eq!(a.union(&c), (2..).into());
        assert_eq!(b.union(&c), IntegerInterval::top());
    }
}
