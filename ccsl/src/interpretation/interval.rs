use std::cmp::Ordering;
use std::ops::{Add, Deref, RangeInclusive, Sub};

use crate::interpretation::{Lattice, ValueDomain};

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
enum LeftBound<T> {
    Infinity,
    Bound(T),
}
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
enum RightBound<T> {
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
enum Interval<T> {
    #[default]
    None,
    Bound(LeftBound<T>, RightBound<T>),
}

impl<T> PartialOrd for Interval<T>
where
    T: PartialOrd,
{
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
            (Interval::None, Interval::None) => Some(Ordering::Equal),
            (Interval::None, _) => Some(Ordering::Less),
            (_, Interval::None) => Some(Ordering::Greater),
            (Interval::Bound(a, b), Interval::Bound(c, d)) => {
                let inside = a <= c && b <= d;
                let outside = a >= c && b >= d;
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
            .map(|c| c == Ordering::Less)
            .unwrap_or(false)
            || self == rhs
    }

    fn union_inplace(&mut self, rhs: &Self) {
        match (self.deref(), rhs) {
            (_, Interval::None) => {}
            (Interval::Bound(a, b), Interval::Bound(c, d)) => {
                *self = Interval::Bound(a.min(c).clone(), b.min(d).clone())
            }
            (Interval::None, _) => *self = rhs.clone(),
        }
    }

    fn intersection_inplace(&mut self, rhs: &Self) {
        match (self.deref(), rhs) {
            (Interval::None, _) | (_, Interval::None) => *self = Interval::None,
            (Interval::Bound(a, b), Interval::Bound(c, d)) => {
                let x = a.max(c);
                let y = b.min(d);
                *self = if x <= y {
                    Interval::Bound(x.clone(), y.clone())
                } else {
                    Interval::None
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
            (None, _) | (_, None) => None,
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
            (None, _) | (_, None) => None,
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

impl<T> ValueDomain for Interval<T>
where
    T: Clone + Add<Output = T> + Sub<Output = T> + Ord,
{
    type C = T;
}
