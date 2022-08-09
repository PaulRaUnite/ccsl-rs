use crate::interpretation::{Lattice, ValueDomain};
use std::cmp::Ordering;
use std::fmt::{Display, Formatter};
use std::ops::{BitAnd, BitOr, Deref, Not};

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Bool {
    Neither,
    True,
    False,
    Both,
}

impl Lattice for Bool {
    fn subset(&self, rhs: &Self) -> bool {
        match (self, rhs) {
            (Bool::Neither, _) => true,
            (Bool::True, Bool::Both) => true,
            (Bool::True, _) => false,
            (Bool::False, Bool::Both) => true,
            (Bool::False, _) => false,
            (Bool::Both, Bool::Both) => true,
            (Bool::Both, _) => false,
        }
    }

    fn union_inplace(&mut self, rhs: &Self) {
        match (*self, *rhs) {
            (Bool::Neither, other) | (other, Bool::Neither) => *self = other,
            (Bool::True, Bool::False) | (Bool::False, Bool::True) => *self = Bool::Both,
            (Bool::Both, _) | (_, Bool::Both) => *self = Bool::Both,
            (Bool::True, Bool::True) | (Bool::False, Bool::False) => {}
        }
    }

    fn intersection_inplace(&mut self, rhs: &Self) {
        match (self.deref(), rhs) {
            (Bool::Neither, _) | (_, Bool::Neither) => *self = Bool::Neither,
            (Bool::True, Bool::False) | (Bool::False, Bool::True) => *self = Bool::Neither,
            (Bool::Both, other) | (other, Bool::Both) => *self = *other,
            (Bool::True, Bool::True) | (Bool::False, Bool::False) => {}
        }
    }
}

impl PartialOrd for Bool {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
            (Bool::Neither, Bool::Neither)
            | (Bool::True, Bool::True)
            | (Bool::False, Bool::False)
            | (Bool::Both, Bool::Both) => Some(Ordering::Equal),
            (Bool::Neither, _) | (_, Bool::Both) => Some(Ordering::Less),
            (Bool::Both, _) | (_, Bool::Neither) => Some(Ordering::Greater),
            (Bool::True, Bool::False) | (Bool::False, Bool::True) => None,
        }
    }
}

impl From<bool> for Bool {
    fn from(b: bool) -> Self {
        if b {
            Bool::True
        } else {
            Bool::False
        }
    }
}

impl ValueDomain for Bool {
    type C = bool;
}

// TODO: maybe should rename it into complement, because it is one
impl Not for Bool {
    type Output = Self;

    fn not(self) -> Self::Output {
        match self {
            Bool::Neither => Bool::Both,
            Bool::True => Bool::False,
            Bool::False => Bool::True,
            Bool::Both => Bool::Neither,
        }
    }
}

impl Display for Bool {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Bool::Neither => write!(f, "⊥"),
            Bool::True => write!(f, "t"),
            Bool::False => write!(f, "f"),
            Bool::Both => write!(f, "t/f"),
        }
    }
}

impl BitOr for Bool {
    type Output = Self;

    fn bitor(mut self, rhs: Self) -> Self::Output {
        self.union_inplace(&rhs);
        self
    }
}

impl BitAnd for Bool {
    type Output = Self;

    fn bitand(mut self, rhs: Self) -> Self::Output {
        self.intersection_inplace(&rhs);
        self
    }
}
