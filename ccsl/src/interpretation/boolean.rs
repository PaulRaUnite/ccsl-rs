use crate::interpretation::{Lattice, ValueDomain};
use std::cmp::Ordering;
use std::ops::Deref;

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
        match (self.deref(), rhs) {
            (Bool::Neither, other) | (other, Bool::Neither) => *self = *other,
            (Bool::True, Bool::False) | (Bool::False, Bool::True) => *self = Bool::Both,
            (Bool::Both, _) | (_, Bool::Both) => *self = Bool::Both,
        }
    }

    fn intersection_inplace(&mut self, rhs: &Self) {
        match (self.deref(), rhs) {
            (Bool::Neither, _) | (_, Bool::Neither) => *self = Bool::Neither,
            (Bool::True, Bool::False) | (Bool::False, Bool::True) => *self = Bool::Neither,
            (Bool::Both, other) | (other, Bool::Both) => *self = *other,
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
            (Bool::True, Bool::False) => None,
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
