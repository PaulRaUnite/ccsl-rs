use std::ops::{Add, RangeInclusive, Sub};

pub trait Lattice: PartialOrd + Clone {
    fn subset(&self, rhs: &Self) -> bool;
    fn union(&self, rhs: &Self) -> Self {
        let mut result = self.clone();
        result.union_inplace(rhs);
        result
    }
    fn union_inplace(&mut self, rhs: &Self);
    fn intersection(&self, rhs: &Self) -> Self {
        let mut result = self.clone();
        result.intersection_inplace(rhs);
        result
    }
    fn intersection_inplace(&mut self, rhs: &Self);
}

pub trait ValueDomain: Lattice + Add + Sub + From<Self::C> + From<RangeInclusive<Self::C>> {
    type C;
}

pub(crate) mod interval;

pub trait Widening {
    type Domain;
    fn widen(prev: &Self::Domain, next: &Self::Domain) -> Self::Domain;
}

pub trait Narrowing {
    type Domain;
    fn narrow(prev: &Self::Domain, next: &Self::Domain) -> Self::Domain;
}
