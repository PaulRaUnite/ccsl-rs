use std::ops::{Add, RangeInclusive, Sub};

trait Lattice: PartialOrd + Clone {
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

trait ValueDomain: Lattice + Add + Sub + From<Self::C> + From<RangeInclusive<Self::C>> {
    type C;
}

pub(crate) mod interval;
