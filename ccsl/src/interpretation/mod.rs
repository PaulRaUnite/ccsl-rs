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

pub trait ValueDomain: Lattice + From<Self::C> {
    type C;
}

pub mod boolean;
pub mod interval;

pub trait Widening {
    type Domain;
    fn widen(&mut self, prev: &Self::Domain, next: &Self::Domain) -> Self::Domain;
}

pub trait Narrowing {
    type Domain;
    fn narrow(&mut self, prev: &Self::Domain, next: &Self::Domain) -> Self::Domain;
}

pub trait Succ {
    fn succ(&self) -> Self;
}

pub trait Prec {
    fn prec(&self) -> Self;
}
impl Succ for i64 {
    fn succ(&self) -> Self {
        *self + 1
    }
}

impl Prec for i64 {
    fn prec(&self) -> Self {
        *self - 1
    }
}
