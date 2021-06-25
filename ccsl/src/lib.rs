extern crate pest;
#[macro_use]
extern crate pest_derive;
extern crate derive_more;
extern crate itertools;
extern crate nalgebra as na;
extern crate num;
extern crate permutation;
extern crate rand;
extern crate thiserror;

pub mod lccsl {
    pub mod constraints;

    pub mod automata;

    pub mod parser;

    pub mod runner;

    pub mod expressions;

    pub mod algo;

    pub mod vizualization;

    pub mod gen;

    pub mod opti;

    pub mod format;
}
