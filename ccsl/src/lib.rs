extern crate pest;
#[macro_use]
extern crate pest_derive;
extern crate derive_more;
extern crate itertools;
extern crate num;
extern crate thiserror;

#[cfg(test)]
mod tests {}

// TODO:
//   [-] finish automata structure
//   [-] port automata generation
//   [] port automata runtime combination algorithm
//   [-] write visualization:
//      [] search space with dependency tracking
//      [] search space with actions taken
//      [] constraint complexity approximation graph

mod core {
    trait Constraint<const N: usize> {
        fn test(&self, events: &[u8; N]) -> bool;
        fn apply(&mut self, events: &mut [u8; N], state: &u32);
    }

    pub struct System<const I: usize, const N: usize, const D: usize> {
        states: [u32; N],
        differences: [i32; D],
        constraints: [&'static dyn Constraint<{ N }>],
    }

    impl<const I: usize, const N: usize, const D: usize> System<I, N, D> {
        pub fn make_decision(&mut self, input_events: &[u8; I]) -> [u8; N] {
            let mut events: [u8; N] = [0; N];

            return events;
        }
    }
}

pub mod lccsl {
    pub mod constraints;

    pub mod automata;

    pub mod parser;

    pub mod runner;

    pub mod expressions;

    pub mod algo;

    pub mod vizualization;
}
