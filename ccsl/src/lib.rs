#[macro_use]
extern crate pest_derive;
#[macro_use]
extern crate lazy_static;

pub mod generation;
pub mod kernel;
pub mod lccsl;

// TODO: rename the module and functions in it
pub mod algo;

pub mod visualisation;

pub mod conflict_graph;
pub mod optimization;
pub mod symbolic;

pub mod dot;
