#[macro_use]
extern crate pest_derive;
#[macro_use]
extern crate lazy_static;

pub mod generation;
pub mod kernel;
pub mod lccsl;

pub mod sort {}
pub mod complexity {}

// TODO: rename the module and functions in it
pub mod algo;

pub mod visualisation;

pub mod analysis;
pub mod optimization;

pub mod dot;
