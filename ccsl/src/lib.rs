#[macro_use]
extern crate pest_derive;
#[macro_use]
extern crate lazy_static;
extern crate core;

pub mod lccsl {
    pub mod parser;

    pub mod runner;

    // TODO: rename the module and functions in it
    pub mod algo;

    pub mod vizualization;

    pub mod optimization;

    pub mod format;

    pub mod analysis;

    pub mod dot;
}

pub mod interpretation;
