use std::collections::HashSet;
use std::fmt::Debug;

use pest::iterators::Pairs;
use pest::Parser;
use thiserror::Error;

use crate::lccsl::constraints::*;

#[derive(Parser)]
#[grammar = "ccsl.pest"]
struct LightCCSLParser;

pub fn parse_raw(input: &str) -> Result<Pairs<Rule>, pest::error::Error<Rule>> {
    LightCCSLParser::parse(Rule::file, input)
}

#[derive(Debug)]
pub struct Specification<C: Eq + Debug> {
    pub clocks: HashSet<C>,
    pub constraints: Vec<Constraint<C>>,
}

#[derive(Error, Debug)]
pub enum ParseError {
    #[error("parsing error")]
    Unspecified(#[from] pest::error::Error<Rule>),
    #[error("unknown error")]
    Unknown,
}

pub enum AST {}

pub fn parse(input: &str) -> Result<Specification<String>, ParseError> {
    let rules = parse_raw(input)?;
    let mut clocks = Default::default();
    let mut constraints = vec![];
    Ok(Specification {
        clocks,
        constraints,
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        let spec = "
        Specification demo {
            Clock p,a,m,t
            [
            repeat onn every 2 p from 0
            repeat off every 2 p from 1

            Let m0 be a + off
            Let m be m0 - onn
            Precedence a < m
            SubClocking onn <- a
            Exclusion off # a
            Let out be m and t
            ]
        }";
        assert!(matches!(parse_raw(spec), Ok(_)));
    }
}
