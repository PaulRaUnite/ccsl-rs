use std::collections::{BTreeSet, HashMap, HashSet};
use std::fmt::Debug;

use derive_more::From;
use pest::iterators::{Pair, Pairs};
use pest::pratt_parser::{Assoc, Op, PrattParser};
use pest::Parser;
use thiserror::Error;

use itertools::Itertools;
use kernel::constraints::*;
use rand::distributions::Alphanumeric;
use rand::{thread_rng, Rng};
use std::cell::RefCell;
use std::iter::FromIterator;
use std::ops::RangeFrom;

#[derive(Parser)]
#[grammar = "ccsl.pest"]
struct LightCCSLParser;

pub fn parse_raw(input: &str) -> Result<Pair<Rule>, pest::error::Error<Rule>> {
    Ok(LightCCSLParser::parse(Rule::file, input)?.next().unwrap())
}

#[derive(Debug, Clone)]
pub struct Specification<C> {
    pub name: String,
    pub clocks: HashSet<C>,
    pub constraints: Vec<Constraint<C>>,
}

#[derive(Error, Debug)]
pub enum ParseError {
    #[error("parsing error")]
    Unspecified(#[from] pest::error::Error<Rule>),
    #[error("undefined clocks")]
    UndefinedClocks,
    #[error("unknown error")]
    Unknown,
}
// TODO: parse into a specialized AST/enum, not directly into constrains,
//  for reasons of encoding/decoding roundtrip: some constraints
//  are split and so do not preserve the initial structure.

fn parse(input: &str) -> Result<Specification<ID>, ParseError> {
    let file = parse_raw(input)?;
    let mut name = "".to_owned();
    let mut clocks = HashSet::new();
    let mut constraints: Vec<Constraint<ID>> = vec![];

    let clock_gen = RefCell::new(0usize..);

    for record in file.into_inner() {
        match record.as_rule() {
            Rule::specification => {
                for field in record.into_inner() {
                    match field.as_rule() {
                        Rule::id => name = field.as_str().to_string(),
                        Rule::clocks => clocks.extend(parse_clocks(field)),
                        Rule::repeat => constraints.push(parse_repeat(field)),
                        Rule::causal_chain => constraints.extend(parse_causality(field)),
                        Rule::subclock_chain => constraints.extend(parse_subclocking(field)),
                        Rule::exclusion_chain => constraints.extend(parse_exclusion(field)),
                        Rule::let_expr => constraints.extend(parse_let_expr(field, &clock_gen)),
                        Rule::periodic_def => constraints.push(parse_periodic_def(field)),
                        _ => unreachable!(),
                    };
                }
            }
            Rule::EOI => (),
            _ => unreachable!(),
        }
    }

    Ok(Specification {
        name,
        clocks,
        constraints,
    })
}

pub fn parse_to_u32(input: &str) -> Result<Specification<u32>, ParseError> {
    let spec = parse(input)?;
    let unique_clocks: HashMap<_, _> = spec
        .constraints
        .iter()
        .flat_map(|c| c.clocks().into_iter())
        .unique()
        .enumerate()
        .map(|(i, c)| (c, i as u32))
        .collect();
    Ok(Specification {
        name: spec.name,
        clocks: spec
            .clocks
            .into_iter()
            .map(|c| *unique_clocks.get(&c).unwrap())
            .collect(),
        constraints: spec
            .constraints
            .iter()
            .map(|c| c.map(|c| *unique_clocks.get(&c).unwrap()))
            .collect(),
    })
}

pub fn parse_to_string(input: &str) -> Result<Specification<String>, ParseError> {
    let spec = parse(input)?;
    let unique_clocks: HashSet<_> = spec
        .constraints
        .iter()
        .flat_map(|c| {
            c.clocks().into_iter().filter_map(|c| match c {
                ID::C(s) => Some(s),
                ID::G(_) => None,
            })
        })
        .collect();
    // FIXME: random as prefix for generated clocks is cringe
    let prefix: String = thread_rng()
        .sample_iter(&Alphanumeric)
        .take(4)
        .map(char::from)
        .collect();
    let prefix = format!("g{}_", prefix);
    if unique_clocks.contains(&prefix) {
        panic!("couldn't generate prefix for numeric clocks")
    }

    Ok(Specification {
        name: spec.name,
        clocks: spec
            .clocks
            .into_iter()
            .map(|c| match c {
                ID::C(s) => s,
                ID::G(n) => format!("{}{}", prefix, n),
            })
            .collect(),
        constraints: spec
            .constraints
            .iter()
            .map(|c| {
                c.map(|c| match c {
                    ID::C(s) => s.clone(),
                    ID::G(n) => format!("{}{}", prefix, n),
                })
            })
            .collect(),
    })
}

fn parse_clocks<'a>(input: Pair<'a, Rule>) -> impl Iterator<Item = ID> + 'a {
    input
        .into_inner()
        .map(|clock| clock.as_str().to_string().into())
}

fn parse_repeat(input: Pair<Rule>) -> Constraint<ID> {
    let mut every = 1;
    let mut clocks: Vec<ID> = Vec::with_capacity(2);
    let mut from = None;
    let mut up_to = None;

    for field in input.into_inner() {
        match field.as_rule() {
            Rule::id => clocks.push(field.as_str().to_string().into()),
            Rule::int => every = field.as_str().parse::<usize>().unwrap_or(1),
            Rule::from => from = Some(field.into_inner().as_str().parse::<usize>().unwrap()),
            Rule::up_to => up_to = Some(field.into_inner().as_str().parse::<usize>().unwrap()),
            _ => unreachable!(),
        }
    }

    let out = clocks.remove(0);
    let base = clocks.remove(0);
    Repeat {
        out,
        every,
        base,
        from,
        up_to,
    }
    .into()
}
fn parse_causality(input: Pair<Rule>) -> impl Iterator<Item = Constraint<ID>> {
    let mut clocks: Vec<ID> = Vec::with_capacity(2);
    let mut kinds = Vec::with_capacity(2);
    let mut params = HashMap::new();
    for field in input.into_inner() {
        match field.as_rule() {
            Rule::id => clocks.push(field.as_str().to_string().into()),
            Rule::causality_kind => kinds.push(field.as_str() == "<="),
            Rule::causal_params => {
                let mut par = field.into_inner();
                let init = par.next().unwrap().as_str().parse::<usize>().unwrap();
                let max = par.next().unwrap().as_str().parse::<usize>().unwrap();
                params.insert(clocks.len(), (init, max));
            }
            _ => unreachable!(),
        }
    }
    let mut first = clocks.remove(0);
    let mut constraints: Vec<Constraint<ID>> = Vec::with_capacity(clocks.len());
    for (i, (clock, causality)) in clocks.into_iter().zip(kinds).enumerate() {
        let (init, max) = params
            .get(&i)
            .map_or((None, None), |(init, max)| (Some(*init), Some(*max)));
        constraints.push(if causality {
            Causality {
                left: first,
                right: clock.clone(),
                init,
                max,
            }
            .into()
        } else {
            Precedence {
                left: first,
                right: clock.clone(),
                init,
                max,
            }
            .into()
        });
        first = clock;
    }
    constraints.into_iter()
}
fn parse_subclocking(input: Pair<Rule>) -> impl Iterator<Item = Constraint<ID>> {
    let mut clocks = parse_clock_arguments(input);
    let mut first = clocks.remove(0);
    let mut constraints: Vec<Constraint<ID>> = Vec::with_capacity(clocks.len());
    for clock in clocks {
        constraints.push(
            Subclocking {
                left: first,
                right: clock.clone(),
            }
            .into(),
        );
        first = clock;
    }
    constraints.into_iter()
}

fn parse_clock_arguments(input: Pair<Rule>) -> Vec<ID> {
    let mut clocks: Vec<ID> = Vec::with_capacity(2);
    for field in input.into_inner() {
        match field.as_rule() {
            Rule::id => clocks.push(field.as_str().to_string().into()),
            _ => unreachable!(),
        }
    }
    clocks
}

fn parse_exclusion(input: Pair<Rule>) -> impl Iterator<Item = Constraint<ID>> {
    let mut clocks: Vec<ID> = parse_clock_arguments(input);
    let mut first = clocks.remove(0);
    let mut constraints: Vec<Constraint<ID>> = Vec::with_capacity(clocks.len());
    for clock in clocks {
        let mut set = BTreeSet::new();
        set.insert(first);
        set.insert(clock.clone());
        constraints.push(Exclusion { clocks: set }.into());
        first = clock;
    }
    constraints.into_iter()
}

#[derive(Debug, Clone, Hash, Ord, Eq, PartialOrd, PartialEq, From)]
enum ID {
    C(String),
    G(usize),
}

fn parse_let_expr(
    input: Pair<Rule>,
    gen: &RefCell<RangeFrom<usize>>,
) -> impl Iterator<Item = Constraint<ID>> {
    let mut inner = input.into_inner();
    let mut constraints = Vec::new();
    let out: ID = inner.next().unwrap().as_str().to_string().into();
    let expr_out = parse_expression(inner, gen, &mut constraints);
    let last = constraints.last_mut().unwrap();
    *last = last.map(|c| {
        if c == &expr_out {
            out.clone()
        } else {
            c.clone()
        }
    });
    constraints.into_iter()
}

lazy_static! {
    static ref PREC_CLIMBER: PrattParser<Rule> = {
        use Rule::*;
        PrattParser::new()
            .op(Op::infix(union, Assoc::Left))
            .op(Op::infix(intersection, Assoc::Left))
            .op(Op::infix(minus, Assoc::Left))
    };
}

fn parse_expression(
    input: Pairs<Rule>,
    gen: &RefCell<RangeFrom<usize>>,
    container: &mut Vec<Constraint<ID>>,
) -> ID {
    let mut primaries: Vec<Constraint<ID>> = Vec::new();
    let out = PREC_CLIMBER
        .map_primary(|pair: Pair<Rule>| match pair.as_rule() {
            Rule::id => pair.as_str().to_string().into(),
            Rule::prefix_expr => parse_prefix_expression(pair, gen, &mut primaries),
            Rule::expression => parse_expression(pair.into_inner(), gen, &mut primaries),
            _ => unreachable!(),
        })
        .map_infix(|lhs: ID, op: Pair<Rule>, rhs: ID| {
            let out: ID = gen.borrow_mut().next().unwrap().into();
            let args = BTreeSet::from_iter([lhs.clone(), rhs.clone()]);
            let c: Constraint<ID> = match op.as_rule() {
                Rule::union => Union {
                    out: out.clone(),
                    args,
                }
                .into(),
                Rule::intersection => Intersection {
                    out: out.clone(),
                    args,
                }
                .into(),
                Rule::minus => Minus {
                    out: out.clone(),
                    left: lhs,
                    right: rhs,
                }
                .into(),
                _ => unreachable!(),
            };
            container.push(c);
            out
        })
        .parse(input);
    container.extend_from_slice(&primaries);
    out
}

fn parse_prefix_expression(
    input: Pair<Rule>,
    gen: &RefCell<RangeFrom<usize>>,
    container: &mut Vec<Constraint<ID>>,
) -> ID {
    let mut inner = input.into_inner();
    let sup = inner.next().unwrap().as_str() == "sup";
    let args = inner.map(|c| c.as_str().to_string().into()).collect_vec();
    if sup {
        args.into_iter()
            .reduce(|left, right| {
                let out: ID = gen.borrow_mut().next().unwrap().into();
                container.push(
                    Supremum {
                        out: out.clone(),
                        left,
                        right,
                    }
                    .into(),
                );
                out
            })
            .unwrap()
    } else {
        args.into_iter()
            .reduce(|left, right| {
                let out: ID = gen.borrow_mut().next().unwrap().into();
                container.push(
                    Infinity {
                        out: out.clone(),
                        left,
                        right,
                    }
                    .into(),
                );
                out
            })
            .unwrap()
    }
}

fn parse_periodic_def(input: Pair<Rule>) -> Constraint<ID> {
    let mut inner = input.into_inner();
    let out: ID = inner.next().unwrap().as_str().to_string().into();
    let first: ID = inner.next().unwrap().as_str().to_string().into();
    let field = inner.next().unwrap();
    match field.as_rule() {
        Rule::delay => {
            let mut inner = field.into_inner();
            Delay {
                out,
                base: first,
                delay: inner.next().unwrap().as_str().parse::<usize>().unwrap(),
                on: inner.next().map(|v| v.as_str().to_string().into()),
            }
            .into()
        }
        Rule::sampleOn => SampleOn {
            out,
            trigger: first,
            base: field
                .into_inner()
                .next()
                .unwrap()
                .as_str()
                .to_string()
                .into(),
        }
        .into(),
        Rule::diff => {
            let mut inner = field.into_inner();
            Diff {
                out,
                base: first,
                from: inner.next().unwrap().as_str().parse().unwrap(),
                up_to: inner.next().unwrap().as_str().parse().unwrap(),
            }
            .into()
        }
        _ => unreachable!(),
    }
}

impl<T> From<Specification<T>> for kernel::constraints::Specification<T> {
    fn from(spec: Specification<T>) -> Self {
        Self(spec.constraints)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lccsl::format::render_lccsl;

    #[test]
    fn it_works() {
        let spec = "\
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

    #[test]
    fn roundtrip() {
        let spec = "\
        Specification demo {
            Clock p,a,m,t,c
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
        let spec_const = parse_to_string(spec).expect("should parse");
        let spec = render_lccsl(&spec_const.constraints, &spec_const.name);
        assert_eq!(
            remove_whitespace(&spec),
            remove_whitespace(&render_lccsl(&spec_const.constraints, &spec_const.name))
        );
    }
    fn remove_whitespace(s: &str) -> String {
        s.chars().filter(|c| !c.is_whitespace()).collect()
    }
}
