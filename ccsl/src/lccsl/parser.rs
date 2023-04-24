use std::collections::{BTreeSet, HashMap, HashSet};
use std::fmt::Debug;

use derive_more::From;
use pest::iterators::{Pair, Pairs};
use pest::pratt_parser::{Assoc, Op, PrattParser};
use pest::Parser;
use thiserror::Error;

use crate::kernel::constraints::*;
use itertools::Itertools;
use std::cell::RefCell;
use std::hash::Hash;
use std::iter::FromIterator;
use std::ops::RangeFrom;
use std::slice::Iter;
use std::vec::IntoIter;

// origin LcDsl.xtext from lightccsl-feature.zip/plugins/lcdsl.jar
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

impl<C> Specification<C> {
    pub fn map_clocks<B, F>(&self, mut f: F) -> Specification<B>
    where
        F: FnMut(&C) -> B,
        B: Hash + Eq + Ord,
    {
        Specification {
            name: self.name.clone(),
            clocks: self.clocks.iter().map(&mut f).collect(),
            constraints: self
                .constraints
                .iter()
                .map(|c| c.map_clocks(|c| f(c)))
                .collect(),
        }
    }

    pub fn iter(&self) -> Iter<'_, Constraint<C>> {
        self.into_iter()
    }
}

impl<C> IntoIterator for Specification<C> {
    type Item = Constraint<C>;
    type IntoIter = IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.constraints.into_iter()
    }
}

impl<'a, C> IntoIterator for &'a Specification<C> {
    type Item = &'a Constraint<C>;
    type IntoIter = Iter<'a, Constraint<C>>;

    fn into_iter(self) -> Self::IntoIter {
        self.constraints.iter()
    }
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
//  Main problem is that some expression can nest, creating anonymous clocks,
//  which then are rendered explicitly in roundtrip test.

impl<'a> TryFrom<&'a str> for Specification<ID<'a>> {
    type Error = ParseError;

    fn try_from(input: &'a str) -> Result<Self, Self::Error> {
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
}

impl<'a> TryFrom<&'a str> for Specification<u32> {
    type Error = ParseError;

    fn try_from(input: &'a str) -> Result<Self, Self::Error> {
        let spec: Specification<ID> = input.try_into()?;
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
                .map(|c| c.map_clocks(|c| *unique_clocks.get(&c).unwrap()))
                .collect(),
        })
    }
}

impl<'a> TryFrom<&'a str> for Specification<String> {
    type Error = ParseError;

    fn try_from(input: &'a str) -> Result<Self, Self::Error> {
        let spec: Specification<ID> = input.try_into()?;
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
        let prefix = "gc_";
        if unique_clocks.contains(&prefix) {
            panic!("couldn't generate prefix for numeric clocks")
        }

        Ok(Specification {
            name: spec.name,
            clocks: spec
                .clocks
                .into_iter()
                .map(|c| match c {
                    ID::C(s) => s.to_owned(),
                    ID::G(n) => format!("{}{}", prefix, n),
                })
                .collect(),
            constraints: spec
                .constraints
                .iter()
                .map(|c| {
                    c.map_clocks(|c| match c {
                        ID::C(s) => (*s).to_owned(),
                        ID::G(n) => format!("{}{}", prefix, n),
                    })
                })
                .collect(),
        })
    }
}

fn parse_clocks(input: Pair<Rule>) -> impl Iterator<Item = ID> {
    input.into_inner().map(|clock| clock.as_str().into())
}

fn parse_repeat(input: Pair<Rule>) -> Constraint<ID> {
    let mut every = 1;
    let mut clocks: Vec<ID> = Vec::with_capacity(2);
    let mut from = None;
    let mut up_to = None;

    for field in input.into_inner() {
        match field.as_rule() {
            Rule::id => clocks.push(field.as_str().into()),
            Rule::int => every = field.as_str().parse::<usize>().unwrap_or(1),
            Rule::from => from = Some(field.into_inner().as_str().parse::<usize>().unwrap()),
            Rule::up_to => up_to = parse_infint(field),
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
            Rule::id => clocks.push(field.as_str().into()),
            Rule::causality_kind => kinds.push(field.as_str() == "<="),
            Rule::causal_params => {
                params.insert(clocks.len() - 1, parse_causal_params(field));
            }
            _ => unreachable!(),
        }
    }
    let mut first = clocks.remove(0);
    let mut constraints: Vec<Constraint<ID>> = Vec::with_capacity(clocks.len());
    for (i, (clock, causality)) in clocks.into_iter().zip(kinds).enumerate() {
        let (init, max) = params.get(&i).copied().unwrap_or((None, None));
        constraints.push(if causality {
            Causality {
                cause: first,
                effect: clock.clone(),
                init,
                max,
            }
            .into()
        } else {
            Precedence {
                cause: first,
                effect: clock.clone(),
                init,
                max,
            }
            .into()
        });
        first = clock;
    }
    constraints.into_iter()
}

fn parse_causal_params(input: Pair<Rule>) -> (Option<usize>, Option<usize>) {
    let mut init = None;
    let mut max = None;
    for field in input.into_inner() {
        match field.as_rule() {
            Rule::init => {
                init = Some(
                    field
                        .into_inner()
                        .next()
                        .unwrap()
                        .as_str()
                        .parse::<usize>()
                        .unwrap(),
                );
            }
            Rule::maximum => {
                max = parse_infint(field.into_inner().next().unwrap());
            }
            _ => unreachable!(),
        }
    }
    (init, max)
}
fn parse_subclocking(input: Pair<Rule>) -> impl Iterator<Item = Constraint<ID>> {
    let mut clocks = parse_clock_arguments(input);
    let mut first = clocks.remove(0);
    let mut constraints: Vec<Constraint<ID>> = Vec::with_capacity(clocks.len());
    for clock in clocks {
        constraints.push(
            Subclocking {
                sub: first,
                sup: clock.clone(),
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
            Rule::id => clocks.push(field.as_str().into()),
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
pub enum ID<'a> {
    C(&'a str),
    G(usize),
}

fn parse_let_expr<'i>(
    input: Pair<'i, Rule>,
    gen: &RefCell<RangeFrom<usize>>,
) -> impl Iterator<Item = Constraint<ID<'i>>> {
    let mut inner = input.into_inner();
    let mut constraints = Vec::new();
    let out: ID = inner.next().unwrap().as_str().into();
    let expr_out = parse_expression(inner, gen, &mut constraints);
    if let Some(last) = constraints.last_mut() {
        *last = last.map_clocks(|c| {
            if c == &expr_out {
                out.clone()
            } else {
                c.clone()
            }
        });
    } else {
        constraints.push(
            Coincidence {
                left: out,
                right: expr_out,
            }
            .into(),
        );
    }
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

fn parse_expression<'a>(
    input: Pairs<'a, Rule>,
    gen: &RefCell<RangeFrom<usize>>,
    container: &mut Vec<Constraint<ID<'a>>>,
) -> ID<'a> {
    let mut primaries: Vec<Constraint<ID>> = Vec::new();
    let out = PREC_CLIMBER
        .map_primary(|pair: Pair<Rule>| match pair.as_rule() {
            Rule::id => pair.as_str().into(),
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

fn parse_prefix_expression<'a>(
    input: Pair<'a, Rule>,
    gen: &RefCell<RangeFrom<usize>>,
    container: &mut Vec<Constraint<ID<'a>>>,
) -> ID<'a> {
    let mut inner = input.into_inner();
    let sup = inner.next().unwrap().as_str() == "sup";
    let args = inner.map(|c| c.as_str().into()).collect_vec();
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
                    Infimum {
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
    let out: ID = inner.next().unwrap().as_str().into();
    let first: ID = inner.next().unwrap().as_str().into();
    let field = inner.next().unwrap();
    match field.as_rule() {
        Rule::delay => {
            let mut inner = field.into_inner();
            Delay {
                out,
                trigger: first,
                delay: inner.next().unwrap().as_str().parse::<usize>().unwrap(),
                on: inner.next().map(|v| v.as_str().into()),
            }
            .into()
        }
        Rule::sampleOn => SampleOn {
            out,
            trigger: first,
            base: field.into_inner().next().unwrap().as_str().into(),
        }
        .into(),
        Rule::slice => {
            let mut inner = field.into_inner();
            Slice {
                out,
                base: first,
                from: inner.next().unwrap().as_str().parse().unwrap(),
                up_to: parse_infint(inner.next().unwrap()),
            }
            .into()
        }
        _ => unreachable!(),
    }
}

fn parse_infint(input: Pair<Rule>) -> Option<usize> {
    match input.as_str() {
        "Infinity" | "∞" => None,
        x => Some(x.parse::<usize>().unwrap()),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lccsl::format::render;

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
            Precedence a < (init: 5 max: 10) m
            Precedence a < (max: 10) m
            SubClocking onn <- a
            Exclusion off # a
            Let out be m and t
            ]
        }";
        let spec_const: Specification<String> = spec.try_into().expect("should parse");
        let spec = render(&spec_const.constraints, &spec_const.name);
        assert_eq!(
            remove_whitespace(&spec),
            remove_whitespace(&render(&spec_const.constraints, &spec_const.name))
        );
    }
    fn remove_whitespace(s: &str) -> String {
        s.chars().filter(|c| !c.is_whitespace()).collect()
    }
}
