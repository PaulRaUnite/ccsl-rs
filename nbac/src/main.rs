use anyhow::Result;
use ccsl::lccsl::automata::label::StaticBitmapLabel;
use ccsl::lccsl::automata::{Delta, STS};
use ccsl::lccsl::constraints::Specification;
use ccsl::lccsl::expressions::{
    BooleanComparisonKind, BooleanExpression, IntegerArithmeticsKind, IntegerComparisonKind,
    IntegerExpression,
};
use ccsl::lccsl::parser::{parse, parse_to_string};
use std::collections::HashSet;
use std::fmt::Display;

use ccsl::lccsl::analysis::Invariant;
use itertools::Itertools;
use nbac::{Spec, StateDeclaration};
use std::fs::{read_to_string, File};
use std::io::{BufWriter, Write};
use std::iter::once;
use std::ops::{BitAnd, BitOr, BitXor};
use std::path::PathBuf;
use structopt::StructOpt;

#[derive(StructOpt, Debug)]
#[structopt(
    name = "ccsl2nbac",
    about = "Compiles LightCCSL specifications into NBAC format"
)]
struct App {
    spec: PathBuf,
    out: PathBuf,
}

// enum Property {
//     Boundness {
//         #[structopt(short, long, default = "128")]
//         limit: u16,
//     },
// }

// TODO: replace by something like Cow<&static str|String>? Check equality for Cow
type Variable = String;
fn converti(expr: &IntegerExpression<Delta<Variable>>) -> nbac::IntegerExpression<Variable> {
    match expr {
        IntegerExpression::Variable(v) => {
            nbac::IntegerExpression::Variable(format!("d_{}_{}", v.0, v.1))
        }
        IntegerExpression::Constant(c) => nbac::IntegerExpression::Constant(*c),
        IntegerExpression::IntegerBinary { kind, left, right } => {
            nbac::IntegerExpression::IntegerBinary {
                kind: match kind {
                    IntegerArithmeticsKind::Add => nbac::IntegerArithmeticsKind::Add,
                    IntegerArithmeticsKind::Sub => nbac::IntegerArithmeticsKind::Sub,
                    IntegerArithmeticsKind::Mul => nbac::IntegerArithmeticsKind::Mul,
                },
                left: Box::new(converti(left)),
                right: Box::new(converti(right)),
            }
        }
    }
}
fn convertb(
    expr: &BooleanExpression<Delta<Variable>, Variable>,
) -> nbac::BooleanExpression<Variable> {
    match expr {
        BooleanExpression::IntegerBinary { kind, left, right } => {
            nbac::BooleanExpression::IntegerBinary {
                kind: match kind {
                    IntegerComparisonKind::Equal => nbac::IntegerComparisonKind::Equal,
                    IntegerComparisonKind::Less => nbac::IntegerComparisonKind::Less,
                    IntegerComparisonKind::LessEq => nbac::IntegerComparisonKind::LessEq,
                    IntegerComparisonKind::More => nbac::IntegerComparisonKind::More,
                    IntegerComparisonKind::MoreEq => nbac::IntegerComparisonKind::MoreEq,
                },
                left: Box::new(converti(&left)),
                right: Box::new(converti(&right)),
            }
        }
        BooleanExpression::BooleanBinary { kind, left, right } => {
            nbac::BooleanExpression::BooleanBinary {
                kind: match kind {
                    BooleanComparisonKind::And => nbac::BooleanComparisonKind::And,
                    BooleanComparisonKind::Or => nbac::BooleanComparisonKind::Or,
                    BooleanComparisonKind::Xor => nbac::BooleanComparisonKind::Xor,
                    BooleanComparisonKind::Eq => nbac::BooleanComparisonKind::Eq,
                },
                left: Box::new(convertb(&left)),
                right: Box::new(convertb(&right)),
            }
        }
        BooleanExpression::Not(expr) => nbac::BooleanExpression::Not(Box::new(convertb(&expr))),
        BooleanExpression::Constant(c) => nbac::BooleanExpression::Constant(*c),
        BooleanExpression::Variable(v) => nbac::BooleanExpression::Variable(v.clone()),
    }
}

fn translate_spec(spec: &Specification<Variable>) -> Spec<Variable> {
    let initial = nbac::BooleanExpression::var("init".to_owned());
    let ok = nbac::BooleanExpression::var("ok".to_owned());
    let mut nbac_spec = Spec::<Variable>::new(initial.clone(), !(initial.clone() | ok));
    nbac_spec
        .states
        .push(StateDeclaration::Bool("init".to_owned()));
    nbac_spec
        .states
        .push(StateDeclaration::Bool("ok".to_owned()));
    nbac_spec.transit("init".to_owned(), nbac::BooleanExpression::from(false));
    let mut states = HashSet::new();
    let mut clocks = HashSet::new();
    let mut clock_restrictions = vec![];
    for inv in spec.iter().map(Into::<Invariant<Variable>>::into) {
        inv.0.leaves(&mut states, &mut clocks);
        clock_restrictions.push(convertb(&inv.0));
    }
    nbac_spec
        .inputs
        .extend(clocks.iter().map(|v| StateDeclaration::Bool(v.clone())));
    nbac_spec.states.extend(
        states
            .iter()
            .map(|v| StateDeclaration::Integer(format!("d_{}_{}", v.0, v.1))),
    );
    for v in states {
        let var = format!("d_{}_{}", &v.0, &v.1);
        let (c1, c2) = (
            nbac::BooleanExpression::<Variable>::var(v.0),
            nbac::BooleanExpression::<Variable>::var(v.1),
        );
        nbac_spec.transit(
            var.clone(),
            initial.if_then_else(
                nbac::IntegerExpression::fixed(0),
                nbac::IntegerExpression::var(var) + c1.if_then_else(1, 0) - c2.if_then_else(1, 0),
            ),
        );
    }
    nbac_spec.assertion = Some(
        clock_restrictions
            .into_iter()
            .chain(once(
                clocks
                    .into_iter()
                    .map(nbac::BooleanExpression::var)
                    .reduce(BitOr::bitor)
                    .unwrap(),
            ))
            .reduce(BitAnd::bitand)
            .unwrap()
            | initial,
    );
    nbac_spec
}

fn add_boundness_goal(mut spec: Spec<Variable>, bound: u32) -> Spec<Variable> {
    let ok = nbac::BooleanExpression::var("ok".to_owned());
    let init = nbac::BooleanExpression::var("init".to_owned());
    spec.transit(
        "ok".to_owned(),
        init.if_then_elseb(
            true,
            ok & spec
                .states
                .iter()
                .filter_map(|s| match s {
                    StateDeclaration::Bool(_) => None,
                    StateDeclaration::Integer(v) => {
                        let v = nbac::IntegerExpression::var(v.clone());
                        Some(v.less_eq(bound as i64) & v.more_eq(-(bound as i64)))
                    }
                })
                .reduce(BitAnd::bitand)
                .unwrap(),
        ),
    );
    spec
}
fn add_deadlock_goal(mut spec: Spec<Variable>, step_limit: i64) -> Spec<Variable> {
    let ok = nbac::BooleanExpression::var("ok".to_owned());
    let init = nbac::BooleanExpression::var("init".to_owned());
    let clocks = spec
        .inputs
        .iter()
        .map(|d| d.variable())
        .cloned()
        .collect_vec();
    for v in &clocks {
        let dead_str = format!("dead_{}", v);
        let kill_str = format!("kill_{}", v);

        spec.inputs.push(StateDeclaration::Bool(kill_str.clone()));
        spec.states.push(StateDeclaration::Bool(dead_str.clone()));
        let dead = nbac::BooleanExpression::var(dead_str.clone());
        let kill = nbac::BooleanExpression::var(kill_str);
        spec.transit(dead_str, init.if_then_elseb(false, dead | kill));
    }

    let lock_count = nbac::IntegerExpression::var("lock_count".to_owned());
    spec.transit(
        "lock_count".to_owned(),
        init.if_then_else(
            0,
            (clocks
                .iter()
                .map(|v| nbac::BooleanExpression::var(format!("dead_{}", v)))
                .reduce(BitOr::bitor)
                .unwrap()
                & !clocks
                    .iter()
                    .map(|v| nbac::BooleanExpression::var(format!("dead_{}", v)))
                    .reduce(BitAnd::bitand)
                    .unwrap()) // TODO: not sure
            .if_then_else(lock_count.clone() + 1i64.into(), lock_count.clone()),
        ),
    );
    spec.assertion = Some(match spec.assertion.unwrap() {
        nbac::BooleanExpression::BooleanBinary { kind, left, right } => {
            nbac::BooleanExpression::BooleanBinary {
                kind,
                left: Box::new(
                    *left
                        & clocks
                            .iter()
                            .map(|c| {
                                nbac::BooleanExpression::var(format!("dead_{}", c))
                                    .implies(nbac::BooleanExpression::var(c.to_owned()))
                            })
                            .reduce(BitAnd::bitand)
                            .unwrap(),
                ),
                right,
            }
        }
        _ => panic!("expected boolean or"),
    });
    spec.transit(
        "ok".to_owned(),
        init.if_then_elseb(true, ok & lock_count.less_eq(step_limit)),
    );
    spec
}

fn main() -> Result<()> {
    let args = App::from_args();
    let spec: Specification<String> = parse_to_string(&read_to_string(args.spec)?)?.into();

    let nbac_spec = add_deadlock_goal(translate_spec(&spec), 4);

    let mut out_file = BufWriter::new(File::create(&args.out)?);
    write!(&mut out_file, "{}", nbac_spec)?;
    out_file.flush()?;

    // let sts: Vec<STS<String, StaticBitmapLabel>> = vec_into_vec(&spec);
    Ok(())
}
