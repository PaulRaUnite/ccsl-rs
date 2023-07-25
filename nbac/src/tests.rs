use crate::goal::{negative_trace_check, positive_trace_check, with_observer_spec};
use crate::tests::NbacResult::{DontKnow, Success};
use crate::{BooleanExpression, IntegerExpression, Spec, Variable, VariableDeclaration};
use anyhow::{bail, Result};
use ccsl::kernel::test_corpus::all;
use ccsl::lccsl::parser::Specification;
use ccsl::symbolic::ts::TransitionSystem;
use itertools::Itertools;
use regex::Regex;
use std::fmt::{Display, Formatter};
use std::io::Write;
use std::process::Command;
use std::str::from_utf8;
use structopt::lazy_static::lazy_static;
use table_test::table_test;
use tempfile::NamedTempFile;

#[test]
fn basic() {
    let tests = all()
        .into_iter()
        .flat_map(|(c, test_set)| {
            test_set
                .into_iter()
                .map(move |(trace, expected)| ((c.clone(), trace), expected))
        })
        .collect_vec();
    for (validator, (c, trace), expected) in table_test!(tests) {
        let ts: TransitionSystem<String> = c.map_clocks(|c| format!("c{}", c)).map_ref_into();
        let nbac_spec: Spec<Variable> = ts.into();
        let trace = trace.map_clocks(|c| Variable::Owned(format!("c{}", c)));
        let result = if expected {
            let aug_spec = positive_trace_check(nbac_spec.clone(), &trace);
            println!("{}", &aug_spec);
            run(&aug_spec).unwrap()
        } else {
            let aug_spec = negative_trace_check(nbac_spec.clone(), &trace);
            println!("{}", &aug_spec);
            run(&aug_spec).unwrap()
        };
        validator
            .given(&format!("{}\n{}", c, &trace))
            .when("check trace")
            .then(&format!("it should be {}", expected))
            .assert_eq(Success, result);
    }
}

#[test]
fn periodic() {
    // TODO: make multiplicative periodic specification
    let (p1, p2, p3) = (3, 5, 7);
    let spec: Specification<String> = format!(
        "Specification example {{
    Clock m,a,b,c,r
    [
        repeat a every {p1} m
        repeat b every {p2} m
        repeat c every {p3} m
        Let i be a * b * c
    ]
}}"
    )
    .as_str()
    .try_into()
    .unwrap();
    let ts: TransitionSystem<Variable> = spec
        .into_iter()
        .map(|c| {
            c.map_clocks(|clock| Variable::Owned(clock.clone()))
                .map_ref_into()
        })
        .collect();
    let mut nbac_spec: Spec<Variable> = ts.into();
    nbac_spec.states.push(VariableDeclaration::int("m_i"));
    nbac_spec.states.push(VariableDeclaration::int("i_i"));
    let init = BooleanExpression::var("init");
    nbac_spec.transit(
        "m_i",
        init.if_then_else(
            0,
            IntegerExpression::var("m_i") + BooleanExpression::var("m").if_then_else(1, 0),
        ),
    );
    nbac_spec.transit(
        "i_i",
        init.if_then_else(
            0,
            IntegerExpression::var("i_i") + BooleanExpression::var("i").if_then_else(1, 0),
        ),
    );
    nbac_spec.transit(
        "ok",
        init.if_then_elseb(
            true,
            BooleanExpression::var("ok")
                & BooleanExpression::var("i").implies(
                    (IntegerExpression::var("m_i") + 1.into())
                        .eq((IntegerExpression::var("i_i") + 1.into()) * (p1 * p2 * p3).into()),
                ),
        ),
    );
    println!("{}", &nbac_spec);
    assert_eq!(run(&nbac_spec).unwrap(), Success);
}

#[test]
fn periodic_observer() {
    let (p1, p2) = (2, 3);
    let spec: Specification<String> = format!(
        "Specification example {{
    Clock m,a,b,i
    [
        repeat a every {p1} m
        repeat b every {p2} m
        Let i be a * b
    ]
}}"
    )
    .as_str()
    .try_into()
    .unwrap();
    let observer: Specification<String> = format!(
        "Specification observer {{
    Clock m,i
    [
        repeat i every {} m
    ]
}}",
        p1 * p2
    )
    .as_str()
    .try_into()
    .unwrap();
    let spec = with_observer_spec(spec, observer);

    println!("{}", &spec);
    assert_eq!(run(&spec).unwrap(), Success);
}
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
enum NbacResult {
    Success,
    DontKnow,
}

impl Display for NbacResult {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Success => write!(f, "SUCCESS"),
            DontKnow => write!(f, "DON'T KNOW"),
        }
    }
}

fn run(spec: &Spec<Variable>) -> Result<NbacResult> {
    let mut temp_file = NamedTempFile::new()?;
    write!(&mut temp_file, "{}", &spec)?;
    temp_file.flush()?;
    println!("{}", temp_file.path().display());

    let output = Command::new("./tool/nbacg.opt")
        .env("LD_LIBRARY_PATH", "./tool/")
        // .arg("-postpre")
        // .arg("-drelation 1")
        .arg(temp_file.path())
        .output()?;
    if !output.stderr.is_empty() {
        bail!(
            "NBAC crashed: {}\n{}{}",
            temp_file.path().display(),
            from_utf8(&output.stdout).unwrap(),
            from_utf8(&output.stderr).unwrap(),
        )
    }
    let text = from_utf8(&output.stdout).unwrap();
    lazy_static! {
        static ref RE_NBAC: Regex = Regex::new("(SUCCESS)|(DON'T KNOW)").unwrap();
        static ref RE_MG: Regex = Regex::new("(true)|(false)").unwrap();
    }
    let nbac_out = match RE_NBAC.find(text).map(|m| m.as_str()).unwrap_or("") {
        "SUCCESS" => Success,
        "DON'T KNOW" => DontKnow,
        _ => {
            println!("{}", temp_file.path().display());
            println!("{}", text);
            println!("{}", from_utf8(&output.stderr).unwrap());
            panic!("shouldn't reach")
        }
    };
    Ok(nbac_out)
}
