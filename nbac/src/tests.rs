use crate::goal::{negative_trace_check, positive_trace_check};
use crate::{Spec, Variable};
use anyhow::{bail, Result};
use ccsl::kernel::test_corpus::all;
use ccsl::symbolic::ts::TransitionSystem;
use itertools::Itertools;
use regex::Regex;
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
            !run(&aug_spec).unwrap()
        } else {
            let aug_spec = negative_trace_check(nbac_spec.clone(), &trace);
            println!("{}", &aug_spec);
            run(&aug_spec).unwrap()
        };
        validator
            .given(&format!("{}\n{}", c, &trace))
            .when("check trace")
            .then(&format!("it should be {}", expected))
            .assert_eq(expected, result);
    }
}

fn run(spec: &Spec<Variable>) -> Result<bool> {
    let mut temp_file = NamedTempFile::new()?;
    write!(&mut temp_file, "{}", &spec)?;
    temp_file.flush()?;
    println!("{}", temp_file.path().display());

    let output = Command::new("./tool/nbacg.opt")
        .env("LD_LIBRARY_PATH", "./tool/")
        .arg("-postpre")
        .arg("-drelation 1")
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
        "SUCCESS" => false,
        "DON'T KNOW" => true,
        _ => {
            println!("{}", temp_file.path().display());
            println!("{}", text);
            println!("{}", from_utf8(&output.stderr).unwrap());
            panic!("shouldn't reach")
        }
    };
    Ok(nbac_out)
}
