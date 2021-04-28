extern crate itertools;

use std::error::Error;
use std::path::PathBuf;

use structopt::StructOpt;

use ccsl::lccsl::automata::STS;
use ccsl::lccsl::constraints::{Constraint, Delay, Precedence};
use tool::{all_constraints, analyze_specification};

#[derive(StructOpt, Debug)]
#[structopt(name = "basic", about = "Visualization of LightCCSL constraints")]
struct Opt {
    // // A flag, true if used in the command line. Note doc comment will
    // // be used for the help message of the flag. The name of the
    // // argument will be, by default, based on the name of the field.
    // /// Activate debug mode
    // #[structopt(short, long)]
    // debug: bool,
    //
    // // The number of occurrences of the `v/verbose` flag
    // /// Verbose mode (-v, -vv, -vvv, etc.)
    // #[structopt(short, long, parse(from_occurrences))]
    // verbose: u8,
    //
    // /// Set speed
    // #[structopt(short, long, default_value = "42")]
    // speed: f64,
    //
    // /// Output file
    // #[structopt(short, long, parse(from_os_str))]
    // output: PathBuf,
    //
    // // the long option will be translated by default to kebab case,
    // // i.e. `--nb-cars`.
    // /// Number of cars
    // #[structopt(short = "c", long)]
    // nb_cars: Option<i32>,
    //
    // /// admin_level to consider
    // #[structopt(short, long)]
    // level: Vec<String>,
    /// Files to process
    #[structopt(
        name = "DIR",
        parse(from_os_str),
        default_value = "/home/paulra/Code/ccsl-rs/tool/dot/"
    )]
    dir: PathBuf,
}

fn main() -> Result<(), Box<dyn Error>> {
    let opt: Opt = Opt::from_args();

    all_constraints(&opt.dir.join("constraints"))?;

    let spec: Vec<Constraint<&str>> = vec![
        Precedence {
            left: "a",
            right: "b",
            init: None,
            max: None,
        }
        .into(),
        Precedence {
            left: "b",
            right: "c",
            init: None,
            max: None,
        }
        .into(),
        Precedence {
            left: "c",
            right: "d",
            init: None,
            max: None,
        }
        .into(),
        Delay {
            out: "d",
            base: "a",
            delay: 1,
            on: None,
        }
        .into(),
    ];

    analyze_specification(&opt.dir.join("spec"), spec)?;

    // for name in names {
    //     Command::new("dot")
    //         .arg("-O")
    //         .arg("-Tpng")
    //         .arg(opt.dir.join(name).join(".dot"))
    //         .output()?;
    // }
    Ok(())
}
