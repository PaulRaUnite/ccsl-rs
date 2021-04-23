extern crate itertools;

use std::collections::{HashMap, HashSet};
use std::error::Error;
use std::fmt::Display;
use std::fs::File;
use std::io::BufWriter;
use std::io::Write;
use std::path::{Path, PathBuf};

use itertools::Itertools;
use petgraph::dot::{Config, Dot};
use petgraph::Graph;
use structopt::StructOpt;

use ccsl::lccsl::algo::{compare_approx_and_solutions, conflict_map_combinations};
use ccsl::lccsl::automata::STS;
use ccsl::lccsl::constraints::{
    Alternates, Causality, Coincidence, Delay, Exclusion, Intersection, Precedence, Subclocking,
    Union,
};
use ccsl::lccsl::vizualization::unfold_specification;

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
    #[structopt(name = "DIR", parse(from_os_str), default_value = "./dot/")]
    dir: PathBuf,
}

macro_rules! collection {
    // map-like
    ($($k:expr => $v:expr),* $(,)?) => {
        std::iter::Iterator::collect(std::array::IntoIter::new([$(($k, $v),)*]))
    };
    // set-like
    ($($v:expr),* $(,)?) => {
        std::iter::Iterator::collect(std::array::IntoIter::new([$($v,)*]))
    };
}

fn write_graph<N: Display, E: Display>(
    g: &Graph<N, E>,
    dir: &Path,
    file: &str,
) -> Result<(), Box<dyn Error>> {
    let mut file = BufWriter::new(File::create(dir.join(file).with_extension("dot"))?);
    let dot = Dot::new(g);
    writeln!(&mut file, "{}", &dot)?;
    Ok(())
}

fn main() -> Result<(), Box<dyn Error>> {
    let opt: Opt = Opt::from_args();
    let mut map: HashMap<&str, STS<&str>> = HashMap::new();
    map.insert(
        "coincidence",
        Coincidence {
            left: "a",
            right: "b",
        }
        .into(),
    );
    map.insert(
        "alternates",
        Alternates {
            left: "a",
            right: "b",
        }
        .into(),
    );
    map.insert(
        "causality",
        Causality {
            left: "a",
            right: "b",
            init: None,
            max: None,
        }
        .into(),
    );
    map.insert(
        "precedence",
        Precedence {
            left: "a",
            right: "b",
            init: None,
            max: None,
        }
        .into(),
    );
    map.insert(
        "exclusion",
        Exclusion {
            clocks: collection!("a", "b"),
        }
        .into(),
    );
    map.insert(
        "subclocking",
        Subclocking {
            left: "a",
            right: "b",
        }
        .into(),
    );
    map.insert(
        "intersection",
        Intersection {
            out: "i",
            args: collection!("a", "b"),
        }
        .into(),
    );
    map.insert(
        "union",
        Union {
            out: "u",
            args: collection!("a", "b"),
        }
        .into(),
    );
    map.insert(
        "delay",
        Delay {
            out: "d",
            base: "a",
            delay: 2,
            on: None,
        }
        .into(),
    );
    for (name, c) in map.into_iter() {
        let g: petgraph::Graph<_, _> = STS::from(c).into();
        write_graph(&g, &opt.dir, name)?;
    }

    let spec: Vec<STS<&str>> = vec![
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
    let conflict_dir = opt.dir.join("conflict");
    let dir = conflict_dir.join("tree");
    for (i, g) in unfold_specification(&spec).iter().enumerate() {
        write_graph(g, &dir, &i.to_string())?;
    }

    let dir = conflict_dir.join("map");
    for (i, g) in conflict_map_combinations(&spec).iter().enumerate() {
        write_graph(g, &dir, &i.to_string())?;
    }

    for (actual, counter, approx) in compare_approx_and_solutions(&spec) {
        let (numer, denom) = approx.into();
        println!(
            "actual: {}, counter: {}, approximation: {:.4}",
            actual,
            counter,
            (numer as f64) / (denom as f64)
        );
    }

    let dir = conflict_dir.join("squish");
    let spec: Vec<_> = spec.into_iter().map(|c| c.squish()).collect();
    let g = unfold_specification(&spec).into_iter().exactly_one()?;
    write_graph(&g, &dir, "tree")?;

    let g = conflict_map_combinations(&spec)
        .into_iter()
        .exactly_one()
        .unwrap();
    write_graph(&g, &dir, "map")?;

    for (actual, counter, approx) in compare_approx_and_solutions(&spec) {
        let (numer, denom) = approx.into();
        println!(
            "actual: {}, counter: {}, approximation: {:.4}",
            actual,
            counter,
            (numer as f64) / (denom as f64)
        );
    }
    // for name in names {
    //     Command::new("dot")
    //         .arg("-O")
    //         .arg("-Tpng")
    //         .arg(opt.dir.join(name).join(".dot"))
    //         .output()?;
    // }
    Ok(())
}
