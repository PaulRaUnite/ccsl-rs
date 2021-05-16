extern crate csv;
extern crate itertools;

use std::error::Error;
use std::path::PathBuf;

use structopt::StructOpt;

use ccsl::lccsl::automata::STS;
use ccsl::lccsl::constraints::{Constraint, Delay, Precedence};
use ccsl::lccsl::gen::{
    circle_spec, star, to_precedence_spec, to_subclocking_spec, tree, TreeIterator,
};
use ccsl::lccsl::opti::optimize_spec;
use itertools::Itertools;
use std::fs::File;
use std::io::BufWriter;
use tool::{analyze_specification, hash, hash_spec, write_graph, write_graph_no_label};

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

const FULL_HEADERS: [&str; 12] = [
    "spec",
    "variant",
    "comb",
    "real.tests",
    "real.downs",
    "real.solutions",
    "limit.tests",
    "limit.downs",
    "limit.solutions",
    "approx.tests",
    "approx.downs",
    "approx.solutions",
];
const SHORT_HEADERS: [&str; 8] = [
    "spec",
    "variant",
    "limit.tests",
    "limit.downs",
    "limit.solutions",
    "approx.tests",
    "approx.downs",
    "approx.solutions",
];

fn main() -> Result<(), Box<dyn Error>> {
    let opt: Opt = Opt::from_args();

    // all_constraints(&opt.dir.join("constraints"))?;

    let g = star(5, 3).unwrap();
    write_graph_no_label(&g, &opt.dir.join("gen"), "star")?;
    println!("{:?}", to_precedence_spec(&g));
    println!("{:?}", to_subclocking_spec(&g));

    let g = tree(5);
    write_graph_no_label(&g, &opt.dir.join("gen"), "tree")?;

    println!("{:?}", to_precedence_spec(&g));
    println!("{:?}", to_subclocking_spec(&g));

    for (i, g) in TreeIterator::new(9).enumerate() {
        write_graph_no_label(&g, &opt.dir.join("gen/test"), &i.to_string())?;
    }

    let mut raw_wrt = csv::WriterBuilder::new()
        .has_headers(false)
        .from_writer(BufWriter::new(File::create(
            "/home/paulra/Code/ccsl-rs/plotter/data.csv",
        )?));
    let mut optimized_wrt =
        csv::WriterBuilder::new()
            .has_headers(false)
            .from_writer(BufWriter::new(File::create(
                "/home/paulra/Code/ccsl-rs/plotter/optimized.csv",
            )?));
    let mut squished_wrt =
        csv::WriterBuilder::new()
            .has_headers(false)
            .from_writer(BufWriter::new(File::create(
                "/home/paulra/Code/ccsl-rs/plotter/squished.csv",
            )?));

    raw_wrt.write_record(&FULL_HEADERS)?;
    optimized_wrt.write_record(&FULL_HEADERS)?;
    squished_wrt.write_record(&SHORT_HEADERS)?;
    let gen_range = 3..=6;
    for spec in gen_range
        .clone()
        .map(|size| circle_spec(size).unwrap())
        .chain(gen_range.clone().map(|size| {
            to_precedence_spec(&star(size, 3).unwrap())
                .into_iter()
                .map(Into::into)
                .collect_vec()
        }))
        .chain(gen_range.clone().flat_map(|size| {
            TreeIterator::new(size + 1).map(|tr| {
                to_precedence_spec(&tr)
                    .into_iter()
                    .map(Into::into)
                    .collect_vec()
            })
        }))
        .chain(gen_range.clone().flat_map(|size| {
            TreeIterator::new(size + 1).map(|tr| {
                to_subclocking_spec(&tr)
                    .into_iter()
                    .map(Into::into)
                    .collect_vec()
            })
        }))
    {
        let len = spec.len();
        let permutations_amount: usize = (1..=len).product();
        let orig_hash = hash_spec(spec.iter());
        println!(
            "step: {}/{} ({})",
            len,
            gen_range.end(),
            permutations_amount
        );
        {
            let hashes = (&spec).into_iter().map(|c| hash(c)).collect_vec();
            let spec: Vec<STS<usize>> = spec.clone().into_iter().map(Into::into).collect();
            let opti_spec = optimize_spec(spec.as_slice());
            let (analysis, _) = analyze_specification(opti_spec, orig_hash, &hashes)?;
            for el in analysis {
                optimized_wrt.serialize(el)?;
            }
        }
        for perm in spec.into_iter().permutations(len) {
            let hashes = (&perm).into_iter().map(|c| hash(c)).collect_vec();

            let perm: Vec<STS<_>> = perm.into_iter().map(Into::into).collect();
            let (analysis, squished) = analyze_specification(perm, orig_hash, &hashes)?;
            for el in analysis {
                raw_wrt.serialize(el)?;
            }
            squished_wrt.serialize(squished)?;
        }
    }

    raw_wrt.flush()?;
    optimized_wrt.flush()?;
    squished_wrt.flush()?;

    // for name in names {
    //     Command::new("dot")
    //         .arg("-O")
    //         .arg("-Tpng")
    //         .arg(opt.dir.join(name).join(".dot"))
    //         .output()?;
    // }
    Ok(())
}
