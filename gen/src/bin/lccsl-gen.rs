use std::fs::OpenOptions;
use std::io::{BufWriter, Write};
use std::path::{Path, PathBuf};

use rand::{Rng, SeedableRng};
use rayon::prelude::{ParallelBridge, ParallelIterator};
use structopt::StructOpt;

use ccsl::lccsl::format::to_lccsl;

use anyhow::{anyhow, Result};
use gen::generation::random_connected_specification;
use itertools::Itertools;

#[derive(StructOpt, Debug)]
#[structopt(name = "lccsl-gen", about = "LightCCSL specification generator")]
struct App {
    #[structopt(subcommand)]
    cmd: Cmd,
}

#[derive(StructOpt, Debug)]
enum Cmd {
    /// Generate a directory of specifications
    /// Files are outputted in LightCCSL format, with name format <size>-<seed>.lc
    /// Subdirectory is created for easier separation
    Dir {
        /// Path to a directory to generate CCSL specifications
        dir: PathBuf,
        /// Size of specifications to generate
        #[structopt(short, long)]
        size: usize,
        /// Amount of specifications to generate
        #[structopt(short, long)]
        amount: usize,
        /// Starting seed of a random generator to generate specification seeds
        #[structopt(short, long)]
        seed: u64,
        /// Outputs directly into the specified directory without creating a subdirectory for specific starting seed
        #[structopt(short, long)]
        flatten: bool,
        /// Flag to disable parallelism
        #[structopt(short, long)]
        no_par: bool,
    },
    /// Generate one specification
    One {
        /// File path to the output file, stdout if omitted
        file: Option<PathBuf>,
        /// Size of the specification
        #[structopt(short, long)]
        size: usize,
        /// Seed of the specification
        #[structopt(short, long)]
        seed: u64,
    },
}

fn generate_spec(dir: &Path, seed: u64, size: usize) -> Result<()> {
    let filename = format!("{}-{}", size, seed);
    let filepath = dir.join(&filename).with_extension("lc");
    let file = BufWriter::new(OpenOptions::new().write(true).create(true).open(filepath)?);
    write_spec(file, seed, size)
}

fn write_spec(mut w: impl Write, seed: u64, size: usize) -> Result<()> {
    let spec_name = format!("spec_{}_{}", size, seed);
    let spec = random_connected_specification(seed, size, true)
        .into_iter()
        .map(|c| c.map(&mut |clock| format!("c{}", clock)))
        .collect_vec();
    write!(w, "{}", &to_lccsl(&spec, &spec_name))?;
    Ok(())
}

fn generate_dir(
    dir: &Path,
    specs: impl Iterator<Item = (u64, usize)> + Send,
    parallel: bool,
) -> Result<()> {
    let mut results: Vec<Result<()>> = if parallel {
        specs
            .par_bridge()
            .map(|(seed, size)| generate_spec(dir, seed, size))
            .filter(|r| r.is_err())
            .collect()
    } else {
        specs
            .map(|(seed, size)| generate_spec(dir, seed, size))
            .filter(|r| r.is_err())
            .collect()
    };
    if !results.is_empty() {
        results.pop().unwrap()
    } else {
        Ok(())
    }
}

fn main() -> Result<()> {
    let app: App = App::from_args();

    match app.cmd {
        Cmd::Dir {
            dir,
            size,
            amount,
            seed,
            flatten,
            no_par,
        } => {
            let mut rng = rand::rngs::StdRng::seed_from_u64(seed);
            if !dir.exists() {
                return Err(anyhow!("Specified directory doesn't exist"));
            }
            let dir = if flatten {
                dir
            } else {
                let dir = dir.join(seed.to_string());
                if let Err(e) = std::fs::create_dir(&dir) {
                    if e.kind() != std::io::ErrorKind::AlreadyExists {
                        return Err(e.into());
                    }
                }
                dir
            };
            generate_dir(&dir, (0..amount).map(|_| (rng.gen(), size)), !no_par)?;
        }
        Cmd::One { size, seed, file } => {
            if let Some(filepath) = file {
                let file = OpenOptions::new().write(true).create(true).open(filepath)?;
                write_spec(BufWriter::new(file), seed, size)?;
            } else {
                write_spec(std::io::stdout(), seed, size)?;
            };
        }
    }
    Ok(())
}
