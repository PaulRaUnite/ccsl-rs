use std::fmt::{Display, Formatter};
use std::fs::OpenOptions;
use std::io::{BufWriter, Write};
use std::num::NonZeroUsize;
use std::path::{Path, PathBuf};

use rand::{Rng, SeedableRng};
use structopt::StructOpt;

use ccsl::lccsl::format::render_lccsl;

use anyhow::{anyhow, Result};
use ccsl::generation::graph::random_processing_network;
use ccsl::generation::specification::{
    cycle_with_tail_and_spike, point_backpressure, precedence_trees,
    random_connected_specification, to_precedence_spec, trees_with_backpressure, NetworkParams,
};
use ccsl::kernel::constraints::Constraint;
use itertools::Itertools;
use rand::prelude::{SliceRandom, StdRng};

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
        #[structopt(flatten)]
        family: Family,
        /// Path to a directory to generate CCSL specifications
        dir: PathBuf,
        /// Amount of specifications to generate
        #[structopt(short, long)]
        amount: usize,
        /// Outputs directly into the specified directory without creating a subdirectory for specific starting seed
        #[structopt(short, long)]
        flatten: bool,
        /// Output graph alongside (in DOT format)
        #[structopt(short, long)]
        graph: bool,
        /// Conflict-based topological sorting
        #[structopt(short, long)]
        sort: bool,
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

#[derive(StructOpt, Debug)]
enum Family {
    Rand {
        /// Starting seed of a random generator to generate specification seeds
        #[structopt(short, long)]
        seed: u64,
        /// Size of specifications to generate
        #[structopt(short, long)]
        size: usize,
    },
    Tree {
        /// Size of specifications to generate
        #[structopt(short, long)]
        size: usize,
        /// Add backpressure constraints (to make it finite)
        #[structopt(short, long)]
        backpressure: Option<NonZeroUsize>,
    },
    Network {
        /// Layer's dimensions (format is "1,2,3")
        #[structopt(short, long = "dim")]
        dimensions: NetworkParams,
        /// Starting seed of a random generator to generate specification seeds
        #[structopt(short, long)]
        seed: u64,
        /// Add backpressure constraints (to make it finite)
        #[structopt(short, long)]
        backpressure: Option<NonZeroUsize>,
    },
    Cycle {
        /// Size of the cycle
        #[structopt(short, long)]
        size: usize,
        /// Size of tail (from 0 to the argument)
        #[structopt(short, long)]
        tail_up_to: usize,
        /// Size of spike (from 0 to the argument)
        #[structopt(short, long)]
        spike_up_to: usize,
        /// Add backpressure constraint (between head and tail)
        #[structopt(short, long)]
        backpressure: Option<NonZeroUsize>,
    },
}

impl Display for Family {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Family::Rand { .. } => "rand",
            Family::Tree { .. } => "tree",
            Family::Network { .. } => "network",
            Family::Cycle { .. } => "cycle",
        };
        write!(f, "{}", s)
    }
}

fn generate_to_dir(
    dir: &Path,
    specs: impl Iterator<Item = (String, Vec<Constraint<usize>>)>,
) -> Result<()> {
    for (spec_name, spec) in specs {
        let filepath_spec = dir.join(&spec_name).with_extension("lc");
        let mut spec_file = BufWriter::new(
            OpenOptions::new()
                .write(true)
                .create(true)
                .open(filepath_spec)?,
        );
        let spec = spec
            .into_iter()
            .map(|c| c.map(|clock| format!("c{}", clock)))
            .collect_vec();
        write!(spec_file, "{}", &render_lccsl(&spec, &spec_name))?;
    }
    Ok(())
}

fn specification_directory_structure(family: &Family, dir: PathBuf) -> PathBuf {
    match &family {
        Family::Rand { size, seed } => dir.join(seed.to_string()).join(size.to_string()),
        Family::Tree {
            size, backpressure, ..
        }
        | Family::Cycle {
            size, backpressure, ..
        } => {
            let dir = dir.join(size.to_string());
            if backpressure.is_some() {
                dir.join("bp")
            } else {
                dir
            }
        }
        Family::Network {
            dimensions,
            seed,
            backpressure,
            ..
        } => {
            let dir = dir.join(seed.to_string()).join(dimensions.to_string());
            if backpressure.is_some() {
                dir.join("bp")
            } else {
                dir
            }
        }
    }
}

fn unique_random_seeds(seed: u64, amount: usize) -> impl Iterator<Item = u64> {
    let mut rng = StdRng::seed_from_u64(seed);
    (0..).map(move |_| rng.gen()).unique().take(amount)
}

fn main() -> Result<()> {
    let app: App = App::from_args();

    match app.cmd {
        Cmd::Dir {
            family,
            dir,
            amount,
            flatten,
            graph,
            sort,
        } => {
            let dir = if flatten {
                dir
            } else {
                let dir = dir.join(family.to_string());
                let dir = specification_directory_structure(&family, dir);
                if let Err(e) = std::fs::create_dir_all(&dir) {
                    if e.kind() != std::io::ErrorKind::AlreadyExists {
                        return Err(e.into());
                    }
                }
                dir
            };
            if !dir.exists() {
                return Err(anyhow!(
                    "Directory doesn't exist: {}",
                    dir.to_str().unwrap()
                ));
            }
            match family {
                Family::Rand { size, seed } => generate_to_dir(
                    &dir,
                    unique_random_seeds(seed, amount).map(|seed| {
                        (
                            format!("rand_{}_{}", size, seed),
                            random_connected_specification(seed, size, true),
                        )
                    }),
                )?,
                Family::Tree { size, backpressure } => {
                    let specs: Box<dyn Iterator<Item = Vec<Constraint<usize>>>> =
                        if let Some(buffer) = backpressure {
                            Box::new(trees_with_backpressure(size, buffer.get()))
                        } else {
                            Box::new(precedence_trees(size))
                        };
                    generate_to_dir(
                        &dir,
                        specs
                            .enumerate()
                            .map(|(i, spec)| (format!("tree_{}", i), spec)),
                    )?
                }
                Family::Network {
                    dimensions,
                    seed,
                    backpressure,
                } => {
                    let specs = unique_random_seeds(seed, amount).map(|seed| {
                        let (g, inputs, outputs) = random_processing_network(seed, &dimensions);
                        let mut spec = to_precedence_spec(&g);
                        if let Some(buffer) = backpressure {
                            let mut rng = StdRng::seed_from_u64(seed);
                            spec.extend(point_backpressure(
                                inputs.choose(&mut rng).unwrap().index(),
                                outputs.choose(&mut rng).unwrap().index(),
                                buffer.get(),
                            ))
                        }
                        (format!("net_{}", seed), spec)
                    });
                    generate_to_dir(&dir, specs)?
                }
                Family::Cycle {
                    size,
                    tail_up_to,
                    spike_up_to,
                    backpressure,
                } => generate_to_dir(
                    &dir,
                    (0..tail_up_to)
                        .cartesian_product(0..spike_up_to)
                        .map(|(tail, spike)| {
                            let mut spec = cycle_with_tail_and_spike(size, tail, spike_up_to, 1);
                            if let Some(buffer) = backpressure {
                                spec.extend(point_backpressure(
                                    0,
                                    size + tail_up_to + spike_up_to,
                                    buffer.get(),
                                ))
                            }
                            (format!("cycle_{}_{}_{}", tail, size, spike), spec)
                        }),
                )?,
            }
        }
        Cmd::One { size, seed, file } => {
            let spec_name = format!("rand_{}_{}", size, seed);
            let spec = random_connected_specification(seed, size, true);
            if let Some(filepath) = file {
                let file = OpenOptions::new().write(true).create(true).open(filepath)?;
                write!(BufWriter::new(file), "{}", &render_lccsl(&spec, &spec_name))?;
            } else {
                write!(std::io::stdout(), "{}", &render_lccsl(&spec, &spec_name))?;
            };
        }
    }
    Ok(())
}
