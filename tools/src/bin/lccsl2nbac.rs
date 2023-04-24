use anyhow::Result;
use ccsl::kernel::constraints::Specification;
use ccsl::lccsl::parser::parse_to_string;
use ccsl::symbolic::ts::TransitionSystem;
use nbac::goal::{boundness, deadlock};
use std::io::read_to_string;
use std::path::PathBuf;
use structopt::StructOpt;
use tools::{file_or_stdin, file_or_stdout};

#[derive(StructOpt, Debug)]
#[structopt(
    name = "ccsl2nbac",
    about = "Compiles LightCCSL specifications into NBAC format"
)]
struct App {
    spec: Option<PathBuf>,
    out: Option<PathBuf>,
    #[structopt(flatten)]
    property: Property,
}

#[derive(StructOpt, Debug)]
enum Property {
    Boundness {
        #[structopt(short, long, default_value = "256")]
        bound: u32,
    },
    Liveness {
        #[structopt(short, long, default_value = "256")]
        limit: u32,
    },
}

fn main() -> Result<()> {
    let args = App::from_args();
    let spec: Specification<String> =
        parse_to_string(&read_to_string(file_or_stdin(&args.spec)?)?)?.into();
    let nbac_spec = spec
        .into_iter()
        .map(|c| c.map_ref_into())
        .collect::<TransitionSystem<String>>()
        .into();
    let nbac_spec = match args.property {
        Property::Boundness { bound } => boundness(nbac_spec, bound),
        Property::Liveness { limit } => deadlock(nbac_spec, limit),
    };
    write!(&mut file_or_stdout(&args.out)?, "{}", nbac_spec)?;
    Ok(())
}
