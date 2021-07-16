use std::path::PathBuf;
use tool::all_constraints;

use std::error::Error;
use structopt::StructOpt;

#[derive(StructOpt, Debug)]
#[structopt(name = "basic", about = "Processing of LightCCSL constraints")]
struct Opt {
    dir: PathBuf,
}

fn main() -> Result<(), Box<dyn Error>> {
    let opt: Opt = Opt::from_args();
    all_constraints(&opt.dir)
}
