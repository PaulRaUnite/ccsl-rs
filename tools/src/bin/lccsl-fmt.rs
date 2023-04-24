use anyhow::Result;
use ccsl::kernel::automata::label::ClockLabelClassic;
use ccsl::lccsl::format::render;
use ccsl::lccsl::parser::Specification;
use ccsl::optimization::optimize_by_min_front_init_weights;
use std::fs::read_to_string;
use std::path::PathBuf;
use structopt::StructOpt;
use tools::file_or_stdout;

#[derive(StructOpt, Debug)]
#[structopt(name = "lccsl-fmt", about = "LightCCSL specification formatter")]
struct App {
    file: PathBuf,
    out: Option<PathBuf>,
}
/// Applies sorting to a LightCCSL specification
fn main() -> Result<()> {
    let app: App = App::from_args();

    let mut spec: Specification<String> = read_to_string(&app.file)?.as_str().try_into()?;
    let mut perm =
        optimize_by_min_front_init_weights::<_, ClockLabelClassic<String>>(&spec.constraints);
    perm.apply_slice_in_place(&mut spec.constraints);
    write!(
        file_or_stdout(&app.out)?,
        "{}",
        &render(&spec.constraints, &spec.name)
    )?;

    Ok(())
}
