use anyhow::Result;
use ccsl::dot::render_dot;
use ccsl::lccsl::parser::parse_to_string;
use std::fs::read_to_string;
use std::path::PathBuf;
use structopt::StructOpt;
use tools::file_or_stdout;

#[derive(StructOpt, Debug)]
#[structopt(
    name = "lccsl-gen",
    about = "Translates LightCCSL specification into graphviz DOT format"
)]
struct App {
    file: PathBuf,
    out: Option<PathBuf>,
}

/// Translates LightCCSL specifications into graphviz DOT format
fn main() -> Result<()> {
    let app: App = App::from_args();

    let spec = parse_to_string(&read_to_string(&app.file)?)?;
    render_dot(
        &mut file_or_stdout(&app.out)?,
        &spec.name,
        &spec.constraints,
    )?;

    Ok(())
}
