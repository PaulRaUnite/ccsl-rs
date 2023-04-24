use anyhow::Result;
use ccsl::dot::render_dot;
use ccsl::lccsl::parser::Specification;
use std::io::read_to_string;
use std::path::PathBuf;
use structopt::StructOpt;
use tools::{file_or_stdin, file_or_stdout};

#[derive(StructOpt, Debug)]
#[structopt(
    name = "lccsl2dot",
    about = "Translates LightCCSL specification into graphviz DOT format"
)]
struct App {
    file: Option<PathBuf>,
    out: Option<PathBuf>,
}

/// Translates LightCCSL specifications into graphviz DOT format
fn main() -> Result<()> {
    let app: App = App::from_args();

    let spec: Specification<String> = read_to_string(file_or_stdin(&app.file)?)?
        .as_str()
        .try_into()?;
    render_dot(
        &mut file_or_stdout(&app.out)?,
        &spec.name,
        &spec.constraints,
    )?;

    Ok(())
}
