use anyhow::Result;
use chrono::Duration;
use regex::Regex;
use serde::{Deserialize, Serialize};
use std::path::{Path, PathBuf};
use std::process::Command;
use std::str::from_utf8;
use structopt::lazy_static::lazy_static;
use structopt::StructOpt;
use walkdir::WalkDir;

#[derive(StructOpt, Debug)]
#[structopt(
    name = "stats",
    about = "Compiles LightCCSL specifications into NBAC format"
)]
struct App {
    #[structopt(subcommand)]
    cmd: Cmd,
}
#[derive(StructOpt, Debug)]
enum Cmd {
    Gen { dir: PathBuf, csv: PathBuf },
    Plot { csv: PathBuf },
}

fn main() -> Result<()> {
    let app: App = App::from_args();

    match app.cmd {
        Cmd::Gen { dir, csv } => gen(&dir, &csv),
        Cmd::Plot { csv } => plot(&csv),
    }
}

#[derive(Serialize, Deserialize, Debug)]
struct DataEntry {
    filename: String,
    mg_out: bool,
    nbac_out: bool,
    // mg_time: Duration,
    // nbac_time: Duration,
}

fn gen(dir: &Path, out: &Path) -> Result<()> {
    let mut wrt = csv::Writer::from_path(out)?;
    let mut errs = csv::Writer::from_path("/home/ptokarie/code/ccsl-rs/examples/err.csv")?;
    for entry in WalkDir::new(dir).into_iter().filter_map(|e| e.ok()) {
        if !entry.file_type().is_file() {
            continue;
        }
        let filename = entry
            .path()
            .file_name()
            .unwrap()
            .to_str()
            .unwrap()
            .to_string();
        if !filename.ends_with(".lc") {
            continue;
        }
        let nbac_file = entry.path().with_extension("lc.nbac");
        let output = Command::new("/home/ptokarie/code/ccsl-rs/target/release/ccsl2nbac")
            .arg(entry.path())
            .arg(&nbac_file)
            .output()?;
        if !output.status.success() {
            errs.write_record(&[
                &filename,
                from_utf8(&output.stdout).unwrap(),
                from_utf8(&output.stderr).unwrap(),
            ])?;
            continue;
        }
        let output = Command::new("/home/ptokarie/Applications/nbac/nbacg.opt")
            .env("LD_LIBRARY_PATH", "/home/ptokarie/Applications/nbac/")
            .arg(&nbac_file)
            .output()?;
        if !output.stderr.is_empty() {
            errs.write_record(&[
                &filename,
                from_utf8(&output.stdout).unwrap(),
                from_utf8(&output.stderr).unwrap(),
            ])?;
            continue;
        }
        let text = from_utf8(&output.stdout).unwrap();
        lazy_static! {
            static ref RE_NBAC: Regex = Regex::new("(SUCCESS)|(DON'T KNOW)").unwrap();
            static ref RE_MG: Regex = Regex::new("(true)|(false)").unwrap();
        }
        let nbac_out = match RE_NBAC.find(text).map(|m| m.as_str()).unwrap_or("") {
            "SUCCESS" => true,
            "DON'T KNOW" => false,
            _ => {
                println!("{}", nbac_file.display());
                println!("{}", text);
                println!("{}", from_utf8(&output.stderr).unwrap());
                panic!("shouldn't reach")
            }
        };

        let output = Command::new("java")
            .arg("-jar")
            .arg("/home/ptokarie/code/soa_bound/out/artifacts/soa_bound_jar/soa_bound.jar")
            .arg(entry.path())
            .output()?;
        let text = from_utf8(&output.stdout).unwrap();
        let mg_out = match RE_MG.find(text).unwrap().as_str() {
            "true" => true,
            "false" => false,
            _ => panic!("shouldn't reach"),
        };

        wrt.serialize(DataEntry {
            filename,
            mg_out,
            nbac_out,
            // mg_time: (),
            // nbac_time: (),
        })?;
    }
    wrt.flush()?;

    Ok(())
}
fn plot(csv: &Path) -> Result<()> {
    Ok(())
}
