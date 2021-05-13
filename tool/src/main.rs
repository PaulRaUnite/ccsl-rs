extern crate arrow;
extern crate csv;
extern crate itertools;
extern crate parquet;

use std::error::Error;
use std::path::PathBuf;

use structopt::StructOpt;

use arrow::datatypes::Schema;
use arrow::record_batch::RecordBatch;
use ccsl::lccsl::automata::STS;
use ccsl::lccsl::gen::{
    circle_spec, star, to_precedence_spec, to_subclocking_spec, tree, TreeIterator,
};
use ccsl::lccsl::opti::optimize_spec;
use itertools::Itertools;
use parquet::arrow::ArrowWriter;
use parquet::basic;
use parquet::basic::Compression;
use parquet::basic::{IntType, LogicalType, Repetition};
use parquet::column::writer::ColumnWriter;
use parquet::file::properties::WriterProperties;
use parquet::file::writer::{FileWriter, SerializedFileWriter, TryClone};
use parquet::schema::types::{Type, TypePtr};
use serde::Serialize;
use std::borrow::BorrowMut;
use std::fs::File;
use std::io::BufWriter;
use std::sync::Arc;
use tool::{
    analyze_specification, hash, hash_spec, write_graph_no_label, SpecCombParams, SquishedParams,
};

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

    let spec_comb_schema = Arc::new(SpecCombParams::schema());
    let squished_schema = Arc::new(SquishedParams::schema());
    {
        let mut main_parquet_wrt = ArrowWriter::try_new(
            File::create("/home/paulra/Code/ccsl-rs/plotter/data.parquet")?,
            spec_comb_schema.clone(),
            Some(
                WriterProperties::builder()
                    .set_dictionary_enabled(false)
                    .set_compression(Compression::SNAPPY)
                    .build(),
            ),
        )?;
        let mut squished_parquet_wrt = ArrowWriter::try_new(
            File::create("/home/paulra/Code/ccsl-rs/plotter/squished.parquet")?,
            squished_schema.clone(),
            Some(
                WriterProperties::builder()
                    .set_dictionary_enabled(false)
                    .set_compression(Compression::SNAPPY)
                    .build(),
            ),
        )?;
        let mut optimized_parquet_wrt = ArrowWriter::try_new(
            File::create("/home/paulra/Code/ccsl-rs/plotter/optimized.parquet")?,
            spec_comb_schema.clone(),
            Some(
                WriterProperties::builder()
                    .set_dictionary_enabled(false)
                    .set_compression(Compression::SNAPPY)
                    .build(),
            ),
        )?;

        analysis_test_refactor(
            spec_comb_schema,
            &squished_schema,
            &mut main_parquet_wrt,
            &mut squished_parquet_wrt,
            &mut optimized_parquet_wrt,
        )?;

        main_parquet_wrt.close()?;
        squished_parquet_wrt.close()?;
        optimized_parquet_wrt.close()?;
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

fn analysis_test_refactor(
    spec_comb_schema: Arc<Schema>,
    squished_schema: &Arc<Schema>,
    main_parquet_wrt: &mut ArrowWriter<File>,
    squished_parquet_wrt: &mut ArrowWriter<File>,
    optimized_parquet_wrt: &mut ArrowWriter<File>,
) -> Result<(), Box<dyn Error>> {
    let mut squished_buffer = Vec::with_capacity(1024);
    let gen_range = 3..=7;
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
            {
                let batch = SpecCombParams::batch(spec_comb_schema.clone(), analysis)?;
                optimized_parquet_wrt.write(&batch)?;
            }
        }
        for perm in spec.into_iter().permutations(len) {
            let hashes = (&perm).into_iter().map(|c| hash(c)).collect_vec();

            let perm: Vec<STS<_>> = perm.into_iter().map(Into::into).collect();
            let (analysis, squished) = analyze_specification(perm, orig_hash, &hashes)?;
            {
                let batch = SpecCombParams::batch(spec_comb_schema.clone(), analysis)?;
                main_parquet_wrt.write(&batch)?;
            }

            squished_buffer.push(squished);
            if squished_buffer.len() == squished_buffer.capacity() {
                squished_parquet_wrt.write(&SquishedParams::batch(
                    squished_schema.clone(),
                    &squished_buffer,
                )?)?;
                squished_buffer.clear();
            }
        }
    }
    squished_parquet_wrt.write(&SquishedParams::batch(
        squished_schema.clone(),
        &squished_buffer,
    )?)?;
    Ok(())
}
