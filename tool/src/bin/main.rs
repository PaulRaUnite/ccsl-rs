extern crate arrow;
extern crate csv;
extern crate itertools;
extern crate parquet;
extern crate rand;

use std::error::Error;
use std::path::{Path, PathBuf};

use structopt::StructOpt;

use ccsl::lccsl::automata::{
    ClockLabelClassic, DynBitmapLabel, RoaringBitmapLabel, STSBuilder, StaticBitmapLabel, STS,
};
use ccsl::lccsl::constraints::{Constraint, Specification};
use ccsl::lccsl::gen::{
    circle_spec, random_specification, star, to_precedence_spec, to_subclocking_spec, TreeIterator,
};
use ccsl::lccsl::opti::{optimize_by_tree_depth, optimize_by_tree_width, optimize_spec_by_sort};
use itertools::Itertools;
use parquet::arrow::ArrowWriter;
use rand::{RngCore, SeedableRng};
use std::fs::File;
use std::sync::Arc;
use tool::{
    analyze_specification, gen_spec, gen_spec_flat, inverse_graph, vec_into_vec,
    write_graph_no_label, SpecCombParams, SquishedParams,
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

//type L = ClockLabelClassic<u32>;
//type L = RoaringBitmapLabel;
//type L = StaticBitmapLabel;
type L = DynBitmapLabel;

fn main() -> Result<(), Box<dyn Error>> {
    let opt: Opt = Opt::from_args();
    // all_constraints(&opt.dir.join("constraints"))?;

    let g = star(5, 3).unwrap();
    write_graph_no_label(&g, &opt.dir.join("gen"), "star")?;
    println!("{:?}", to_precedence_spec(&g));
    println!("{:?}", to_subclocking_spec(&g));

    for (i, g) in TreeIterator::new(9).enumerate() {
        write_graph_no_label(&g, &opt.dir.join("gen/test"), &i.to_string())?;
    }

    let data_dir = Path::new("/home/paulra/Code/ccsl-rs/plotter/data/");
    analyse_specs(
        data_dir.join("circle"),
        gen_spec(3..=6, |size| circle_spec(size).unwrap()),
    )?;
    analyse_specs(
        data_dir.join("star/precedence"),
        gen_spec(3..=6, |size| to_precedence_spec(&star(size, 3).unwrap())),
    )?;
    analyse_specs(
        data_dir.join("star/subclocking"),
        gen_spec(3..=6, |size| to_subclocking_spec(&star(size, 3).unwrap())),
    )?;
    analyse_specs(
        data_dir.join("tree/precedence"),
        gen_spec_flat(3..=6, |size| {
            TreeIterator::new(size + 1).map(|tr| to_precedence_spec(&tr))
        }),
    )?;
    analyse_specs(
        data_dir.join("tree/subclocking"),
        gen_spec_flat(3..=6, |size| {
            TreeIterator::new(size + 1).map(|tr| to_subclocking_spec(&tr))
        }),
    )?;
    analyse_specs(
        data_dir.join("star/inverse/precedence"),
        gen_spec(3..=6, |size| {
            to_precedence_spec(&inverse_graph(star(size, 3).unwrap()))
        }),
    )?;
    analyse_specs(
        data_dir.join("star/inverse/subclocking"),
        gen_spec(3..=6, |size| {
            to_subclocking_spec(&inverse_graph(star(size, 3).unwrap()))
        }),
    )?;
    analyse_specs(
        data_dir.join("tree/inverse/precedence"),
        gen_spec_flat(3..=6, |size| {
            TreeIterator::new(size + 1).map(|tr| to_precedence_spec(&inverse_graph(tr)))
        }),
    )?;
    analyse_specs(
        data_dir.join("tree/inverse/subclocking"),
        gen_spec_flat(3..=6, |size| {
            TreeIterator::new(size + 1).map(|tr| to_subclocking_spec(&inverse_graph(tr)))
        }),
    )?;
    let mut rng = rand::rngs::StdRng::from_entropy();
    let mut specs = Vec::new();
    for size in 3..=6 {
        for _ in 0..(100 - 10 * size) {
            let seed = rng.next_u64();
            specs.push((seed, random_specification(seed, size)));
        }
    }
    analyse_specs(data_dir.join("random"), specs)?;
    // println!(
    //     "{:?}",
    //     TreeIterator::new(4)
    //         .map(|tr| {
    //             to_precedence_spec(&tr)
    //         })
    //         .collect_vec()[1]
    // );
    // for name in names {
    //     Command::new("dot")
    //         .arg("-O")
    //         .arg("-Tpng")
    //         .arg(opt.dir.join(name).join(".dot"))
    //         .output()?;
    // }
    Ok(())
}

fn analyse_specs(
    dir: impl AsRef<Path>,
    specs: impl IntoIterator<Item = (u64, Vec<Constraint<usize>>)>,
) -> Result<(), Box<dyn Error>> {
    let spec_comb_schema = Arc::new(SpecCombParams::schema());
    let squished_schema = Arc::new(SquishedParams::schema());

    std::fs::create_dir_all(dir.as_ref())?;
    let mut main_parquet_wrt = ArrowWriter::try_new(
        File::create(dir.as_ref().join("data.parquet"))?,
        spec_comb_schema.clone(),
        None,
    )?;
    let mut squished_parquet_wrt = ArrowWriter::try_new(
        File::create(dir.as_ref().join("squished.parquet"))?,
        squished_schema.clone(),
        None,
    )?;
    let mut optimized_parquet_wrt = ArrowWriter::try_new(
        File::create(dir.as_ref().join("optimized.parquet"))?,
        spec_comb_schema.clone(),
        None,
    )?;

    let mut optimized_buffer = Vec::with_capacity(8192);
    let mut main_buffer = Vec::with_capacity(8192);
    let mut squished_buffer = Vec::with_capacity(8192);
    for (spec_id, spec) in specs {
        let spec: Specification<usize> = spec.into();
        let spec: Vec<Constraint<u32>> = spec.map(&mut |clock| *clock as u32).into();
        let len = spec.len();
        if len > (u8::MAX as usize) {
            panic!("lehmer lib requires u8 size for permutation indexes");
        }
        let permutations_amount: usize = (1..=len).product();
        println!("step: {} ({})", len, permutations_amount);
        {
            let opti_spec_perm = optimize_by_tree_width::<u32, L>(&spec);
            let opti_spec = opti_spec_perm.apply_slice(spec.as_slice());
            let perm_id = lehmer::Lehmer::from_permutation(
                &opti_spec_perm.apply_slice(
                    (0..spec.len())
                        .into_iter()
                        .map(|i| i as u8)
                        .collect_vec()
                        .as_slice(),
                ),
            )
            .to_decimal() as u64;
            let opti_spec: Vec<STS<u32, L>> = vec_into_vec(&opti_spec);
            let (analysis, _) = analyze_specification(&opti_spec, spec_id, perm_id)?;
            optimized_buffer.extend(analysis);
            if optimized_buffer.len() >= 6144 {
                optimized_parquet_wrt.write(&SpecCombParams::batch(
                    spec_comb_schema.clone(),
                    &optimized_buffer,
                )?)?;
                optimized_buffer.clear();
            }
        }
        let spec: Vec<STS<u32, L>> = vec_into_vec(&spec);
        let mut perm = Vec::with_capacity(len);
        for perm_vec in (0..spec.len() as u8).permutations(len) {
            perm.clear();
            let perm_id = lehmer::Lehmer::from_permutation(&perm_vec).to_decimal();
            perm.extend(perm_vec.into_iter().map(|i| spec[i as usize].clone()));
            let (analysis, squished) = analyze_specification(&perm, spec_id, perm_id as u64)?;

            main_buffer.extend(analysis);
            if main_buffer.len() >= 6144 {
                main_parquet_wrt.write(&SpecCombParams::batch(
                    spec_comb_schema.clone(),
                    &main_buffer,
                )?)?;
                main_buffer.clear();
            }

            squished_buffer.push(squished);
            if squished_buffer.len() >= 6144 {
                squished_parquet_wrt.write(&SquishedParams::batch(
                    squished_schema.clone(),
                    &squished_buffer,
                )?)?;
                squished_buffer.clear();
            }
        }
    }

    optimized_parquet_wrt.write(&SpecCombParams::batch(
        spec_comb_schema.clone(),
        &optimized_buffer,
    )?)?;
    main_parquet_wrt.write(&SpecCombParams::batch(
        spec_comb_schema.clone(),
        &main_buffer,
    )?)?;
    squished_parquet_wrt.write(&SquishedParams::batch(
        squished_schema.clone(),
        &squished_buffer,
    )?)?;

    main_parquet_wrt.close()?;
    squished_parquet_wrt.close()?;
    optimized_parquet_wrt.close()?;
    Ok(())
}
