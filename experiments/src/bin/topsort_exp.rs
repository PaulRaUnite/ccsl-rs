use anyhow::Result;
use std::fs::{create_dir, create_dir_all, remove_dir_all, File};
use std::path::{Path, PathBuf};
use std::sync::Arc;

use itertools::Itertools;
use permutation::Permutation;
use polars::io::SerWriter;
use polars::prelude::{col, CsvWriter, DataType, Expr, IntoLazy, JoinType, LazyFrame};
use rand::prelude::SliceRandom;
use rand::rngs::StdRng;
use rand::{RngCore, SeedableRng};
use structopt::StructOpt;
use walkdir::WalkDir;

use anyhow::Context;
use ccsl::algo::generate_combinations;
use ccsl::generation::graph::{star, TreeIterator};
use ccsl::generation::specification::{
    cycle_with_tail_and_spike, random_connected_specification, to_precedence_spec,
    to_subclocking_spec,
};
use ccsl::kernel::automata::label::StaticBitmapLabel;
use ccsl::kernel::automata::STS;
use ccsl::kernel::constraints::{Causality, Constraint, Delay, Precedence, Union};
use ccsl::optimization::{
    optimize, optimize_by_min_front_init_weights, optimize_by_min_front_with_tricost_root,
    optimize_by_sort_weights, optimize_by_tree_depth, optimize_by_tree_width, order_by_min_front,
    order_via_dijkstra, root, root_by_min_outgoing, root_by_tricost,
};
use experiments::{
    analyze_specification, analyze_specification_combination, analyze_squish_specification,
    collection, gen_spec, gen_spec_flat, inverse_graph, stream_to_parquet, stream_to_parquet_flat,
    vec_into_vec,
};
#[derive(StructOpt, Debug)]
#[structopt(name = "basic", about = "Processing of LightCCSL constraints")]
struct Opt {
    #[structopt(subcommand)]
    cmd: Cmd,
}

#[derive(StructOpt, Debug)]
enum Cmd {
    Generate {
        /// Path to a directory to generate data
        dir: PathBuf,
    },
    Analyze {
        /// Path to a directory to read data
        dir: PathBuf,
    },
}
//type L = ClockLabelClassic<u32>;
//type L = RoaringBitmapLabel;
//type L = StaticBitmapLabel;
type L = StaticBitmapLabel;

fn main() -> Result<()> {
    let app: Opt = Opt::from_args();
    match app.cmd {
        Cmd::Generate { dir } => generate(&dir),
        Cmd::Analyze { dir } => analyze(&dir),
    }
}

fn generate(data_dir: &Path) -> Result<()> {
    analyse_specs(
        &data_dir.join("circle_tail"),
        gen_spec_flat(3..=4, |size| {
            (0..size).map(move |tail| cycle_with_tail_and_spike(size, tail, 0, 1))
        }),
    )?;
    analyse_specs(
        &data_dir.join("circle_spike"),
        gen_spec_flat(3..=4, |size| {
            (0..size).map(move |spike| cycle_with_tail_and_spike(size, 0, spike, 1))
        }),
    )?;
    analyse_specs(
        &data_dir.join("circle_spike_tail"),
        gen_spec_flat(3..=4, |size| {
            (1..size)
                .cartesian_product(1..size)
                .map(move |(tail, spike)| cycle_with_tail_and_spike(size, tail, spike, 1))
        }),
    )?;
    analyse_specs(
        &data_dir.join("cycle"),
        gen_spec(3..=8, |size| cycle_with_tail_and_spike(size, 0, 0, 0)),
    )?;
    analyse_specs(
        &data_dir.join("star/precedence"),
        gen_spec(3..=8, |size| to_precedence_spec(&star(size, 3).unwrap())),
    )?;
    analyse_specs(
        &data_dir.join("star/subclocking"),
        gen_spec(3..=8, |size| to_subclocking_spec(&star(size, 3).unwrap())),
    )?;
    analyse_specs(
        &data_dir.join("tree/precedence"),
        gen_spec_flat(3..=7, |size| {
            TreeIterator::new(size + 1).map(|tr| to_precedence_spec(&tr))
        }),
    )?;
    analyse_specs(
        &data_dir.join("tree/subclocking"),
        gen_spec_flat(3..=7, |size| {
            TreeIterator::new(size + 1).map(|tr| to_subclocking_spec(&tr))
        }),
    )?;
    analyse_specs(
        &data_dir.join("star/inverse/precedence"),
        gen_spec(3..=8, |size| {
            to_precedence_spec(&inverse_graph(star(size, 3).unwrap()))
        }),
    )?;
    analyse_specs(
        &data_dir.join("star/inverse/subclocking"),
        gen_spec(3..=8, |size| {
            to_subclocking_spec(&inverse_graph(star(size, 3).unwrap()))
        }),
    )?;
    analyse_specs(
        &data_dir.join("tree/inverse/precedence"),
        gen_spec_flat(3..=6, |size| {
            TreeIterator::new(size + 1).map(|tr| to_precedence_spec(&inverse_graph(tr)))
        }),
    )?;
    analyse_specs(
        &data_dir.join("tree/inverse/subclocking"),
        gen_spec_flat(3..=7, |size| {
            TreeIterator::new(size + 1).map(|tr| to_subclocking_spec(&inverse_graph(tr)))
        }),
    )?;
    let mut rng = StdRng::seed_from_u64(12345678);
    let mut fixed_specs = Vec::new();
    let mut unfixed_specs = Vec::new();
    for size in 4..=7 {
        for _ in 0..(800 - 70 * size) {
            let seed = rng.next_u64();
            fixed_specs.push((seed, random_connected_specification(seed, size, true)));
            if size < 6 {
                unfixed_specs.push((seed, random_connected_specification(seed, size, false)));
            }
        }
    }
    analyse_specs(&data_dir.join("random/fixed"), fixed_specs)?;
    analyse_specs(&data_dir.join("random/unfixed"), unfixed_specs)?;

    analyse_specs(&data_dir.join("scp15"), old_examples_specs().collect_vec())?;
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

fn analyse_specs<I>(dir: &Path, specs: I) -> Result<()>
where
    I: IntoIterator<Item = (u64, Vec<Constraint<usize>>)>,
    I::IntoIter: Clone,
{
    create_dir_all(dir)?;
    let specs = specs.into_iter();
    let (hint, _) = specs.size_hint();
    println!("specs >= {}", hint);

    let prepared_specs = specs
        .map(|(spec_id, s)| {
            let spec: Vec<Constraint<u32>> = s
                .into_iter()
                .map(|c| c.map(|clock| *clock as u32))
                .collect();
            if spec.len() > (u8::MAX as usize) {
                panic!("lehmer lib requires u8 size for permutation indexes");
            }
            (spec, spec_id)
        })
        .collect_vec();

    let permuted_specs = prepared_specs.iter().flat_map(|(spec, spec_id)| {
        let len = spec.len();
        let spec: Vec<STS<u32, L>> = vec_into_vec(spec);
        (0..len as u8).permutations(len).map(move |perm_vec| {
            let perm_id = lehmer::Lehmer::from_permutation(&perm_vec).to_decimal();
            let perm = Permutation::oneline(perm_vec.iter().map(|x| *x as usize).collect_vec());
            let permuted_spec = perm.apply_slice(&spec);
            (permuted_spec, *spec_id, perm_id as u64)
        })
    });
    let perm_comb_specs = permuted_specs
        .clone()
        .map(|(spec, spec_id, perm_id)| (Arc::new(spec), spec_id, perm_id))
        .flat_map(|(spec, spec_id, perm_id)| {
            generate_combinations(&spec)
                .enumerate()
                .map(|(comb_id, comb)| (spec.clone(), comb, spec_id, perm_id, comb_id as u64))
                .collect_vec()
        });
    all_optimizations_to_parquet(&dir.join("opti"), prepared_specs.iter())?;

    stream_to_parquet(
        &dir.join("main.parquet"),
        perm_comb_specs,
        |(spec, comb, spec_id, perm_id, comb_id)| {
            analyze_specification_combination(spec, &comb, *spec_id, *perm_id, *comb_id)
        },
        512,
    )?;

    stream_to_parquet(
        &dir.join("squished.parquet"),
        permuted_specs,
        |(spec, spec_id, perm_id)| analyze_squish_specification(spec, *spec_id, *perm_id),
        512,
    )?;
    Ok(())
}

fn all_optimizations_to_parquet<'a>(
    dir: &Path,
    prepared_specs: impl Iterator<Item = &'a (Vec<Constraint<u32>>, u64)> + Clone,
) -> Result<()> {
    create_dir_all(dir)?;
    let optimizers: Vec<(&str, Box<dyn Sync + Fn(&[Constraint<u32>]) -> Permutation>)> = vec![
        (
            "random",
            Box::new(|spec| {
                let mut perm = (0..spec.len()).collect_vec();
                perm.shuffle(&mut StdRng::from_entropy());
                Permutation::oneline(perm)
            }),
        ),
        (
            "sort_min_weights",
            Box::new(optimize_by_sort_weights::<u32, L>),
        ),
        (
            "min_out.tree_width",
            Box::new(optimize_by_tree_width::<u32, L>),
        ),
        (
            "min_out.tree_depth",
            Box::new(optimize_by_tree_depth::<u32, L>),
        ),
        (
            "random.min_front",
            Box::new(|spec| optimize::<_, L>(spec, &root::random, &order_by_min_front)),
        ),
        (
            "init_weights.min_front",
            Box::new(optimize_by_min_front_init_weights::<u32, L>),
        ),
        (
            "min_out.min_front",
            Box::new(|spec| optimize::<_, L>(spec, &root_by_min_outgoing, &order_by_min_front)),
        ),
        (
            "tricost.min_front",
            Box::new(optimize_by_min_front_with_tricost_root::<u32, L>),
        ),
        (
            "random.dijkstra",
            Box::new(|spec| optimize::<_, L>(spec, &root::random, &order_via_dijkstra)),
        ),
        (
            "min_out.dijkstra",
            Box::new(|spec| optimize::<_, L>(spec, &root_by_min_outgoing, &order_via_dijkstra)),
        ),
        (
            "init_weights.dijkstra",
            Box::new(|spec| optimize::<_, L>(spec, &root::weights_with_init, &order_via_dijkstra)),
        ),
        (
            "tricost.dijkstra",
            Box::new(|spec| optimize::<_, L>(spec, &root_by_tricost, &order_via_dijkstra)),
        ),
    ];

    for (name, opti) in optimizers {
        let opti = opti.as_ref();
        stream_to_parquet_flat(
            &dir.join(format!("{}.parquet", name)),
            prepared_specs.clone(),
            |(spec, spec_id)| {
                let opti_spec_perm: Permutation = opti(spec);
                assert!(opti_spec_perm.valid());
                let opti_spec = opti_spec_perm.apply_slice(spec.as_slice());
                let perm_id = lehmer::Lehmer::from_permutation(
                    &opti_spec_perm.apply_slice((0..spec.len() as u8).collect_vec().as_slice()),
                )
                .to_decimal() as u64;
                let opti_spec: Vec<STS<u32, L>> = vec_into_vec(&opti_spec);
                let (analysis, _) = analyze_specification(&opti_spec, *spec_id, perm_id);
                analysis
            },
            48,
            512,
        )?;
    }
    Ok(())
}

fn old_examples_specs() -> impl Iterator<Item = (u64, Vec<Constraint<usize>>)> {
    vec![
        vec![
            //alt.lc
            Delay {
                out: 1,
                trigger: 0,
                delay: 1,
                on: None,
            }
            .into(),
            Precedence {
                cause: 0,
                effect: 2,
                init: None,
                max: None,
            }
            .into(),
            Precedence {
                cause: 2,
                effect: 1,
                init: None,
                max: None,
            }
            .into(),
        ],
        // vec![
        //     // scp15v1
        //     Causality {
        //         left: 0,
        //         right: 2,
        //         init: None,
        //         max: None,
        //     }
        //     .into(),
        //     Precedence {
        //         left: 2,
        //         right: 4,
        //         init: None,
        //         max: None,
        //     }
        //     .into(),
        //     Causality {
        //         left: 4,
        //         right: 5,
        //         init: None,
        //         max: None,
        //     }
        //     .into(),
        //     Causality {
        //         left: 1,
        //         right: 3,
        //         init: None,
        //         max: None,
        //     }
        //     .into(),
        //     Precedence {
        //         left: 3,
        //         right: 4,
        //         init: None,
        //         max: None,
        //     }
        //     .into(),
        //     Infinity {
        //         out: 6,
        //         args: collection! {0,1},
        //     }
        //     .into(),
        //     Precedence {
        //         left: 3,
        //         right: 4,
        //         init: None,
        //         max: Some(1),
        //     }
        //     .into(),
        // ],
        // vec![
        //     // scp15-v1b
        //     Precedence {
        //         left: 0,
        //         right: 2,
        //         init: None,
        //         max: None,
        //     }
        //     .into(),
        //     Causality {
        //         left: 2,
        //         right: 3,
        //         init: None,
        //         max: None,
        //     }
        //     .into(),
        //     Precedence {
        //         left: 1,
        //         right: 2,
        //         init: None,
        //         max: None,
        //     }
        //     .into(),
        //     Infinity {
        //         out: 4,
        //         args: collection! {0,1},
        //     }
        //     .into(),
        //     Precedence {
        //         left: 4,
        //         right: 3,
        //         init: None,
        //         max: Some(1),
        //     }
        //     .into(),
        // ],
        // vec![
        //     // scp15v2
        //     Causality {
        //         left: 0,
        //         right: 2,
        //         init: None,
        //         max: None,
        //     }
        //     .into(),
        //     Precedence {
        //         left: 2,
        //         right: 4,
        //         init: None,
        //         max: None,
        //     }
        //     .into(),
        //     Causality {
        //         left: 4,
        //         right: 5,
        //         init: None,
        //         max: None,
        //     }
        //     .into(),
        //     Causality {
        //         left: 1,
        //         right: 3,
        //         init: None,
        //         max: None,
        //     }
        //     .into(),
        //     Precedence {
        //         left: 3,
        //         right: 4,
        //         init: None,
        //         max: None,
        //     }
        //     .into(),
        //     Supremum {
        //         out: 6,
        //         args: collection! {0,1},
        //     }
        //     .into(),
        //     Precedence {
        //         left: 3,
        //         right: 4,
        //         init: None,
        //         max: Some(1),
        //     }
        //     .into(),
        // ],
        // vec![
        //     // scp15-v2b
        //     Precedence {
        //         left: 0,
        //         right: 2,
        //         init: None,
        //         max: None,
        //     }
        //     .into(),
        //     Causality {
        //         left: 2,
        //         right: 3,
        //         init: None,
        //         max: None,
        //     }
        //     .into(),
        //     Precedence {
        //         left: 1,
        //         right: 2,
        //         init: None,
        //         max: None,
        //     }
        //     .into(),
        //     Supremum {
        //         out: 4,
        //         args: collection! {0,1},
        //     }
        //     .into(),
        //     Precedence {
        //         left: 4,
        //         right: 3,
        //         init: None,
        //         max: Some(1),
        //     }
        //     .into(),
        // ],
        vec![
            // scp15v3
            Causality {
                cause: 0,
                effect: 2,
                init: None,
                max: None,
            }
            .into(),
            Precedence {
                cause: 2,
                effect: 4,
                init: None,
                max: None,
            }
            .into(),
            Causality {
                cause: 4,
                effect: 5,
                init: None,
                max: None,
            }
            .into(),
            Causality {
                cause: 1,
                effect: 3,
                init: None,
                max: None,
            }
            .into(),
            Precedence {
                cause: 3,
                effect: 4,
                init: None,
                max: None,
            }
            .into(),
            Union {
                out: 6,
                args: collection! {0,1},
            }
            .into(),
            Precedence {
                cause: 3,
                effect: 4,
                init: None,
                max: Some(1),
            }
            .into(),
        ],
        vec![
            // scp15-v3b
            Precedence {
                cause: 0,
                effect: 2,
                init: None,
                max: None,
            }
            .into(),
            Causality {
                cause: 2,
                effect: 3,
                init: None,
                max: None,
            }
            .into(),
            Precedence {
                cause: 1,
                effect: 2,
                init: None,
                max: None,
            }
            .into(),
            Union {
                out: 4,
                args: collection! {0,1},
            }
            .into(),
            Precedence {
                cause: 4,
                effect: 3,
                init: None,
                max: Some(1),
            }
            .into(),
        ],
    ]
    .into_iter()
    .enumerate()
    .map(|(i, v)| (i as u64, v))
}

fn analyze(dir: &Path) -> Result<()> {
    let reference = LazyFrame::scan_parquet(
        dir.join("main.parquet").to_str().unwrap().to_string(),
        Default::default(),
    )?
    .groupby([col("spec"), col("variant")])
    .agg([col("real_tests").max(), col("clocks").max()])
    .collect()?;
    let squished_diff = LazyFrame::scan_parquet(
        dir.join("squished.parquet").to_str().unwrap().to_string(),
        Default::default(),
    )?
    .join(
        reference.clone().lazy(),
        [col("spec"), col("variant")],
        [col("spec"), col("variant")],
        JoinType::Inner,
    )
    .select([
        col("spec"),
        col("variant"),
        col("clocks"),
        col("approx_tests"),
        col("limit_tests"),
        (col("approx_tests").cast(DataType::Int64) - col("real_tests")).alias("approx_diff"),
        (col("limit_tests").cast(DataType::Int64) - col("real_tests")).alias("limit_diff"),
    ])
    .collect()?;
    let mut squished_stats_by_clocks = squished_diff
        .lazy()
        .groupby([col("clocks")])
        .agg([
            col("approx_diff").max().alias("approx_max"),
            col("approx_diff").min().alias("approx_min"),
            col("approx_diff").mean().alias("approx_mean"),
            col("limit_diff").max().alias("limit_max"),
            col("limit_diff").min().alias("limit_min"),
            col("limit_diff").mean().alias("limit_mean"),
        ])
        .collect()?;
    let output_dir = dir.join("csv");
    if output_dir.exists() {
        remove_dir_all(&output_dir)?;
    }
    create_dir(&output_dir)?;
    CsvWriter::new(File::create(output_dir.join("squished.csv"))?)
        .has_header(true)
        .finish(&mut squished_stats_by_clocks)?;

    let methods: Vec<(String, LazyFrame)> = WalkDir::new(dir.join("opti"))
        .into_iter()
        .filter_ok(|e| e.metadata().map(|m| !m.is_dir()).unwrap_or(true))
        .map(|e| -> Result<(String, LazyFrame)> {
            let entry = e?;
            let filepath = entry.path();
            let string = filepath.to_str().unwrap().to_string();
            Ok((
                filepath.file_name().unwrap().to_str().unwrap().to_string(),
                LazyFrame::scan_parquet(string.clone(), Default::default())
                    .with_context(|| format!("File {}", &string))?,
            ))
        })
        .collect::<Result<Vec<_>>>()?;

    for (filename, df) in methods {
        let mut df = df
            .groupby([col("spec")])
            .agg([col("real_tests").max().alias("optimized")])
            .join(
                reference.clone().lazy().groupby([col("spec")]).agg([
                    col("real_tests").max().alias("max"),
                    col("real_tests").mean().alias("mean"),
                    col("real_tests").min().alias("min"),
                ]),
                [col("spec")],
                [col("spec")],
                JoinType::Left,
            )
            .select([
                col("spec"),
                ((col("mean") - col("optimized")) / (col("max") - col("min")) * Expr::from(100.0))
                    .alias("gain"),
            ])
            .collect()?;
        let mut filename = output_dir.join(filename);
        filename.set_extension("csv");
        CsvWriter::new(File::create(&filename)?)
            .has_header(true)
            .finish(&mut df)?;
    }
    Ok(())
}
