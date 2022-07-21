use std::path::{Path, PathBuf};
use tool::{all_constraints, vec_into_vec, write_graph_no_label};

use ccsl::lccsl::algo::generate_combinations;
use ccsl::lccsl::automata::label::StaticBitmapLabel;
use ccsl::lccsl::automata::STS;
use ccsl::lccsl::constraints::{Constraint, Precedence, Subclocking};
use ccsl::lccsl::vizualization::unfold_specification;
use itertools::Itertools;
use permutation::Permutation;
use std::error::Error;
use structopt::StructOpt;

#[derive(StructOpt, Debug)]
#[structopt(name = "basic", about = "Processing of LightCCSL constraints")]
struct Opt {
    dir: PathBuf,
}

fn main() -> Result<(), Box<dyn Error>> {
    let opt: Opt = Opt::from_args();
    all_constraints(&opt.dir)?;

    let subclock_spec: Vec<Constraint<u32>> = vec![
        Subclocking { left: 0, right: 1 }.into(),
        Subclocking { left: 1, right: 2 }.into(),
        Subclocking { left: 2, right: 3 }.into(),
    ];
    let prec_spec: Vec<Constraint<u32>> = vec![
        Precedence {
            left: 0,
            right: 1,
            init: None,
            max: None,
        }
        .into(),
        Precedence {
            left: 1,
            right: 2,
            init: None,
            max: None,
        }
        .into(),
        Precedence {
            left: 2,
            right: 3,
            init: None,
            max: None,
        }
        .into(),
    ];
    let subclock_spec: Vec<STS<u32, StaticBitmapLabel>> = vec_into_vec(subclock_spec.iter());
    unfold(subclock_spec, &opt.dir.join("subclock"))?;
    let prec_spec: Vec<STS<u32, StaticBitmapLabel>> = vec_into_vec(prec_spec.iter());
    unfold(prec_spec, &opt.dir.join("precedence"))?;

    Ok(())
}

fn unfold(spec: Vec<STS<u32, StaticBitmapLabel>>, dir: &Path) -> Result<(), Box<dyn Error>> {
    let len = spec.len();
    for (i, perm) in (0..len).permutations(len).enumerate() {
        let spec_perm = perm.iter().map(|i| spec[*i as usize].clone()).collect_vec();
        let anti_perm = Permutation::from_vec(perm).inverse();
        for comb in generate_combinations(&spec_perm) {
            let orig_comb = anti_perm.apply_slice(comb.as_slice());
            write_graph_no_label(
                &unfold_specification(&spec_perm, &comb, true),
                dir,
                &format!("{}_({})", i, orig_comb.into_iter().map(|s| s.0).join(",")),
            )?;
        }
    }
    Ok(())
}
