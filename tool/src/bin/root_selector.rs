use ccsl::lccsl::automata::label::StaticBitmapLabel;
use ccsl::lccsl::automata::STS;
use ccsl::lccsl::generation::random_connected_specification;
use ccsl::lccsl::optimization::{
    optimize_by_min_front_init_weights, optimize_by_min_front_init_weights_root,
};
use itertools::Itertools;
use std::error::Error;
use structopt::StructOpt;
use tool::{analyze_specification, vec_into_vec};

#[derive(StructOpt, Debug)]
#[structopt(name = "basic", about = "Resolver for specification identifiers")]
struct Opt {
    #[structopt(short, long)]
    spec: u64,
    #[structopt(short, long)]
    size: usize,
}

type L = StaticBitmapLabel;

fn main() -> Result<(), Box<dyn Error>> {
    let opt: Opt = Opt::from_args();
    let spec = random_connected_specification(opt.spec, opt.size, true)
        .into_iter()
        .map(|c| c.map(&mut |clock| *clock as u32))
        .collect_vec();

    let best_opti = (0..opt.size as u8)
        .permutations(opt.size)
        .map(|perm_vec| {
            let perm_id = lehmer::Lehmer::from_permutation(&perm_vec).to_decimal();
            let perm = perm_vec
                .iter()
                .map(|i| spec[*i as usize].clone())
                .collect_vec();
            (perm, perm_vec, perm_id as u64)
        })
        .map(|(spec, perm_vec, perm_id)| {
            let opti_spec_perm =
                optimize_by_min_front_init_weights_root::<u32, L>(spec.as_slice(), 0);
            let new_perm_id =
                lehmer::Lehmer::from_permutation(&opti_spec_perm.apply_slice(perm_vec))
                    .to_decimal();
            let opti_spec = opti_spec_perm.apply_slice(spec.as_slice());
            let opti_spec: Vec<STS<u32, L>> = vec_into_vec(&opti_spec);
            let (analysis, _) = analyze_specification(&opti_spec, opt.spec, new_perm_id as u64);
            (
                perm_id,
                analysis.into_iter().max_by_key(|p| p.real.test).unwrap(),
            )
        })
        .min_by_key(|(_, p)| p.real.test);
    let best = (0..opt.size as u8)
        .permutations(opt.size)
        .map(|perm_vec| {
            let perm_id = lehmer::Lehmer::from_permutation(&perm_vec).to_decimal();
            let perm = perm_vec
                .into_iter()
                .map(|i| spec[i as usize].clone())
                .collect_vec();

            let spec: Vec<STS<u32, L>> = vec_into_vec(&perm);
            let (analysis, _) = analyze_specification(&spec, opt.spec, perm_id as u64);
            (
                perm_id,
                *analysis.iter().max_by_key(|p| p.real.test).unwrap(),
                *analysis.iter().max_by_key(|p| p.real.down).unwrap(),
            )
        })
        .min_by_key(|(_, p1, p2)| (p1.real.test, p2.real.down));
    let opti = {
        let opti_perm = optimize_by_min_front_init_weights::<_, L>(&spec);
        println!("{:?}", opti_perm);
        let opti = opti_perm.apply_slice(spec.as_slice());
        let perm_id = lehmer::Lehmer::from_permutation(
            &opti_perm.apply_slice((0..opti_perm.len() as u8).collect_vec()),
        )
        .to_decimal();
        let opti: Vec<STS<u32, L>> = vec_into_vec(&opti);
        let (analysis, _) = analyze_specification(&opti, opt.spec, perm_id as u64);
        println!("optimal:\n{}", opti.iter().join(";\n"));
        (
            *analysis.iter().max_by_key(|p| p.real.test).unwrap(),
            *analysis.iter().max_by_key(|p| p.real.down).unwrap(),
        )
    };
    println!(
        "best opti:{:?}\nthe best:{:?}\noptimal:{:?}",
        best_opti, best, opti
    );
    let spec: Vec<STS<u32, L>> = vec_into_vec(&spec);
    println!("spec\n{}", spec.iter().join(";\n"));

    Ok(())
}
