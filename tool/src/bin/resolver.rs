use ccsl::lccsl::algo::approx_conflict_map;
use ccsl::lccsl::automata::{StaticBitmapLabel, STS};
use ccsl::lccsl::gen::{star, to_precedence_spec, to_subclocking_spec, TreeIterator};
use itertools::Itertools;
use std::error::Error;
use std::path::Path;
use structopt::StructOpt;
use tool::{
    decode_comb, decode_perm, decode_spec, gen_spec, gen_spec_flat, vec_into_vec,
    write_graph_indexed,
};

#[derive(StructOpt, Debug)]
#[structopt(name = "basic", about = "Resolver for specification identifiers")]
struct Opt {
    #[structopt(short, long)]
    spec: u64,
    #[structopt(short, long)]
    perm: u64,
    #[structopt(short, long)]
    comb: u64,
}

type C = u32;
type L = StaticBitmapLabel;

fn main() -> Result<(), Box<dyn Error>> {
    let opt: Opt = Opt::from_args();
    let gen = gen_spec_flat(3..=6, |size| {
        TreeIterator::new(size + 1).map(|tr| {
            to_subclocking_spec(&tr)
                .into_iter()
                .map(|c| c.map(&mut |x| *x as u32))
                .collect_vec()
        })
    });

    let spec_id = opt.spec;
    let perm_id = opt.perm;
    let comb_id = opt.comb;

    let dir = Path::new("/home/paulra/Code/ccsl-rs/tool/dot/resolver/");

    let spec = decode_spec(gen, spec_id).unwrap();
    let perm = decode_perm::<C>(&spec, perm_id).unwrap();
    let comb = decode_comb::<C, L>(&perm, comb_id).unwrap();

    let spec: Vec<STS<u32>> = vec_into_vec(&perm);
    let spec = spec.into_iter().map(|c| c.squish()).collect_vec();
    let conflicts = approx_conflict_map(&spec, &spec.iter().map(|c| c.initial()).collect_vec());

    write_graph_indexed(&conflicts, dir, "conflict")?;

    Ok(())
}
