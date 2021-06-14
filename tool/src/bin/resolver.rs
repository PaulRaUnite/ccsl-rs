use ccsl::lccsl::algo::approx_conflict_map;
use ccsl::lccsl::automata::{StaticBitmapLabel, STS};
use ccsl::lccsl::gen::random_connected_specification;
use ccsl::lccsl::opti::squished_map;
use ccsl::lccsl::vizualization::unfold_specification;
use itertools::Itertools;
use petgraph::algo::min_spanning_tree;
use petgraph::data::FromElements;
use petgraph::graph::UnGraph;
use std::error::Error;
use std::path::Path;
use structopt::StructOpt;
use tool::{
    analyze_specification, decode_comb, decode_perm, vec_into_vec, write_graph_indexed,
    write_graph_indexed_debug,
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
    #[structopt(short, long)]
    size: Option<usize>,
}

type C = u32;
type L = StaticBitmapLabel;

fn main() -> Result<(), Box<dyn Error>> {
    let opt: Opt = Opt::from_args();

    let spec_id = opt.spec;
    let perm_id = opt.perm;
    let comb_id = opt.comb;

    let dir = Path::new("/home/paulra/Code/ccsl-rs/tool/dot/resolver/");

    let spec = random_connected_specification(opt.spec, opt.size.unwrap())
        .into_iter()
        .map(|c| c.map(&mut |clock| *clock as u32))
        .collect_vec();
    //decode_spec(gen, spec_id).unwrap();
    let perm = decode_perm::<C>(&spec, perm_id).unwrap();
    let _comb = decode_comb::<C, L>(&perm, comb_id).unwrap();

    let spec: Vec<STS<u32, L>> = vec_into_vec(&perm);
    println!("{}", spec.iter().join(";\n"));

    write_graph_indexed_debug(
        &unfold_specification(&spec, &spec.iter().map(|c| c.initial()).collect_vec(), true),
        dir,
        "solution_tree",
    )?;

    let spec = spec.into_iter().map(|c| c.squish()).collect_vec();
    let conflicts = approx_conflict_map(&spec, &spec.iter().map(|c| c.initial()).collect_vec());

    write_graph_indexed(&conflicts, dir, "conflict")?;
    let undirect_conflicts = squished_map::<_, L>(&perm);
    write_graph_indexed_debug(&undirect_conflicts, dir, "undirected_conflict")?;
    let tree = UnGraph::from_elements(min_spanning_tree(&squished_map::<_, L>(&perm)));
    write_graph_indexed_debug(&tree, dir, "tree")?;

    let (analysis, _) = analyze_specification(&spec, spec_id, perm_id);
    println!("{:?}", analysis.into_iter().max_by_key(|p| p.real.test));
    Ok(())
}
