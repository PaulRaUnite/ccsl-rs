use ccsl::lccsl::algo::{squished_conflict_map, unidirect_squished_map};
use ccsl::lccsl::automata::label::StaticBitmapLabel;
use ccsl::lccsl::automata::STS;
use ccsl::lccsl::generation::random_connected_specification;
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
struct App {
    #[structopt(flatten)]
    opt: Opt,
    #[structopt(subcommand)]
    cmd: Cmd,
}

#[derive(StructOpt, Debug)]
struct Opt {
    #[structopt(short, long)]
    spec: u64,
    #[structopt(short, long)]
    comb: u64,
    #[structopt(short, long)]
    size: usize,
}

#[derive(StructOpt, Debug)]
enum Cmd {
    ID {
        #[structopt(short, long)]
        variant: u64,
    },
    Manual {
        perm: Vec<u8>,
    },
}

type C = u32;
type L = StaticBitmapLabel;

fn main() -> Result<(), Box<dyn Error>> {
    let app: App = App::from_args();

    let spec_id = app.opt.spec;
    let comb_id = app.opt.comb;
    let size = app.opt.size;

    let dir = Path::new("./graph/resolver/");

    let spec = random_connected_specification(spec_id, size, true)
        .into_iter()
        .map(|c| c.map(&mut |clock| *clock as u32))
        .collect_vec();
    //decode_spec(gen, spec_id).unwrap();
    let variant = match app.cmd {
        Cmd::ID { variant } => variant,
        Cmd::Manual { perm } => lehmer::Lehmer::from_permutation(&perm).to_decimal() as u64,
    };
    let perm = decode_perm::<C>(&spec, variant).unwrap();
    let spec: Vec<STS<u32, L>> = vec_into_vec(&perm);
    let comb = decode_comb::<C, L>(&spec, comb_id).unwrap();

    println!("{}", spec.iter().join(";\n"));

    write_graph_indexed_debug(
        &unfold_specification(&spec, &comb, true),
        dir,
        "solution_tree",
    )?;

    let (analysis, _) = analyze_specification(&spec, spec_id, variant);
    println!("{:?}", analysis.into_iter().max_by_key(|p| p.real.test));

    let conflicts = squished_conflict_map::<_, L>(&perm);
    write_graph_indexed(&conflicts, dir, "conflict")?;
    let undirect_conflicts = unidirect_squished_map::<_, L>(&perm);
    write_graph_indexed_debug(&undirect_conflicts, dir, "undirected_conflict")?;
    let tree = UnGraph::from_elements(min_spanning_tree(&unidirect_squished_map::<_, L>(&perm)));
    write_graph_indexed_debug(&tree, dir, "tree")?;

    Ok(())
}
