extern crate itertools;

use std::collections::HashMap;
use std::error::Error;
use std::fmt::Display;
use std::fs::{create_dir_all, File};
use std::io::BufWriter;
use std::io::Write;
use std::path::Path;

use ccsl::lccsl::algo::{
    approximate_complexity, combination_identifier, conflict_map, find_solutions,
    generate_combinations, CountingVisitor,
};
use ccsl::lccsl::automata::STS;
use ccsl::lccsl::constraints::{
    Alternates, Causality, Coincidence, Constraint, Delay, Exclusion, Intersection, Precedence,
    Subclocking, Union,
};
use ccsl::lccsl::vizualization::unfold_specification;
use itertools::Itertools;
use petgraph::dot::Dot;
use petgraph::Graph;
use std::collections::hash_map::DefaultHasher;
use std::fmt;
use std::hash::{Hash, Hasher};

fn write_graph<N: Display, E: Display>(
    g: &Graph<N, E>,
    dir: &Path,
    file: &str,
) -> Result<(), Box<dyn Error>> {
    create_dir_all(dir)?;
    let mut file = BufWriter::new(File::create(dir.join(file).with_extension("dot"))?);
    let dot = Dot::new(g);
    writeln!(&mut file, "{}", &dot)?;
    Ok(())
}

pub struct Analysis {}

fn hash(h: impl Hash) -> u64 {
    let mut hasher = DefaultHasher::new();
    h.hash(&mut hasher);
    hasher.finish()
}

pub fn analyze_specification<C, I>(dir: &Path, spec: I) -> Result<(), Box<dyn Error>>
where
    C: Hash + Clone + Ord + fmt::Display + fmt::Debug,
    I: IntoIterator<Item = Constraint<C>>,
    for<'a> &'a I: IntoIterator<Item = &'a Constraint<C>>,
{
    let hashes = (&spec)
        .into_iter()
        .map(|c: &Constraint<C>| hash(c))
        .collect_vec();

    let spec_hash = hash(&hashes);

    let dir = dir.join(spec_hash.to_string());
    let tree_full_dir = dir.join("tree/full");
    let tree_trimmed_dir = dir.join("tree/trimmed");
    let map_dir = dir.join("map");

    let spec: Vec<STS<C>> = spec.into_iter().map(Into::into).collect();

    println!("Comparison of combinations");
    for comb in generate_combinations(&spec) {
        let id = combination_identifier::<C, _>(&hashes, &comb).to_string();
        let full_tree = unfold_specification(&spec, &comb, true);
        write_graph(&full_tree, &tree_full_dir, &id)?;
        let trimmed_tree = unfold_specification(&spec, &comb, false);
        write_graph(&trimmed_tree, &tree_trimmed_dir, &id)?;
        let dep_map = conflict_map(&spec, &comb);
        write_graph(&dep_map, &map_dir, &id)?;
        let mut visitor = CountingVisitor::new();
        let actual = find_solutions(&spec, &comb, Some(&mut visitor));
        let approx = approximate_complexity(&dep_map);
        println!("{} {} <=> {}", actual, visitor, approx);
    }

    let squish_dir = dir.join("squish");
    let spec: Vec<STS<_>> = spec.into_iter().map(|c| c.squish()).collect();

    let comb: Vec<_> = spec
        .iter()
        .map(|c| c.states().iter().exactly_one())
        .collect::<Result<Vec<_>, _>>()
        .unwrap();
    let dep_map = conflict_map(&spec, &comb);
    write_graph(&dep_map, &squish_dir, "conflict_map")?;
    let mut visitor = CountingVisitor::new();
    let actual = find_solutions(&spec, &comb, Some(&mut visitor));
    let approx = approximate_complexity(&dep_map);
    println!("Averaged {} {} <=> {}", actual, visitor, approx);

    Ok(())
}

macro_rules! collection {
    // map-like
    ($($k:expr => $v:expr),* $(,)?) => {
        std::iter::Iterator::collect(std::array::IntoIter::new([$(($k, $v),)*]))
    };
    // set-like
    ($($v:expr),* $(,)?) => {
        std::iter::Iterator::collect(std::array::IntoIter::new([$($v,)*]))
    };
}

pub fn all_constraints(dir: &Path) -> Result<(), Box<dyn Error>> {
    let mut map: HashMap<&str, STS<&str>> = HashMap::new();
    map.insert(
        "coincidence",
        Coincidence {
            left: "a",
            right: "b",
        }
        .into(),
    );
    map.insert(
        "alternates",
        Alternates {
            left: "a",
            right: "b",
        }
        .into(),
    );
    map.insert(
        "causality",
        Causality {
            left: "a",
            right: "b",
            init: None,
            max: None,
        }
        .into(),
    );
    map.insert(
        "precedence",
        Precedence {
            left: "a",
            right: "b",
            init: None,
            max: None,
        }
        .into(),
    );
    map.insert(
        "exclusion",
        Exclusion {
            clocks: collection!("a", "b"),
        }
        .into(),
    );
    map.insert(
        "subclocking",
        Subclocking {
            left: "a",
            right: "b",
        }
        .into(),
    );
    map.insert(
        "intersection",
        Intersection {
            out: "i",
            args: collection!("a", "b"),
        }
        .into(),
    );
    map.insert(
        "union",
        Union {
            out: "u",
            args: collection!("a", "b"),
        }
        .into(),
    );
    map.insert(
        "delay",
        Delay {
            out: "d",
            base: "a",
            delay: 2,
            on: None,
        }
        .into(),
    );
    for (name, c) in map.into_iter() {
        let g: petgraph::Graph<&_, _> = (&c).into();
        write_graph(&g, dir, name)?;
    }
    Ok(())
}
