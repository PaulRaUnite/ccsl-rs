extern crate itertools;
extern crate num;
extern crate rayon;
extern crate serde;

use rayon::iter::ParallelBridge;
use rayon::prelude::{ParallelExtend, ParallelIterator};

use num::ToPrimitive;

use serde::Serialize;

use std::collections::HashMap;
use std::error::Error;
use std::fmt::Display;
use std::fs::{create_dir_all, File};
use std::io::BufWriter;
use std::io::Write;
use std::path::Path;

use ccsl::lccsl::algo::{
    approx_conflict_map, combination_identifier, complexity_by_graph, find_solutions,
    generate_combinations, limit_conflict_map, CountingVisitor,
};
use ccsl::lccsl::automata::STS;
use ccsl::lccsl::constraints::{
    Alternates, Causality, Coincidence, Constraint, Delay, Exclusion, Intersection, Precedence,
    Subclocking, Union,
};
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

#[derive(Debug, Copy, Clone, Serialize)]
pub struct SpecCombParams {
    pub spec: u64,
    pub comb: u64,
    pub test: usize,
    pub down: usize,
    pub solutions: usize,
    pub limit_test: usize,
    pub limit_down: usize,
    pub limit_solutions: usize,
}

#[derive(Debug, Copy, Clone, Serialize)]
pub struct SquishedParams {
    pub spec: u64,
    pub limit_test: usize,
    pub limit_down: usize,
    pub limit_solutions: usize,
    pub approx_test: usize,
    pub approx_down: usize,
    pub approx_solutions: usize,
}

fn hash(h: impl Hash) -> u64 {
    let mut hasher = DefaultHasher::new();
    h.hash(&mut hasher);
    hasher.finish()
}

pub fn analyze_specification<C, I>(
    dir: &Path,
    spec: I,
) -> Result<(Vec<SpecCombParams>, SquishedParams), Box<dyn Error>>
where
    C: Hash + Clone + Ord + fmt::Display + fmt::Debug + Send + Sync,
    I: IntoIterator<Item = Constraint<C>>,
    for<'a> &'a I: IntoIterator<Item = &'a Constraint<C>>,
{
    let hashes = (&spec)
        .into_iter()
        .map(|c: &Constraint<C>| hash(c))
        .collect_vec();

    let spec_hash = hash(&hashes);

    // let dir = dir.join(spec_hash.to_string());
    // let tree_full_dir = dir.join("tree/full");
    // let tree_trimmed_dir = dir.join("tree/trimmed");
    // let map_dir = dir.join("map");

    let spec: Vec<STS<C>> = spec.into_iter().map(Into::into).collect();

    let mut analytics = Vec::with_capacity(spec.iter().map(|c| c.states().len()).product());
    analytics.par_extend(generate_combinations(&spec).par_bridge().map(|comb| {
        let comb_hash = combination_identifier::<C, _>(&hashes, &comb);
        // let id = comb_hash.to_string();
        // let full_tree = unfold_specification(&spec, &comb, true);
        // let trimmed_tree = unfold_specification(&spec, &comb, false);
        let dep_map = limit_conflict_map(&spec, &comb);
        let mut visitor = CountingVisitor::new();
        let actual = find_solutions(&spec, &comb, Some(&mut visitor));
        let approx = complexity_by_graph(&dep_map);
        SpecCombParams {
            spec: spec_hash,
            comb: comb_hash,
            test: visitor.test,
            down: visitor.down,
            solutions: actual,
            limit_test: approx.tests,
            limit_down: approx.downs,
            limit_solutions: approx.solutions,
        }
    }));

    // let squish_dir = dir.join("squish");
    let spec: Vec<STS<_>> = spec.into_iter().map(|c| c.squish()).collect();

    let comb: Vec<_> = spec
        .iter()
        .map(|c| c.states().iter().exactly_one())
        .collect::<Result<Vec<_>, _>>()
        .unwrap();
    let dep_map = limit_conflict_map(&spec, &comb);
    let limit = complexity_by_graph(&dep_map);
    let dep_map = approx_conflict_map(&spec, &comb);
    let approx = complexity_by_graph(&dep_map);
    let squished = SquishedParams {
        spec: spec_hash,
        limit_test: limit.tests,
        limit_down: limit.downs,
        limit_solutions: limit.solutions,
        approx_test: approx.tests.to_usize().ok_or("cannot convert to usize")?,
        approx_down: approx.downs.to_usize().ok_or("cannot convert to usize")?,
        approx_solutions: approx
            .solutions
            .to_usize()
            .ok_or("cannot convert to usize")?,
    };

    Ok((analytics, squished))
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
