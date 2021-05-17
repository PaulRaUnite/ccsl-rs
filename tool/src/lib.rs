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
use ccsl::lccsl::automata::{STSBuilder, STS};
use ccsl::lccsl::constraints::{
    Alternates, Causality, Coincidence, Constraint, Delay, Exclusion, Intersection, Precedence,
    Subclocking, Union,
};
use ccsl::lccsl::opti::optimize_spec;
use itertools::Itertools;
use petgraph::dot::Config::{EdgeNoLabel, NodeNoLabel};
use petgraph::dot::Dot;
use petgraph::Graph;
use std::collections::hash_map::DefaultHasher;
use std::fmt;
use std::hash::{Hash, Hasher};

pub fn write_graph<N: Display, E: Display>(
    g: &Graph<N, E>,
    dir: &Path,
    file: &str,
) -> Result<(), Box<dyn Error>> {
    create_dir_all(dir)?;
    let mut file = BufWriter::new(File::create(dir.join(file).with_extension("dot"))?);
    let dot = Dot::with_config(g, &[EdgeNoLabel]);
    writeln!(&mut file, "{}", &dot)?;
    Ok(())
}

pub fn write_graph_no_label<N: fmt::Debug, E: fmt::Debug>(
    g: &Graph<N, E>,
    dir: &Path,
    file: &str,
) -> Result<(), Box<dyn Error>> {
    create_dir_all(dir)?;
    let mut file = BufWriter::new(File::create(dir.join(file).with_extension("dot"))?);
    let dot = Dot::with_config(g, &[NodeNoLabel, EdgeNoLabel]);
    writeln!(&mut file, "{:?}", &dot)?;
    Ok(())
}

pub fn vec_into_vec<I: Into<O>, O>(vec: impl IntoIterator<Item = I>) -> Vec<O> {
    vec.into_iter().map(Into::into).collect_vec()
}

#[derive(Debug, Copy, Clone, Serialize)]
pub struct SpecCombParams {
    pub spec: u64,
    pub variant: u64,
    pub comb: u64,
    pub real: Criteria,
    pub limit: Criteria,
    pub approx: Criteria,
}

#[derive(Debug, Copy, Clone, Serialize)]
pub struct SquishedParams {
    pub spec: u64,
    pub variant: u64,
    pub limit: Criteria,
    pub approx: Criteria,
}

#[derive(Debug, Copy, Clone, Serialize)]
pub struct Criteria {
    pub test: usize,
    pub down: usize,
    pub solutions: usize,
}

pub fn hash(h: impl Hash) -> u64 {
    let mut hasher = DefaultHasher::new();
    h.hash(&mut hasher);
    hasher.finish()
}

pub fn hash_spec<'a, C: 'a + Hash>(spec: impl IntoIterator<Item = &'a Constraint<C>>) -> u64 {
    let hashes = spec
        .into_iter()
        .map(|c: &Constraint<C>| hash(c))
        .collect_vec();
    hash(&hashes)
}

pub fn analyze_specification<C>(
    spec: Vec<STS<C>>,
    original_hash: u64,
    hashes: &[u64],
) -> Result<(Vec<SpecCombParams>, SquishedParams), Box<dyn Error>>
where
    C: Hash + Clone + Ord + fmt::Display + fmt::Debug + Send + Sync,
{
    let spec_hash = hash(&hashes);

    let mut analytics = Vec::with_capacity(spec.iter().map(|c| c.states().size_hint().0).product());
    analytics.par_extend(generate_combinations(&spec).par_bridge().map(|comb| {
        let comb_hash = combination_identifier(&hashes, &comb);
        let mut visitor = CountingVisitor::new();
        let actual = find_solutions(&spec, &comb, Some(&mut visitor));
        let dep_map = limit_conflict_map(&spec, &comb);
        let limit = complexity_by_graph(&dep_map);
        let dep_map = approx_conflict_map(&spec, &comb);
        let approx = complexity_by_graph(&dep_map);
        SpecCombParams {
            spec: original_hash,
            variant: spec_hash,
            comb: comb_hash,
            real: Criteria {
                test: visitor.test,
                down: visitor.down,
                solutions: actual,
            },
            limit: Criteria {
                test: limit.tests,
                down: limit.downs,
                solutions: limit.solutions,
            },
            approx: Criteria {
                test: approx.tests.to_usize().unwrap_or_default(),
                down: approx.downs.to_usize().unwrap_or_default(),
                solutions: approx.solutions.to_usize().unwrap_or_default(),
            },
        }
    }));
    let spec: Vec<STS<_>> = spec.into_iter().map(|c| c.squish()).collect();

    let comb: Vec<_> = spec
        .iter()
        .map(|c| c.states().exactly_one())
        .collect::<Result<Vec<_>, _>>()
        .unwrap();
    let dep_map = limit_conflict_map(&spec, &comb);
    let limit = complexity_by_graph(&dep_map);
    let dep_map = approx_conflict_map(&spec, &comb);
    let approx = complexity_by_graph(&dep_map);

    let squished = SquishedParams {
        spec: original_hash,
        variant: spec_hash,
        limit: Criteria {
            test: limit.tests,
            down: limit.downs,
            solutions: limit.solutions,
        },
        approx: Criteria {
            test: approx.tests.to_usize().ok_or("cannot convert to usize")?,
            down: approx.downs.to_usize().ok_or("cannot convert to usize")?,
            solutions: approx
                .solutions
                .to_usize()
                .ok_or("cannot convert to usize")?,
        },
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
        Into::<STSBuilder<_>>::into(Coincidence {
            left: "a",
            right: "b",
        })
        .into(),
    );
    map.insert(
        "alternates",
        Into::<STSBuilder<_>>::into(Alternates {
            left: "a",
            right: "b",
        })
        .into(),
    );
    map.insert(
        "causality",
        Into::<STSBuilder<_>>::into(Causality {
            left: "a",
            right: "b",
            init: None,
            max: None,
        })
        .into(),
    );
    map.insert(
        "precedence",
        Into::<STSBuilder<_>>::into(Precedence {
            left: "a",
            right: "b",
            init: None,
            max: None,
        })
        .into(),
    );
    map.insert(
        "exclusion",
        Into::<STSBuilder<_>>::into(Exclusion {
            clocks: collection!("a", "b"),
        })
        .into(),
    );
    map.insert(
        "subclocking",
        Into::<STSBuilder<_>>::into(Subclocking {
            left: "a",
            right: "b",
        })
        .into(),
    );
    map.insert(
        "intersection",
        Into::<STSBuilder<_>>::into(Intersection {
            out: "i",
            args: collection!("a", "b"),
        })
        .into(),
    );
    map.insert(
        "union",
        Into::<STSBuilder<_>>::into(Union {
            out: "u",
            args: collection!("a", "b"),
        })
        .into(),
    );
    map.insert(
        "delay",
        Into::<STSBuilder<_>>::into(Delay {
            out: "d",
            base: "a",
            delay: 2,
            on: None,
        })
        .into(),
    );
    for (name, c) in map.into_iter() {
        let g: petgraph::Graph<_, _> = (&c).into();
        write_graph(&g, dir, name)?;
    }
    Ok(())
}
