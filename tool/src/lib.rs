extern crate itertools;
extern crate num;
extern crate rayon;
extern crate serde;
#[macro_use]
extern crate concat_arrays;
extern crate arrow;

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

use arrow::array::{ArrayRef, Int64Array, StructArray, UInt64Array};
use arrow::datatypes::{DataType, Field, Schema, SchemaRef};
use arrow::record_batch::RecordBatch;
use ccsl::lccsl::algo::{
    approx_conflict_map, combination_identifier, complexity_by_graph, find_solutions,
    generate_combinations, limit_conflict_map, CountingVisitor,
};
use ccsl::lccsl::automata::{Label, STSBuilder, STS};
use ccsl::lccsl::constraints::{
    Alternates, Causality, Coincidence, Constraint, Delay, Exclusion, Intersection, Precedence,
    Subclocking, Union,
};
use itertools::Itertools;
use petgraph::dot::Config::{EdgeNoLabel, NodeNoLabel};
use petgraph::dot::Dot;
use petgraph::Graph;
use std::collections::hash_map::DefaultHasher;
use std::fmt;
use std::hash::{Hash, Hasher};
use std::ops::BitOr;
use std::sync::Arc;

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

impl SpecCombParams {
    pub fn into_array(self) -> [u64; 12] {
        let local = [self.spec, self.variant, self.comb];
        let real = self.real.into_array();
        let limit = self.limit.into_array();
        let approx = self.approx.into_array();
        concat_arrays!(local, real, limit, approx)
    }

    pub fn schema() -> Schema {
        let criteria_type = Criteria::arrow_type();
        Schema::new(vec![
            Field::new("spec", DataType::UInt64, false),
            Field::new("variant", DataType::UInt64, false),
            Field::new("comb", DataType::UInt64, false),
            Field::new("real", criteria_type.clone(), false),
            Field::new("limit", criteria_type.clone(), false),
            Field::new("approx", criteria_type, false),
        ])
    }

    pub fn batch(
        schema: SchemaRef,
        data: &Vec<SpecCombParams>,
    ) -> Result<RecordBatch, Box<dyn Error>> {
        let size = data.len();
        let mut spec_vec = Vec::with_capacity(size);
        let mut var_vec = Vec::with_capacity(size);
        let mut comb_vec = Vec::with_capacity(size);
        let mut real_test_vec = Vec::with_capacity(size);
        let mut limit_test_vec = Vec::with_capacity(size);
        let mut approx_test_vec = Vec::with_capacity(size);
        let mut real_down_vec = Vec::with_capacity(size);
        let mut limit_down_vec = Vec::with_capacity(size);
        let mut approx_down_vec = Vec::with_capacity(size);
        let mut real_solution_vec = Vec::with_capacity(size);
        let mut limit_solution_vec = Vec::with_capacity(size);
        let mut approx_solution_vec = Vec::with_capacity(size);
        for e in data {
            spec_vec.push(e.spec as u64);
            var_vec.push(e.variant as u64);
            comb_vec.push(e.comb as u64);
            real_test_vec.push(e.real.test as u64);
            real_down_vec.push(e.real.down as u64);
            real_solution_vec.push(e.real.solutions as u64);
            limit_test_vec.push(e.limit.test as u64);
            limit_down_vec.push(e.limit.down as u64);
            limit_solution_vec.push(e.limit.solutions as u64);
            approx_test_vec.push(e.approx.test as u64);
            approx_down_vec.push(e.approx.down as u64);
            approx_solution_vec.push(e.approx.solutions as u64);
        }

        let spec_vec = Arc::new(UInt64Array::from(spec_vec));
        let var_vec = Arc::new(UInt64Array::from(var_vec));
        let comb_vec = Arc::new(UInt64Array::from(comb_vec));

        let real_test_vec: ArrayRef = Arc::new(UInt64Array::from(real_test_vec));
        let limit_test_vec: ArrayRef = Arc::new(UInt64Array::from(limit_test_vec));
        let approx_test_vec: ArrayRef = Arc::new(UInt64Array::from(approx_test_vec));
        let real_down_vec: ArrayRef = Arc::new(UInt64Array::from(real_down_vec));
        let limit_down_vec: ArrayRef = Arc::new(UInt64Array::from(limit_down_vec));
        let approx_down_vec: ArrayRef = Arc::new(UInt64Array::from(approx_down_vec));
        let real_solution_vec: ArrayRef = Arc::new(UInt64Array::from(real_solution_vec));
        let limit_solution_vec: ArrayRef = Arc::new(UInt64Array::from(limit_solution_vec));
        let approx_solution_vec: ArrayRef = Arc::new(UInt64Array::from(approx_solution_vec));

        let [test_f, down_f, solution_f] = Criteria::arrow_fields();
        let real = Arc::new(StructArray::from(vec![
            (test_f, real_test_vec),
            (down_f, real_down_vec),
            (solution_f, real_solution_vec),
        ]));

        let [test_f, down_f, solution_f] = Criteria::arrow_fields();
        let limit = Arc::new(StructArray::from(vec![
            (test_f, limit_test_vec),
            (down_f, limit_down_vec),
            (solution_f, limit_solution_vec),
        ]));

        let [test_f, down_f, solution_f] = Criteria::arrow_fields();
        let approx = Arc::new(StructArray::from(vec![
            (test_f, approx_test_vec),
            (down_f, approx_down_vec),
            (solution_f, approx_solution_vec),
        ]));

        let result = RecordBatch::try_new(
            schema,
            vec![spec_vec, var_vec, comb_vec, real, limit, approx],
        )?;
        Ok(result)
    }
}

#[derive(Debug, Copy, Clone, Serialize)]
pub struct SquishedParams {
    pub spec: u64,
    pub variant: u64,
    pub limit: Criteria,
    pub approx: Criteria,
}

impl SquishedParams {
    pub fn into_array(self) -> [u64; 8] {
        let local = [self.spec, self.variant];
        let limit = self.limit.into_array();
        let approx = self.approx.into_array();
        concat_arrays!(local, limit, approx)
    }

    pub fn schema() -> Schema {
        let criteria_type = Criteria::arrow_type();
        Schema::new(vec![
            Field::new("spec", DataType::UInt64, false),
            Field::new("variant", DataType::UInt64, false),
            Field::new("limit", criteria_type.clone(), false),
            Field::new("approx", criteria_type, false),
        ])
    }

    pub fn batch(
        schema: SchemaRef,
        data: &Vec<SquishedParams>,
    ) -> Result<RecordBatch, Box<dyn Error>> {
        let size = data.len();
        let mut spec_vec = Vec::with_capacity(size);
        let mut var_vec = Vec::with_capacity(size);
        let mut limit_test_vec = Vec::with_capacity(size);
        let mut approx_test_vec = Vec::with_capacity(size);
        let mut limit_down_vec = Vec::with_capacity(size);
        let mut approx_down_vec = Vec::with_capacity(size);
        let mut limit_solution_vec = Vec::with_capacity(size);
        let mut approx_solution_vec = Vec::with_capacity(size);
        for e in data {
            spec_vec.push(e.spec as u64);
            var_vec.push(e.variant as u64);
            limit_test_vec.push(e.limit.test as u64);
            limit_down_vec.push(e.limit.down as u64);
            limit_solution_vec.push(e.limit.solutions as u64);
            approx_test_vec.push(e.approx.test as u64);
            approx_down_vec.push(e.approx.down as u64);
            approx_solution_vec.push(e.approx.solutions as u64);
        }

        let spec_vec: ArrayRef = Arc::new(UInt64Array::from(spec_vec));
        let var_vec: ArrayRef = Arc::new(UInt64Array::from(var_vec));
        let limit_test_vec: ArrayRef = Arc::new(UInt64Array::from(limit_test_vec));
        let approx_test_vec: ArrayRef = Arc::new(UInt64Array::from(approx_test_vec));
        let limit_down_vec: ArrayRef = Arc::new(UInt64Array::from(limit_down_vec));
        let approx_down_vec: ArrayRef = Arc::new(UInt64Array::from(approx_down_vec));
        let limit_solution_vec: ArrayRef = Arc::new(UInt64Array::from(limit_solution_vec));
        let approx_solution_vec: ArrayRef = Arc::new(UInt64Array::from(approx_solution_vec));

        let [test_f, down_f, solution_f] = Criteria::arrow_fields();
        let limit = Arc::new(StructArray::from(vec![
            (test_f, limit_test_vec),
            (down_f, limit_down_vec),
            (solution_f, limit_solution_vec),
        ]));

        let [test_f, down_f, solution_f] = Criteria::arrow_fields();
        let approx = Arc::new(StructArray::from(vec![
            (test_f, approx_test_vec),
            (down_f, approx_down_vec),
            (solution_f, approx_solution_vec),
        ]));

        let result = RecordBatch::try_new(schema, vec![spec_vec, var_vec, limit, approx])?;
        Ok(result)
    }
}

#[derive(Debug, Copy, Clone, Serialize)]
pub struct Criteria {
    pub test: usize,
    pub down: usize,
    pub solutions: usize,
}
impl Criteria {
    pub fn into_array(self) -> [u64; 3] {
        [self.test as u64, self.down as u64, self.solutions as u64]
    }

    pub fn arrow_type() -> DataType {
        DataType::Struct(Self::arrow_fields().to_vec())
    }
    fn arrow_fields() -> [Field; 3] {
        [
            Field::new("tests", DataType::UInt64, false),
            Field::new("downs", DataType::UInt64, false),
            Field::new("solutions", DataType::UInt64, false),
        ]
    }
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

pub fn analyze_specification<C, L>(
    spec: Vec<STS<C, L>>,
    original_hash: u64,
    hashes: &[u64],
) -> Result<(Vec<SpecCombParams>, SquishedParams), Box<dyn Error>>
where
    C: Hash + Clone + Ord + fmt::Display + fmt::Debug + Sync + Send,
    L: Label<C> + Clone + Hash + Eq + Sync,
    for<'c, 'd> &'c L: BitOr<&'d L, Output = L>,
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
    let spec: Vec<STS<C, L>> = spec.into_iter().map(|c| c.squish()).collect();

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

#[cfg(test)]
mod test {
    #[test]
    fn casting() {
        assert_eq!((u64::MAX as i64) as u64, u64::MAX);
    }
}
