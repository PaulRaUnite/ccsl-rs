extern crate arrow;
extern crate itertools;
extern crate num;
extern crate rayon;
extern crate serde;

use std::error::Error;
use std::fmt;
use std::fmt::Display;
use std::fs::{create_dir_all, File};
use std::hash::Hash;
use std::io::BufWriter;
use std::io::Write;
use std::ops::BitOr;
use std::path::Path;
use std::sync::Arc;

use arrow::array::{ArrayRef, StructArray, UInt32Array, UInt64Array, UInt8Array};
use arrow::datatypes::{DataType, Field, Schema};
use arrow::record_batch::RecordBatch;
use itertools::Itertools;
use num::ToPrimitive;
use permutation::Permutation;
use petgraph::dot::Config::{EdgeNoLabel, NodeNoLabel};
use petgraph::dot::Dot;
use petgraph::{Directed, EdgeType, Graph};
use rayon::iter::ParallelBridge;
use rayon::prelude::{ParallelExtend, ParallelIterator};
use serde::Serialize;

use ccsl::lccsl::algo::{
    approx_conflict_map, complexity_from_graph, find_solutions, generate_combinations,
    limit_conflict_map, CountingVisitor,
};
use ccsl::lccsl::automata::{ClockLabelClassic, Label, STSBuilder, StateRef, STS};
use ccsl::lccsl::constraints::{
    Alternates, Causality, Coincidence, Constraint, Delay, Exclusion, Intersection, Precedence,
    Subclocking, Union,
};
use ccsl::lccsl::vizualization::unfold_specification;
use parquet::arrow::ArrowWriter;
use std::convert::TryInto;

pub fn write_graph<N: Display, E: Display, D: EdgeType>(
    g: &Graph<N, E, D>,
    dir: &Path,
    file: &str,
) -> Result<(), Box<dyn Error>> {
    create_dir_all(dir)?;
    let mut file = BufWriter::new(File::create(dir.join(file).with_extension("dot"))?);
    let dot = Dot::with_config(g, &[]);
    writeln!(&mut file, "{}", &dot)?;
    Ok(())
}

pub fn write_graph_no_label<N: fmt::Debug, E: fmt::Debug, D: EdgeType>(
    g: &Graph<N, E, D>,
    dir: &Path,
    file: &str,
) -> Result<(), Box<dyn Error>> {
    create_dir_all(dir)?;
    let mut file = BufWriter::new(File::create(dir.join(file).with_extension("dot"))?);
    let dot = Dot::with_config(g, &[NodeNoLabel, EdgeNoLabel]);
    writeln!(&mut file, "{:?}", &dot)?;
    Ok(())
}

pub fn write_graph_indexed_debug<N: fmt::Debug, E: fmt::Debug, D: EdgeType>(
    g: &Graph<N, E, D>,
    dir: &Path,
    file: &str,
) -> Result<(), Box<dyn Error>> {
    let g = g.map(
        |n, w| format!("#:{} {:?}", n.index(), w),
        |_, w| format!("{:?}", w),
    );
    write_graph(&g, dir, file)
}

pub fn write_graph_indexed<N: Display, E: Display, D: EdgeType>(
    g: &Graph<N, E, D>,
    dir: &Path,
    file: &str,
) -> Result<(), Box<dyn Error>> {
    let g = g.map(
        |n, w| format!("#:{} {}", n.index(), w),
        |_, w| w.to_string(),
    );
    write_graph(&g, dir, file)
}

pub fn vec_into_vec<I: Into<O>, O>(vec: impl IntoIterator<Item = I>) -> Vec<O> {
    vec.into_iter().map(Into::into).collect_vec()
}

#[derive(Debug, Copy, Clone, Serialize)]
pub struct SpecCombParams {
    pub spec: u64,
    pub variant: u64,
    pub comb: u64,
    pub size: u8,
    pub real: Criteria,
    pub limit: Criteria,
    pub approx: Criteria,
    pub clocks: usize,
}

pub trait ToArrow: Sized {
    fn schema() -> Schema;
    fn batch(data: &Vec<Self>) -> Result<RecordBatch, Box<dyn Error>>;
}

impl ToArrow for SpecCombParams {
    fn schema() -> Schema {
        let criteria_type = Criteria::arrow_type();
        Schema::new(vec![
            Field::new("spec", DataType::UInt64, false),
            Field::new("variant", DataType::UInt64, false),
            Field::new("comb", DataType::UInt64, false),
            Field::new("size", DataType::UInt8, false),
            Field::new("real", criteria_type.clone(), false),
            Field::new("limit", criteria_type.clone(), false),
            Field::new("approx", criteria_type, false),
            Field::new("clocks", DataType::UInt32, false),
        ])
    }

    fn batch(data: &Vec<Self>) -> Result<RecordBatch, Box<dyn Error>> {
        let size = data.len();
        let mut spec_vec = Vec::with_capacity(size);
        let mut var_vec = Vec::with_capacity(size);
        let mut comb_vec = Vec::with_capacity(size);
        let mut size_vec = Vec::with_capacity(size);
        let mut real_test_vec: Vec<u64> = Vec::with_capacity(size);
        let mut limit_test_vec: Vec<u64> = Vec::with_capacity(size);
        let mut approx_test_vec: Vec<u64> = Vec::with_capacity(size);
        let mut real_down_vec: Vec<u64> = Vec::with_capacity(size);
        let mut limit_down_vec: Vec<u64> = Vec::with_capacity(size);
        let mut approx_down_vec: Vec<u64> = Vec::with_capacity(size);
        let mut real_solution_vec: Vec<u64> = Vec::with_capacity(size);
        let mut limit_solution_vec: Vec<u64> = Vec::with_capacity(size);
        let mut approx_solution_vec: Vec<u64> = Vec::with_capacity(size);
        let mut clocks_vec: Vec<u32> = Vec::with_capacity(size);
        for e in data {
            spec_vec.push(e.spec);
            var_vec.push(e.variant);
            comb_vec.push(e.comb);
            size_vec.push(e.size);
            real_test_vec.push(e.real.test.try_into().unwrap());
            real_down_vec.push(e.real.down.try_into().unwrap());
            real_solution_vec.push(e.real.solutions.try_into().unwrap());
            limit_test_vec.push(e.limit.test.try_into().unwrap());
            limit_down_vec.push(e.limit.down.try_into().unwrap());
            limit_solution_vec.push(e.limit.solutions.try_into().unwrap());
            approx_test_vec.push(e.approx.test.try_into().unwrap());
            approx_down_vec.push(e.approx.down.try_into().unwrap());
            approx_solution_vec.push(e.approx.solutions.try_into().unwrap());
            clocks_vec.push(e.clocks.try_into().unwrap());
        }

        let spec_vec = Arc::new(UInt64Array::from(spec_vec));
        let var_vec = Arc::new(UInt64Array::from(var_vec));
        let comb_vec = Arc::new(UInt64Array::from(comb_vec));
        let size_vec = Arc::new(UInt8Array::from(size_vec));

        let real_test_vec: ArrayRef = Arc::new(UInt64Array::from(real_test_vec));
        let limit_test_vec: ArrayRef = Arc::new(UInt64Array::from(limit_test_vec));
        let approx_test_vec: ArrayRef = Arc::new(UInt64Array::from(approx_test_vec));
        let real_down_vec: ArrayRef = Arc::new(UInt64Array::from(real_down_vec));
        let limit_down_vec: ArrayRef = Arc::new(UInt64Array::from(limit_down_vec));
        let approx_down_vec: ArrayRef = Arc::new(UInt64Array::from(approx_down_vec));
        let real_solution_vec: ArrayRef = Arc::new(UInt64Array::from(real_solution_vec));
        let limit_solution_vec: ArrayRef = Arc::new(UInt64Array::from(limit_solution_vec));
        let approx_solution_vec: ArrayRef = Arc::new(UInt64Array::from(approx_solution_vec));
        let clocks_vec: ArrayRef = Arc::new(UInt32Array::from(clocks_vec));

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
            Arc::new(Self::schema()),
            vec![
                spec_vec, var_vec, comb_vec, size_vec, real, limit, approx, clocks_vec,
            ],
        )?;
        Ok(result)
    }
}

#[derive(Debug, Copy, Clone, Serialize)]
pub struct SquishedParams {
    pub spec: u64,
    pub variant: u64,
    pub size: u8,
    pub limit: Criteria,
    pub approx: Criteria,
}

impl ToArrow for SquishedParams {
    fn schema() -> Schema {
        let criteria_type = Criteria::arrow_type();
        Schema::new(vec![
            Field::new("spec", DataType::UInt64, false),
            Field::new("variant", DataType::UInt64, false),
            Field::new("size", DataType::UInt8, false),
            Field::new("limit", criteria_type.clone(), false),
            Field::new("approx", criteria_type, false),
        ])
    }

    fn batch(data: &Vec<Self>) -> Result<RecordBatch, Box<dyn Error>> {
        let size = data.len();
        let mut spec_vec = Vec::with_capacity(size);
        let mut var_vec = Vec::with_capacity(size);
        let mut size_vec = Vec::with_capacity(size);
        let mut limit_test_vec = Vec::with_capacity(size);
        let mut approx_test_vec = Vec::with_capacity(size);
        let mut limit_down_vec = Vec::with_capacity(size);
        let mut approx_down_vec = Vec::with_capacity(size);
        let mut limit_solution_vec = Vec::with_capacity(size);
        let mut approx_solution_vec = Vec::with_capacity(size);
        for e in data {
            spec_vec.push(e.spec as u64);
            var_vec.push(e.variant as u64);
            size_vec.push(e.size);
            limit_test_vec.push(e.limit.test as u64);
            limit_down_vec.push(e.limit.down as u64);
            limit_solution_vec.push(e.limit.solutions as u64);
            approx_test_vec.push(e.approx.test as u64);
            approx_down_vec.push(e.approx.down as u64);
            approx_solution_vec.push(e.approx.solutions as u64);
        }

        let spec_vec: ArrayRef = Arc::new(UInt64Array::from(spec_vec));
        let var_vec: ArrayRef = Arc::new(UInt64Array::from(var_vec));
        let size_vec: ArrayRef = Arc::new(UInt8Array::from(size_vec));

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

        let result = RecordBatch::try_new(
            Arc::new(Self::schema()),
            vec![spec_vec, var_vec, size_vec, limit, approx],
        )?;
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
pub fn analyze_specification_combination<C, L>(
    spec: &[STS<C, L>],
    comb: &[StateRef],
    spec_id: u64,
    perm_id: u64,
    comb_id: u64,
) -> SpecCombParams
where
    C: Hash + Clone + Ord + fmt::Display + fmt::Debug + Sync + Send,
    L: Label<C> + Clone + Hash + Eq + Sync,
    for<'c, 'd> &'c L: BitOr<&'d L, Output = L>,
{
    let mut visitor = CountingVisitor::new();
    let actual = find_solutions(spec, &comb, Some(&mut visitor));
    let dep_map = limit_conflict_map(spec, &comb);
    let limit = complexity_from_graph(&dep_map);
    let dep_map = approx_conflict_map(spec, &comb);
    let approx = complexity_from_graph(&dep_map);
    SpecCombParams {
        spec: spec_id,
        variant: perm_id,
        comb: comb_id,
        size: spec.len() as u8,
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
            test: approx.tests.to_usize().unwrap(),
            down: approx.downs.to_usize().unwrap(),
            solutions: approx.solutions.to_usize().unwrap(),
        },
        clocks: spec.iter().flat_map(|c| c.clocks().iter()).unique().count(),
    }
}

pub fn analyze_squish_specification<C, L>(
    spec: &[STS<C, L>],
    spec_id: u64,
    perm_id: u64,
) -> SquishedParams
where
    C: Clone + Hash + Ord,
    L: Label<C> + Clone + Hash + Eq,
{
    let spec: Vec<STS<C, L>> = spec.iter().map(|c| c.clone().squish()).collect();
    let comb: Vec<StateRef> = spec.iter().map(|c| c.initial()).collect_vec();

    let dep_map = limit_conflict_map(&spec, &comb);
    let limit = complexity_from_graph(&dep_map);
    let dep_map = approx_conflict_map(&spec, &comb);
    let approx = complexity_from_graph(&dep_map);

    SquishedParams {
        spec: spec_id,
        variant: perm_id,
        size: spec.len() as u8,
        limit: Criteria {
            test: limit.tests,
            down: limit.downs,
            solutions: limit.solutions,
        },
        approx: Criteria {
            test: approx.tests.to_usize().unwrap(),
            down: approx.downs.to_usize().unwrap(),
            solutions: approx.solutions.to_usize().unwrap(),
        },
    }
}
pub fn analyze_specification<C, L>(
    spec: &[STS<C, L>],
    spec_id: u64,
    perm_id: u64,
) -> (Vec<SpecCombParams>, SquishedParams)
where
    C: Hash + Clone + Ord + fmt::Display + fmt::Debug + Sync + Send,
    L: Label<C> + Clone + Hash + Eq + Sync,
    for<'c, 'd> &'c L: BitOr<&'d L, Output = L>,
{
    let mut analytics = Vec::with_capacity(spec.iter().map(|c| c.states().size_hint().0).product());
    analytics.par_extend(
        generate_combinations(spec)
            .enumerate()
            .par_bridge()
            .map(|(i, comb)| {
                analyze_specification_combination(spec, &comb, spec_id, perm_id, i as u64)
            }),
    );

    (
        analytics,
        analyze_squish_specification(spec, spec_id, perm_id),
    )
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
    let mut map: Vec<(&str, STS<&str, ClockLabelClassic<&str>>)> = Vec::with_capacity(100);
    map.push((
        "coincidence",
        Into::<STSBuilder<_>>::into(&Coincidence {
            left: "a",
            right: "b",
        })
        .into(),
    ));
    map.push((
        "alternates",
        Into::<STSBuilder<_>>::into(&Alternates {
            left: "a",
            right: "b",
        })
        .into(),
    ));
    map.push((
        "causality",
        Into::<STSBuilder<_>>::into(&Causality {
            left: "a",
            right: "b",
            init: None,
            max: None,
        })
        .into(),
    ));
    map.push((
        "precedence",
        Into::<STSBuilder<_>>::into(&Precedence {
            left: "a",
            right: "b",
            init: None,
            max: None,
        })
        .into(),
    ));
    map.push((
        "exclusion",
        Into::<STSBuilder<_>>::into(&Exclusion {
            clocks: collection!("a", "b"),
        })
        .into(),
    ));
    map.push((
        "subclocking",
        Into::<STSBuilder<_>>::into(&Subclocking {
            left: "a",
            right: "b",
        })
        .into(),
    ));
    map.push((
        "intersection",
        Into::<STSBuilder<_>>::into(&Intersection {
            out: "i",
            args: collection!("a", "b"),
        })
        .into(),
    ));
    map.push((
        "union",
        Into::<STSBuilder<_>>::into(&Union {
            out: "u",
            args: collection!("a", "b"),
        })
        .into(),
    ));
    map.push((
        "delay",
        Into::<STSBuilder<_>>::into(&Delay {
            out: "d",
            base: "a",
            delay: 2,
            on: None,
        })
        .into(),
    ));
    for (name, c) in map.iter() {
        let g: petgraph::Graph<_, _> = c.into();
        write_graph(&g, dir, name)?;
    }
    let spec = map.into_iter().map(|(_, v)| v).take(4).collect_vec();
    let comb = spec.iter().map(|c| c.initial()).collect_vec();
    write_graph(
        &unfold_specification(&spec, &comb, true),
        dir,
        "trimmed.dot",
    )?;
    write_graph(&unfold_specification(&spec, &comb, false), dir, "full.dot")?;
    let spec = [1, 2, 0, 3].iter().map(|i| spec[*i].clone()).collect_vec();
    write_graph(
        &unfold_specification(&spec, &spec.iter().map(|c| c.initial()).collect_vec(), true),
        dir,
        "reordered.dot",
    )?;
    Ok(())
}

#[cfg(test)]
mod test {
    #[test]
    fn casting() {
        assert_eq!((u64::MAX as i64) as u64, u64::MAX);
    }
}

pub fn decode_spec<C>(
    gen: impl IntoIterator<Item = (u64, Vec<Constraint<C>>)>,
    spec_id: u64,
) -> Option<Vec<Constraint<C>>> {
    gen.into_iter()
        .find(|(id, _)| id == &spec_id)
        .map(|(_, s)| s)
}

pub fn decode_perm<C>(spec: &Vec<Constraint<C>>, perm_id: u64) -> Option<Vec<Constraint<C>>>
where
    C: Clone,
{
    if spec.len() > (u8::MAX as usize) {
        None
    } else {
        Some(
            Permutation::from_vec(
                lehmer::Lehmer::from_decimal(perm_id as usize, spec.len())
                    .to_permutation()
                    .into_iter()
                    .map(|x| x as usize)
                    .collect_vec(),
            )
            .apply_slice(spec.as_slice()),
        )
    }
}

pub fn decode_comb<C, L: Label<C>>(spec: &Vec<STS<C, L>>, comb_id: u64) -> Option<Vec<StateRef>>
where
    C: Clone + Ord + Hash + Display,
    for<'a, 'b> &'a L: BitOr<&'b L, Output = L>,
{
    let result = generate_combinations(spec).nth(comb_id as usize);
    result
}

pub fn gen_spec<C>(
    r: impl Iterator<Item = usize> + Clone,
    gen: impl FnMut(usize) -> Vec<Constraint<C>> + Clone,
) -> impl Iterator<Item = (u64, Vec<Constraint<C>>)> + Clone {
    r.into_iter()
        .map(gen)
        .enumerate()
        .map(|(i, s)| (i as u64, s))
}

pub fn gen_spec_flat<C, I>(
    r: impl Iterator<Item = usize> + Clone,
    gen: impl FnMut(usize) -> I + Clone,
) -> impl Iterator<Item = (u64, Vec<Constraint<C>>)> + Clone
where
    I: Iterator<Item = Vec<Constraint<C>>> + Clone,
{
    r.into_iter()
        .flat_map(gen)
        .enumerate()
        .map(|(i, s)| (i as u64, s))
}

pub fn inverse_graph<N, E>(g: Graph<N, E, Directed>) -> Graph<N, E, Directed> {
    let mut new = Graph::with_capacity(g.node_count(), g.edge_count());

    let (nodes, edges) = g.into_nodes_edges();
    for n in nodes {
        new.add_node(n.weight);
    }
    for e in edges {
        new.add_edge(e.target(), e.source(), e.weight);
    }
    new
}

pub fn stream_to_parquet_flat<I, O, F>(
    filepath: &Path,
    stream: impl IntoIterator<Item = I>,
    map: F,
    chunks: usize,
    arrow_buffer: usize,
) -> Result<(), Box<dyn Error>>
where
    I: Sized + Sync,
    O: ToArrow + Send,
    F: Fn(&I) -> Vec<O> + Sync + Send + Clone,
{
    let schema = Arc::new(O::schema());
    let mut parquet_wrt = ArrowWriter::try_new(File::create(filepath)?, schema, None)?;

    let mut in_buffer: Vec<I> = Vec::with_capacity(chunks);
    let mut out_buffer: Vec<O> = Vec::with_capacity(arrow_buffer);

    for chunk in &stream.into_iter().chunks(chunks) {
        in_buffer.clear();
        in_buffer.extend(chunk);
        out_buffer.clear();
        out_buffer.par_extend(in_buffer.iter().par_bridge().flat_map(map.clone()));
        parquet_wrt.write(&O::batch(&out_buffer)?)?;
    }
    parquet_wrt.close()?;
    Ok(())
}

pub fn stream_to_parquet<I, O, F>(
    filepath: &Path,
    stream: impl IntoIterator<Item = I>,
    map: F,
    chunks: usize,
) -> Result<(), Box<dyn Error>>
where
    I: Sized + Sync,
    O: ToArrow + Send,
    F: Fn(&I) -> O + Sync + Send + Clone,
{
    let schema = Arc::new(O::schema());
    let mut parquet_wrt = ArrowWriter::try_new(File::create(filepath)?, schema, None)?;

    let mut in_buffer: Vec<I> = Vec::with_capacity(chunks);
    let mut out_buffer: Vec<O> = Vec::with_capacity(chunks);

    for chunk in &stream.into_iter().chunks(chunks) {
        in_buffer.clear();
        in_buffer.extend(chunk);
        out_buffer.clear();
        out_buffer.par_extend(in_buffer.iter().par_bridge().map(map.clone()));
        parquet_wrt.write(&O::batch(&out_buffer)?)?;
    }
    parquet_wrt.close()?;
    Ok(())
}
