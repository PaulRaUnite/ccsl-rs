use std::fmt;
use std::fmt::Display;
use std::fs::{create_dir_all, File};
use std::hash::Hash;
use std::io::BufWriter;
use std::io::Write;
use std::ops::BitOr;
use std::path::Path;
use std::sync::{Arc, Mutex};

use anyhow::Result;
use arrow::array::{ArrayRef, UInt64Array, UInt8Array};
use arrow::datatypes::{DataType, Field, Schema};
use arrow::record_batch::RecordBatch;
use itertools::Itertools;
use parquet::arrow::ArrowWriter;
use permutation::Permutation;
use petgraph::dot::Config::{EdgeNoLabel, NodeNoLabel};
use petgraph::dot::Dot;
use petgraph::{Directed, EdgeType, Graph};
use rayon::iter::{IntoParallelIterator, ParallelBridge};
use rayon::prelude::{ParallelExtend, ParallelIterator};
use serde::Serialize;
use soa_derive::StructOfArray;

use ccsl::lccsl::algo::{
    approx_conflict_map, complexity_from_graph, find_solutions, generate_combinations,
    limit_conflict_map, CountingVisitor,
};
use ccsl::lccsl::automata::label::Label;
use ccsl::lccsl::automata::{StateRef, STS};
use ccsl::lccsl::constraints::Constraint;

pub trait SoAExt<T> {
    fn with_capacity(size: usize) -> Self;
    fn push(&mut self, v: T);
    fn clear(&mut self);
}
pub fn write_graph<N: Display, E: Display, D: EdgeType>(
    g: &Graph<N, E, D>,
    dir: &Path,
    file: &str,
) -> Result<()> {
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
) -> Result<()> {
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
) -> Result<()> {
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
) -> Result<()> {
    let g = g.map(
        |n, w| format!("#:{} {}", n.index(), w),
        |_, w| w.to_string(),
    );
    write_graph(&g, dir, file)
}

pub fn vec_into_vec<I: Into<O>, O>(vec: impl IntoIterator<Item = I>) -> Vec<O> {
    vec.into_iter().map(Into::into).collect_vec()
}

#[derive(Debug, Copy, Clone, Serialize, StructOfArray)]
pub struct SpecCombParams {
    pub spec: u64,
    pub variant: u64,
    pub comb: u64,
    pub size: u8,
    #[nested_soa]
    pub real: Criteria,
    #[nested_soa]
    pub limit: Criteria,
    #[nested_soa]
    pub approx: Criteria,
    pub clocks: u64,
}

impl SoAExt<SpecCombParams> for SpecCombParamsVec {
    fn with_capacity(size: usize) -> Self {
        SpecCombParamsVec::with_capacity(size)
    }

    fn push(&mut self, v: SpecCombParams) {
        SpecCombParamsVec::push(self, v);
    }
    fn clear(&mut self) {
        SpecCombParamsVec::clear(self);
    }
}

impl ParallelExtend<SpecCombParams> for SpecCombParamsVec {
    fn par_extend<I>(&mut self, par_iter: I)
    where
        I: IntoParallelIterator<Item = SpecCombParams>,
    {
        let m = Arc::new(Mutex::new(self));
        par_iter.into_par_iter().for_each(|v| {
            let mut g = m.lock().unwrap();
            g.push(v);
        });
    }
}
impl SoAExt<SquishedParams> for SquishedParamsVec {
    fn with_capacity(size: usize) -> Self {
        SquishedParamsVec::with_capacity(size)
    }

    fn push(&mut self, v: SquishedParams) {
        SquishedParamsVec::push(self, v);
    }
    fn clear(&mut self) {
        SquishedParamsVec::clear(self);
    }
}

impl ParallelExtend<SquishedParams> for SquishedParamsVec {
    fn par_extend<I>(&mut self, par_iter: I)
    where
        I: IntoParallelIterator<Item = SquishedParams>,
    {
        let m = Arc::new(Mutex::new(self));
        par_iter.into_par_iter().for_each(|v| {
            let mut g = m.lock().unwrap();
            g.push(v);
        });
    }
}

pub trait ToArrow: Sized + StructOfArray {
    fn schema() -> Schema;
    fn batch(data: &<Self as StructOfArray>::Type) -> Result<RecordBatch>;
}

impl ToArrow for SpecCombParams {
    fn schema() -> Schema {
        Schema::new(vec![
            Field::new("spec", DataType::UInt64, false),
            Field::new("variant", DataType::UInt64, false),
            Field::new("comb", DataType::UInt64, false),
            Field::new("size", DataType::UInt8, false),
            Field::new("real_tests", DataType::UInt64, false),
            Field::new("real_downs", DataType::UInt64, false),
            Field::new("real_solutions", DataType::UInt64, false),
            Field::new("limit_tests", DataType::UInt64, false),
            Field::new("limit_downs", DataType::UInt64, false),
            Field::new("limit_solutions", DataType::UInt64, false),
            Field::new("approx_tests", DataType::UInt64, false),
            Field::new("approx_downs", DataType::UInt64, false),
            Field::new("approx_solutions", DataType::UInt64, false),
            Field::new("clocks", DataType::UInt64, false),
        ])
    }

    fn batch(data: &SpecCombParamsVec) -> Result<RecordBatch> {
        let spec_vec = Arc::new(UInt64Array::from(data.spec.clone()));
        let var_vec = Arc::new(UInt64Array::from(data.variant.clone()));
        let comb_vec = Arc::new(UInt64Array::from(data.comb.clone()));
        let size_vec = Arc::new(UInt8Array::from(data.size.clone()));

        let real_test_vec: ArrayRef = Arc::new(UInt64Array::from(data.real.test.clone()));
        let limit_test_vec: ArrayRef = Arc::new(UInt64Array::from(data.limit.test.clone()));
        let approx_test_vec: ArrayRef = Arc::new(UInt64Array::from(data.approx.test.clone()));
        let real_down_vec: ArrayRef = Arc::new(UInt64Array::from(data.real.down.clone()));
        let limit_down_vec: ArrayRef = Arc::new(UInt64Array::from(data.limit.down.clone()));
        let approx_down_vec: ArrayRef = Arc::new(UInt64Array::from(data.approx.down.clone()));
        let real_solution_vec: ArrayRef = Arc::new(UInt64Array::from(data.real.solutions.clone()));
        let limit_solution_vec: ArrayRef =
            Arc::new(UInt64Array::from(data.limit.solutions.clone()));
        let approx_solution_vec: ArrayRef =
            Arc::new(UInt64Array::from(data.approx.solutions.clone()));
        let clocks_vec: ArrayRef = Arc::new(UInt64Array::from(data.clocks.clone()));

        let result = RecordBatch::try_new(
            Arc::new(Self::schema()),
            vec![
                spec_vec,
                var_vec,
                comb_vec,
                size_vec,
                real_test_vec,
                real_down_vec,
                real_solution_vec,
                limit_test_vec,
                limit_down_vec,
                limit_solution_vec,
                approx_test_vec,
                approx_down_vec,
                approx_solution_vec,
                clocks_vec,
            ],
        )?;
        Ok(result)
    }
}

#[derive(Debug, Copy, Clone, Serialize, StructOfArray)]
pub struct SquishedParams {
    pub spec: u64,
    pub variant: u64,
    pub size: u8,
    #[nested_soa]
    pub limit: Criteria,
    #[nested_soa]
    pub approx: Criteria,
}

impl ToArrow for SquishedParams {
    fn schema() -> Schema {
        Schema::new(vec![
            Field::new("spec", DataType::UInt64, false),
            Field::new("variant", DataType::UInt64, false),
            Field::new("size", DataType::UInt8, false),
            Field::new("limit_tests", DataType::UInt64, false),
            Field::new("limit_downs", DataType::UInt64, false),
            Field::new("limit_solutions", DataType::UInt64, false),
            Field::new("approx_tests", DataType::UInt64, false),
            Field::new("approx_downs", DataType::UInt64, false),
            Field::new("approx_solutions", DataType::UInt64, false),
        ])
    }

    fn batch(data: &SquishedParamsVec) -> Result<RecordBatch> {
        let spec_vec: ArrayRef = Arc::new(UInt64Array::from(data.spec.clone()));
        let var_vec: ArrayRef = Arc::new(UInt64Array::from(data.variant.clone()));
        let size_vec: ArrayRef = Arc::new(UInt8Array::from(data.size.clone()));

        let limit_test_vec: ArrayRef = Arc::new(UInt64Array::from(data.limit.test.clone()));
        let approx_test_vec: ArrayRef = Arc::new(UInt64Array::from(data.approx.test.clone()));
        let limit_down_vec: ArrayRef = Arc::new(UInt64Array::from(data.limit.down.clone()));
        let approx_down_vec: ArrayRef = Arc::new(UInt64Array::from(data.approx.down.clone()));
        let limit_solution_vec: ArrayRef =
            Arc::new(UInt64Array::from(data.limit.solutions.clone()));
        let approx_solution_vec: ArrayRef =
            Arc::new(UInt64Array::from(data.approx.solutions.clone()));

        let result = RecordBatch::try_new(
            Arc::new(Self::schema()),
            vec![
                spec_vec,
                var_vec,
                size_vec,
                limit_test_vec,
                limit_down_vec,
                limit_solution_vec,
                approx_test_vec,
                approx_down_vec,
                approx_solution_vec,
            ],
        )?;
        Ok(result)
    }
}

#[derive(Debug, Copy, Clone, Serialize, StructOfArray)]
pub struct Criteria {
    pub test: u64,
    pub down: u64,
    pub solutions: u64,
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
    C: Hash + Clone + Ord + Display + fmt::Debug + Sync + Send,
    L: Label<C> + Clone + Hash + Eq + Sync,
    for<'c, 'd> &'c L: BitOr<&'d L, Output = L>,
{
    let mut visitor = CountingVisitor::default();
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
            test: visitor.test as u64,
            down: visitor.down as u64,
            solutions: actual as u64,
        },
        limit: Criteria {
            test: limit.tests as u64,
            down: limit.downs as u64,
            solutions: limit.solutions as u64,
        },
        approx: Criteria {
            test: approx.tests as u64,
            down: approx.downs as u64,
            solutions: approx.solutions as u64,
        },
        clocks: spec.iter().flat_map(|c| c.clocks().iter()).unique().count() as u64,
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
            test: limit.tests as u64,
            down: limit.downs as u64,
            solutions: limit.solutions as u64,
        },
        approx: Criteria {
            test: approx.tests as u64,
            down: approx.downs as u64,
            solutions: approx.solutions as u64,
        },
    }
}
pub fn analyze_specification<C, L>(
    spec: &[STS<C, L>],
    spec_id: u64,
    perm_id: u64,
) -> (Vec<SpecCombParams>, SquishedParams)
where
    C: Hash + Clone + Ord + Display + fmt::Debug + Sync + Send,
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

#[macro_export]
macro_rules! collection {
    // map-like
    ($($k:expr => $v:expr),* $(,)?) => {
        std::iter::Iterator::collect(IntoIterator::into_iter([$(($k, $v),)*]))
    };
    // set-like
    ($($v:expr),* $(,)?) => {
        std::iter::Iterator::collect(IntoIterator::into_iter([$($v,)*]))
    };
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
            Permutation::oneline(
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

pub fn stream_to_parquet_flat<I, O, SOA, F>(
    filepath: &Path,
    stream: impl IntoIterator<Item = I>,
    map: F,
    chunks: usize,
    arrow_buffer: usize,
) -> Result<()>
where
    I: Sized + Sync,
    O: ToArrow + Send + StructOfArray<Type = SOA>,
    SOA: SoAExt<O> + ParallelExtend<O>,
    F: Fn(&I) -> Vec<O> + Sync + Send + Clone,
{
    let schema = Arc::new(O::schema());
    let mut parquet_wrt = ArrowWriter::try_new(File::create(filepath)?, schema, None)?;

    let mut in_buffer: Vec<I> = Vec::with_capacity(chunks);
    let mut out_buffer = SOA::with_capacity(arrow_buffer);

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

pub fn stream_to_parquet<I, O, SOA, F>(
    filepath: &Path,
    stream: impl IntoIterator<Item = I>,
    map: F,
    chunks: usize,
) -> Result<()>
where
    I: Sized + Sync,
    O: ToArrow + Send + StructOfArray<Type = SOA>,
    SOA: SoAExt<O> + ParallelExtend<O>,
    F: Fn(&I) -> O + Sync + Send + Clone,
{
    let schema = Arc::new(O::schema());
    let mut parquet_wrt = ArrowWriter::try_new(File::create(filepath)?, schema, None)?;

    let mut in_buffer: Vec<I> = Vec::with_capacity(chunks);
    let mut out_buffer = SOA::with_capacity(chunks);

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
