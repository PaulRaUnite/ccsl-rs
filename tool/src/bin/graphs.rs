use std::error::Error;

use ccsl::lccsl::algo::{squished_conflict_map, squished_limit_map, unidirect_squished_map};
use kernel::automata::label::StaticBitmapLabel;
use kernel::constraints::{Causality, Constraint, Delay, Precedence};
use tool::write_graph;

fn main() -> Result<(), Box<dyn Error>> {
    let spec: Vec<Constraint<u32>> = vec![
        Precedence {
            left: 0u32,
            right: 1u32,
            init: None,
            max: None,
        }
        .into(),
        Causality {
            left: 1u32,
            right: 2u32,
            init: None,
            max: None,
        }
        .into(),
        Delay {
            out: 2u32,
            base: 0u32,
            delay: 1,
            on: None,
        }
        .into(),
    ];
    let approx = squished_conflict_map::<u32, StaticBitmapLabel>(&spec);
    let limit = squished_limit_map::<u32, StaticBitmapLabel>(&spec);
    let undirected = unidirect_squished_map::<u32, StaticBitmapLabel>(&spec);

    let dir = "./tool/graph/";
    write_graph(&approx, dir.as_ref(), "approx")?;
    write_graph(&limit, dir.as_ref(), "limit")?;
    write_graph(&undirected, dir.as_ref(), "undirected")?;
    Ok(())
}
