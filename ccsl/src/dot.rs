use crate::kernel::constraints::Specification;
use std::fmt;
use std::fmt::{Display, Write};

fn render_to<C: Display, W: Write>(spec: &Specification<C>) -> fmt::Result {
    // TODO:
    //   - translate into a graph with special labels
    //   - write dot trait implementations for it
    //   - use dot
    unimplemented!();
    Ok(())
}
