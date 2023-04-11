use absint::interval::{IntervalImmediateNarrowing, StandardWidening};
use ccsl::analysis::{assume, interpret, Invariant, StateWidening};
use ccsl::kernel::constraints::{Delay, Precedence, Specification};

fn main() {
    let inv: Invariant<usize> = (&Delay {
        out: 0,
        base: 1,
        delay: 1,
        on: None,
    })
        .into();
    let pos = assume(&inv.0, true);
    println!("{}", pos);

    let spec: Specification<usize> = vec![
        Precedence {
            left: 0,
            right: 1,
            init: None,
            max: None,
        }
        .into(),
        Precedence {
            left: 1,
            right: 2,
            init: None,
            max: None,
        }
        .into(),
        Delay {
            out: 2,
            base: 0,
            delay: 1,
            on: None,
        }
        .into(),
    ]
    .into();

    println!("Start");
    for (k, v) in interpret::<
        usize,
        StateWidening<usize, StandardWidening<i64>>,
        StateWidening<usize, IntervalImmediateNarrowing<i64>>,
    >(&spec)
    {
        println!("{:15}, {}", k, v);
    }
}
