use absint::interval::{IntervalImmediateNarrowing, StandardWidening};
use ccsl::kernel::constraints::{Delay, Precedence, Specification};
use ccsl::symbolic::{assume, interpret, Invariant, StateWidening};

fn main() {
    let inv: Invariant<usize> = (&Delay {
        out: 0,
        trigger: 1,
        delay: 1,
        on: None,
    })
        .into();
    let pos = assume(&inv.0, true);
    println!("{}", pos);

    let spec: Specification<usize> = vec![
        Precedence {
            cause: 0,
            effect: 1,
            init: None,
            max: None,
        }
        .into(),
        Precedence {
            cause: 1,
            effect: 2,
            init: None,
            max: None,
        }
        .into(),
        Delay {
            out: 2,
            trigger: 0,
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
