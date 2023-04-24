use crate::kernel::constraints::Constraint;
use itertools::Itertools;
use std::collections::BTreeSet;
use std::fmt::Display;

pub fn render<C>(spec: &[Constraint<C>], name: &str) -> String
where
    C: Display + Ord,
{
    let clocks: BTreeSet<&C> = spec.iter().flat_map(|c| c.clocks().into_iter()).collect();
    format!(
        "\
Specification {} {{
    Clock {}
    [
{}
    ]
}}
",
        name,
        clocks.into_iter().join(","),
        spec.iter()
            .map(|c| format!("        {}", render_constraint(c)))
            .join("\n"),
    )
}

// TODO: add Display (?) newtype to get rid of unnecessary allocations (low priority)
// TODO: I guess it should be a method (something like `dot_format`)
pub fn render_constraint<C>(c: &Constraint<C>) -> String
where
    C: Display,
{
    match c {
        Constraint::Coincidence(c) => format!("Let {} be {}", c.left, c.right),
        Constraint::Causality(c) => format!(
            "Precedence {} <={} {}",
            c.cause,
            format_causal_params(&c.init, &c.max),
            c.effect
        ),
        Constraint::Precedence(c) => format!(
            "Precedence {} <{} {}",
            c.cause,
            format_causal_params(&c.init, &c.max),
            c.effect
        ),
        Constraint::SubClock(c) => format!("SubClocking {} <- {}", c.sub, c.sup),
        Constraint::Exclusion(c) => format!("Exclusion {}", c.clocks.iter().join(" # ")),
        Constraint::Infimum(c) => format!("Let {} be inf({}, {})", c.out, c.left, c.right),
        Constraint::Supremum(c) => format!("Let {} be sup({}, {})", c.out, c.left, c.right),
        Constraint::Union(c) => format!("Let {} be {}", c.out, c.args.iter().join(" + ")),
        Constraint::Intersection(c) => {
            format!("Let {} be {}", c.out, c.args.iter().join(" * "))
        }
        Constraint::Minus(c) => format!("Let {} be {} - {}", c.out, c.left, c.right,),
        Constraint::Repeat(c) => format!(
            "repeat {} every {} {} {} {}",
            c.out,
            c.every,
            c.base,
            c.from.map_or("".to_owned(), |v| format!("from {}", v)),
            c.up_to.map_or("".to_owned(), |v| format!("upTo {}", v))
        ),
        Constraint::Delay(c) => format!(
            "{} = {} $ {} {}",
            c.out,
            c.trigger,
            c.delay,
            c.on.as_ref().map_or("".to_owned(), |on| on.to_string())
        ),
        Constraint::SampleOn(c) => format!("{} = {} sampleOn {}", c.out, c.trigger, c.base),
        Constraint::Diff(c) => format!(
            "{} = {} [{}, {}]",
            c.out,
            c.base,
            c.from,
            c.up_to
                .as_ref()
                .map_or("Infinity".to_owned(), ToString::to_string)
        ),
    }
}

fn format_causal_params(init: &Option<usize>, max: &Option<usize>) -> String {
    match (init, max) {
        (Some(init), Some(max)) => format!("(init: {} max: {})", init, max),
        (None, Some(max)) => format!("(max: {})", max),
        (Some(init), None) => format!("(init: {})", init),
        (None, None) => "".to_string(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::kernel::constraints::{
        Causality, Delay, Exclusion, Infimum, Intersection, Minus, Precedence, Repeat, Subclocking,
        Supremum, Union,
    };
    use crate::lccsl::parser::parse_raw;

    #[test]
    fn lccsl_roundtrip() {
        let spec: Vec<Constraint<&str>> = vec![
            Precedence {
                cause: "a",
                effect: "b",
                init: None,
                max: None,
            }
            .into(),
            Causality {
                cause: "a",
                effect: "b",
                init: None,
                max: None,
            }
            .into(),
            Subclocking { sub: "a", sup: "b" }.into(),
            Exclusion {
                clocks: vec!["a", "b", "c"].into_iter().collect(),
            }
            .into(),
            Infimum {
                out: "c",
                left: "a",
                right: "b",
            }
            .into(),
            Supremum {
                out: "c",
                left: "a",
                right: "b",
            }
            .into(),
            Union {
                out: "a",
                args: vec!["a", "b"].into_iter().collect(),
            }
            .into(),
            Intersection {
                out: "a",
                args: vec!["a", "b"].into_iter().collect(),
            }
            .into(),
            Minus {
                out: "c",
                left: "a",
                right: "b",
            }
            .into(),
            Repeat {
                out: "a",
                every: 1,
                base: "b",
                from: Some(0),
                up_to: Some(2),
            }
            .into(),
            Repeat {
                out: "a",
                every: 3,
                base: "b",
                from: None,
                up_to: None,
            }
            .into(),
            Delay {
                out: "a",
                trigger: "b",
                delay: 5,
                on: None,
            }
            .into(),
        ];

        let lccsl_format = render(&spec, "test");
        let result = parse_raw(&lccsl_format);

        assert!(matches!(result, Ok(_)), "{:?}", result);
    }
}
