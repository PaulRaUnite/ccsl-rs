use crate::lccsl::constraints::Constraint;
use itertools::Itertools;
use std::collections::BTreeSet;
use std::fmt::Display;

pub fn to_lccsl<C>(spec: &[Constraint<C>], name: &str) -> String
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
            .map(|c| format!("        {}", c.to_lccsl()))
            .join("\n"),
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lccsl::constraints::{
        Causality, Delay, Exclusion, Infinity, Intersection, Minus, Precedence, Repeat,
        Subclocking, Supremum, Union,
    };
    use crate::lccsl::generation::random_connected_specification;
    use crate::lccsl::parser::parse_raw;

    #[test]
    fn lccsl_roundtrip() {
        let spec: Vec<Constraint<&str>> = vec![
            Precedence {
                left: "a",
                right: "b",
                init: None,
                max: None,
            }
            .into(),
            Causality {
                left: "a",
                right: "b",
                init: None,
                max: None,
            }
            .into(),
            Subclocking {
                left: "a",
                right: "b",
            }
            .into(),
            Exclusion {
                clocks: vec!["a", "b", "c"].into_iter().collect(),
            }
            .into(),
            Infinity {
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
                every: 3,
                base: "b",
                from: None,
                up_to: None,
            }
            .into(),
            Delay {
                out: "a",
                base: "b",
                delay: 5,
                on: None,
            }
            .into(),
        ];

        let lccsl_format = to_lccsl(&spec, "test");
        let result = parse_raw(&lccsl_format);

        assert!(matches!(result, Ok(_)), "{:?}", result);
    }

    #[test]
    fn random_fixed() {
        let spec = random_connected_specification(0, 5, true)
            .into_iter()
            .map(|c| c.map(&mut |clock| format!("c{}", clock)))
            .collect_vec();
        let lccsl_format = to_lccsl(&spec, "test");
        let result = parse_raw(&lccsl_format);

        assert!(matches!(result, Ok(_)));
    }
    #[test]
    fn random_unfixed() {
        let spec = random_connected_specification(0, 5, false)
            .into_iter()
            .map(|c| c.map(&mut |clock| format!("c{}", clock)))
            .collect_vec();
        let lccsl_format = to_lccsl(&spec, "test");
        let result = parse_raw(&lccsl_format);

        assert!(matches!(result, Ok(_)));
    }
}
