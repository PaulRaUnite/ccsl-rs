use crate::lccsl::automata::Guard;
use std::fmt;
use std::fmt::Formatter;
use std::ops::{Add, BitAnd, BitOr, BitXor, Index, Mul, Sub};
use std::rc::Rc;

#[derive(Debug, Copy, Clone)]
pub enum IntegerArithmeticsKind {
    Add,
    Sub,
    Mul,
    //Div,
    //Mod,
}
#[derive(Debug, Clone)]
pub enum IntegerExpression<Idx> {
    Variable(Idx),
    Constant(i64),
    IntegerBinary {
        kind: IntegerArithmeticsKind,
        left: Rc<IntegerExpression<Idx>>,
        right: Rc<IntegerExpression<Idx>>,
    },
}

impl<Idx: fmt::Display> fmt::Display for IntegerExpression<Idx> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            IntegerExpression::Variable(v) => write!(f, "{}", v),
            IntegerExpression::Constant(c) => write!(f, "{}", c),
            IntegerExpression::IntegerBinary { kind, left, right } => {
                let sign = match kind {
                    IntegerArithmeticsKind::Add => "+",
                    IntegerArithmeticsKind::Sub => "-",
                    IntegerArithmeticsKind::Mul => "*",
                };
                write!(f, "{} {} {}", left, sign, right)
            }
        }
    }
}

impl<D> Add for IntegerExpression<D> {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        IntegerExpression::IntegerBinary {
            kind: IntegerArithmeticsKind::Add,
            left: Rc::new(self),
            right: Rc::new(rhs),
        }
    }
}

impl<D> Sub for IntegerExpression<D> {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        IntegerExpression::IntegerBinary {
            kind: IntegerArithmeticsKind::Sub,
            left: Rc::new(self),
            right: Rc::new(rhs),
        }
    }
}

impl<D> Mul for IntegerExpression<D> {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        IntegerExpression::IntegerBinary {
            kind: IntegerArithmeticsKind::Mul,
            left: Rc::new(self),
            right: Rc::new(rhs),
        }
    }
}

impl<D> From<i64> for IntegerExpression<D> {
    fn from(n: i64) -> Self {
        IntegerExpression::Constant(n)
    }
}

impl<D> IntegerExpression<D> {
    fn eval<'a>(&'a self, state: &'a dyn Index<&'a D, Output = i64>) -> i64 {
        match &self {
            IntegerExpression::Variable(v) => state[v],
            IntegerExpression::Constant(c) => *c,
            IntegerExpression::IntegerBinary { kind, left, right } => {
                let left = left.eval(state);
                let right = right.eval(state);
                match &kind {
                    IntegerArithmeticsKind::Add => left + right,
                    IntegerArithmeticsKind::Sub => left - right,
                    IntegerArithmeticsKind::Mul => left * right,
                }
            }
        }
    }

    pub fn var(v: D) -> IntegerExpression<D> {
        IntegerExpression::Variable(v)
    }

    pub fn fixed(c: i64) -> IntegerExpression<D> {
        IntegerExpression::Constant(c)
    }

    fn cmp(
        kind: IntegerComparisonKind,
        left: IntegerExpression<D>,
        right: IntegerExpression<D>,
    ) -> BooleanExpression<D> {
        BooleanExpression::IntegerBinary {
            kind,
            left: left.into(),
            right: right.into(),
        }
    }
}

impl<D: Clone> IntegerExpression<D> {
    pub fn eq(&self, rhs: impl Into<Self>) -> BooleanExpression<D> {
        IntegerExpression::cmp(IntegerComparisonKind::Equal, self.clone(), rhs.into())
    }
    pub fn less(&self, rhs: impl Into<Self>) -> BooleanExpression<D> {
        IntegerExpression::cmp(IntegerComparisonKind::Less, self.clone(), rhs.into())
    }
    pub fn less_eq(&self, rhs: impl Into<Self>) -> BooleanExpression<D> {
        IntegerExpression::cmp(IntegerComparisonKind::LessEq, self.clone(), rhs.into())
    }
    pub fn more(&self, rhs: impl Into<Self>) -> BooleanExpression<D> {
        IntegerExpression::cmp(IntegerComparisonKind::More, self.clone(), rhs.into())
    }
    pub fn more_eq(&self, rhs: impl Into<Self>) -> BooleanExpression<D> {
        IntegerExpression::cmp(IntegerComparisonKind::MoreEq, self.clone(), rhs.into())
    }
}

#[derive(Debug, Copy, Clone)]
pub enum IntegerComparisonKind {
    Equal,
    Less,
    LessEq,
    More,
    MoreEq,
}
#[derive(Debug, Copy, Clone)]
pub enum BooleanComparisonKind {
    And,
    Or,
    Xor,
}

#[derive(Debug, Clone)]
pub enum BooleanExpression<Idx> {
    IntegerBinary {
        kind: IntegerComparisonKind,
        left: Box<IntegerExpression<Idx>>,
        right: Box<IntegerExpression<Idx>>,
    },
    BooleanBinary {
        kind: BooleanComparisonKind,
        left: Box<BooleanExpression<Idx>>,
        right: Box<BooleanExpression<Idx>>,
    },
    Not(Box<BooleanExpression<Idx>>),
    Constant(bool),
}

impl<D: fmt::Display> fmt::Display for BooleanExpression<D> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            BooleanExpression::IntegerBinary { kind, left, right } => {
                let sign = match kind {
                    IntegerComparisonKind::Equal => "==",
                    IntegerComparisonKind::Less => "<",
                    IntegerComparisonKind::LessEq => "<=",
                    IntegerComparisonKind::More => ">",
                    IntegerComparisonKind::MoreEq => ">=",
                };
                write!(f, "{} {} {}", left, sign, right)
            }
            BooleanExpression::BooleanBinary { kind, left, right } => {
                let sign = match kind {
                    BooleanComparisonKind::And => "&&",
                    BooleanComparisonKind::Or => "||",
                    BooleanComparisonKind::Xor => "^",
                };
                write!(f, "{} {} {}", left, sign, right)
            }
            BooleanExpression::Not(exp) => write!(f, "!{}", exp),
            BooleanExpression::Constant(b) => {
                write!(f, "{}", b)
            }
        }
    }
}

impl<D> Guard<D> for BooleanExpression<D> {
    fn eval<'a>(&'a self, state: &'a dyn Index<&'a D, Output = i64>) -> bool {
        match &self {
            BooleanExpression::IntegerBinary { kind, left, right } => {
                let left = left.eval(state);
                let right = right.eval(state);
                match &kind {
                    IntegerComparisonKind::Equal => left == right,
                    IntegerComparisonKind::Less => left < right,
                    IntegerComparisonKind::LessEq => left <= right,
                    IntegerComparisonKind::More => left > right,
                    IntegerComparisonKind::MoreEq => left >= right,
                }
            }
            BooleanExpression::BooleanBinary { kind, left, right } => {
                let left = left.eval(state);
                let right = right.eval(state);
                match &kind {
                    BooleanComparisonKind::And => left && right,
                    BooleanComparisonKind::Or => left || right,
                    BooleanComparisonKind::Xor => left ^ right,
                }
            }
            BooleanExpression::Not(e) => !e.eval(state),
            BooleanExpression::Constant(b) => *b,
        }
    }
}

impl<D> BitOr for BooleanExpression<D> {
    type Output = Self;

    fn bitor(self, rhs: Self) -> Self::Output {
        BooleanExpression::BooleanBinary {
            kind: BooleanComparisonKind::Or,
            left: self.into(),
            right: rhs.into(),
        }
    }
}

impl<D> BitAnd for BooleanExpression<D> {
    type Output = Self;

    fn bitand(self, rhs: Self) -> Self::Output {
        BooleanExpression::BooleanBinary {
            kind: BooleanComparisonKind::And,
            left: self.into(),
            right: rhs.into(),
        }
    }
}

impl<D> BitXor for BooleanExpression<D> {
    type Output = Self;

    fn bitxor(self, rhs: Self) -> Self::Output {
        BooleanExpression::BooleanBinary {
            kind: BooleanComparisonKind::Xor,
            left: self.into(),
            right: rhs.into(),
        }
    }
}

impl<D> Default for BooleanExpression<D> {
    fn default() -> Self {
        BooleanExpression::Constant(true)
    }
}

#[derive(Debug, Clone)]
pub struct Switch<G, R> {
    variants: Vec<(G, R)>,
    default: Option<(G, R)>,
}

impl<G: Default, R> Switch<G, R> {
    pub fn new() -> Self {
        Self {
            variants: vec![],
            default: None,
        }
    }

    pub fn add_variant(&mut self, guard: G, result: R) {
        self.variants.push((guard, result))
    }
    pub fn add_default_variant(&mut self, result: R) {
        if self.default.is_some() {
            panic!("can only be one default for a switch");
        } else {
            self.default = Some((G::default(), result));
        }
    }
}

impl<G, R> Switch<G, R> {
    pub fn variants(&self) -> impl Iterator<Item = &(G, R)> + Clone {
        self.variants.iter().chain(self.default.iter())
    }
}
