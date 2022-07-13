use crate::lccsl::automata::Guard;
use std::fmt;
use std::fmt::{Display, Formatter};
use std::ops::{Add, BitAnd, BitOr, BitXor, Index, Mul, Not, Sub};
use std::sync::Arc;

#[derive(Debug, Copy, Clone)]
pub enum IntegerArithmeticsKind {
    Add,
    Sub,
    Mul,
    //Div,
    //Mod,
}
#[derive(Debug, Clone)]
pub enum IntegerExpression<V, C = i64> {
    Variable(V),
    Constant(C),
    IntegerBinary {
        kind: IntegerArithmeticsKind,
        left: Arc<IntegerExpression<V, C>>,
        right: Arc<IntegerExpression<V, C>>,
    },
}

impl<V: Display, C: Display> Display for IntegerExpression<V, C> {
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

impl<V, C> Add for IntegerExpression<V, C> {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        IntegerExpression::IntegerBinary {
            kind: IntegerArithmeticsKind::Add,
            left: Arc::new(self),
            right: Arc::new(rhs),
        }
    }
}

impl<V, C> Sub for IntegerExpression<V, C> {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        IntegerExpression::IntegerBinary {
            kind: IntegerArithmeticsKind::Sub,
            left: Arc::new(self),
            right: Arc::new(rhs),
        }
    }
}

impl<V, C> Mul for IntegerExpression<V, C> {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        IntegerExpression::IntegerBinary {
            kind: IntegerArithmeticsKind::Mul,
            left: Arc::new(self),
            right: Arc::new(rhs),
        }
    }
}

impl<V, C> From<C> for IntegerExpression<V, C> {
    fn from(n: C) -> Self {
        IntegerExpression::Constant(n)
    }
}

impl<V, C> IntegerExpression<V, C>
where
    C: Clone + Add<Output = C> + Sub<Output = C> + Mul<Output = C>,
{
    fn eval<'a>(&'a self, state: &'a dyn Index<&'a V, Output = C>) -> C {
        match &self {
            IntegerExpression::Variable(v) => state[v].clone(),
            IntegerExpression::Constant(c) => c.clone(),
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
}
impl<V, C> IntegerExpression<V, C>
where
    C: Clone,
{
    pub fn var(v: V) -> Self {
        IntegerExpression::Variable(v)
    }

    pub fn fixed(c: C) -> Self {
        IntegerExpression::Constant(c)
    }

    fn cmp(
        kind: IntegerComparisonKind,
        left: IntegerExpression<V, C>,
        right: IntegerExpression<V, C>,
    ) -> BooleanExpression<V, C> {
        BooleanExpression::IntegerBinary {
            kind,
            left: left.into(),
            right: right.into(),
        }
    }
}

impl<V: Clone, C: Clone> IntegerExpression<V, C> {
    pub fn eq(&self, rhs: impl Into<Self>) -> BooleanExpression<V, C> {
        IntegerExpression::cmp(IntegerComparisonKind::Equal, self.clone(), rhs.into())
    }
    pub fn less(&self, rhs: impl Into<Self>) -> BooleanExpression<V, C> {
        IntegerExpression::cmp(IntegerComparisonKind::Less, self.clone(), rhs.into())
    }
    pub fn less_eq(&self, rhs: impl Into<Self>) -> BooleanExpression<V, C> {
        IntegerExpression::cmp(IntegerComparisonKind::LessEq, self.clone(), rhs.into())
    }
    pub fn more(&self, rhs: impl Into<Self>) -> BooleanExpression<V, C> {
        IntegerExpression::cmp(IntegerComparisonKind::More, self.clone(), rhs.into())
    }
    pub fn more_eq(&self, rhs: impl Into<Self>) -> BooleanExpression<V, C> {
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
    Eq,
}

#[derive(Debug, Clone)]
pub enum BooleanExpression<V, C = i64> {
    IntegerBinary {
        kind: IntegerComparisonKind,
        left: Box<IntegerExpression<V, C>>,
        right: Box<IntegerExpression<V, C>>,
    },
    BooleanBinary {
        kind: BooleanComparisonKind,
        left: Box<BooleanExpression<V, C>>,
        right: Box<BooleanExpression<V, C>>,
    },
    Not(Box<BooleanExpression<V, C>>),
    Constant(bool),
}

impl<V: Display, C: Display> Display for BooleanExpression<V, C> {
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
                    BooleanComparisonKind::Eq => "==",
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

impl<V, C> Guard<V, C> for BooleanExpression<V, C>
where
    C: Clone + Ord + Add<Output = C> + Sub<Output = C> + Mul<Output = C>,
{
    fn eval<'a>(&'a self, state: &'a dyn Index<&'a V, Output = C>) -> bool {
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
                    BooleanComparisonKind::Eq => left == right,
                }
            }
            BooleanExpression::Not(e) => !e.eval(state),
            BooleanExpression::Constant(b) => *b,
        }
    }
}

impl<V, C> BitOr for BooleanExpression<V, C> {
    type Output = Self;

    fn bitor(self, rhs: Self) -> Self::Output {
        BooleanExpression::BooleanBinary {
            kind: BooleanComparisonKind::Or,
            left: self.into(),
            right: rhs.into(),
        }
    }
}

impl<V, C> BitAnd for BooleanExpression<V, C> {
    type Output = Self;

    fn bitand(self, rhs: Self) -> Self::Output {
        BooleanExpression::BooleanBinary {
            kind: BooleanComparisonKind::And,
            left: self.into(),
            right: rhs.into(),
        }
    }
}

impl<V, C> BitXor for BooleanExpression<V, C> {
    type Output = Self;

    fn bitxor(self, rhs: Self) -> Self::Output {
        BooleanExpression::BooleanBinary {
            kind: BooleanComparisonKind::Xor,
            left: self.into(),
            right: rhs.into(),
        }
    }
}

impl<V, C> Not for BooleanExpression<V, C> {
    type Output = Self;

    fn not(self) -> Self::Output {
        BooleanExpression::Not(Box::new(self))
    }
}

impl<V, C> Default for BooleanExpression<V, C> {
    fn default() -> Self {
        BooleanExpression::Constant(true)
    }
}

impl<V, C> BooleanExpression<V, C> {
    pub fn implies(self, other: BooleanExpression<V, C>) -> BooleanExpression<V, C> {
        !self | other
    }

    pub fn strictly_implies(self, other: BooleanExpression<V, C>) -> BooleanExpression<V, C> {
        self & other
    }

    pub fn eq(self, other: BooleanExpression<V, C>) -> BooleanExpression<V, C> {
        BooleanExpression::BooleanBinary {
            kind: BooleanComparisonKind::Eq,
            left: self.into(),
            right: other.into(),
        }
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

impl<V, C> IntegerExpression<V, C>
where
    V: Clone,
    C: Clone,
{
    pub fn leaves(&self, variables: &mut Vec<V>, constants: &mut Vec<C>) {
        match self {
            IntegerExpression::Variable(v) => {
                variables.push(v.clone());
            }
            IntegerExpression::Constant(c) => constants.push(c.clone()),
            IntegerExpression::IntegerBinary { left, right, .. } => {
                left.leaves(variables, constants);
                right.leaves(variables, constants);
            }
        }
    }
}

impl<V, C> BooleanExpression<V, C>
where
    V: Clone,
    C: Clone,
{
    pub fn leaves(&self, variables: &mut Vec<V>, integers: &mut Vec<C>, booleans: &mut Vec<bool>) {
        match self {
            BooleanExpression::IntegerBinary { left, right, .. } => {
                left.leaves(variables, integers);
                right.leaves(variables, integers);
            }
            BooleanExpression::BooleanBinary { left, right, .. } => {
                left.leaves(variables, integers, booleans);
                right.leaves(variables, integers, booleans);
            }
            BooleanExpression::Not(expr) => {
                expr.leaves(variables, integers, booleans);
            }
            BooleanExpression::Constant(c) => {
                booleans.push(*c);
            }
        }
    }
}
