use std::fmt;
use std::fmt::{Display, Formatter};
use std::ops::{Add, BitAnd, BitOr, BitXor, Mul, Not, Sub};
use std::sync::Arc;

#[derive(Debug, Copy, Clone)]
pub enum IntegerArithmeticsKind {
    Add,
    Sub,
    Mul,
    //Div,
    //Mod,
}

// TODO: never use the whole expressiveness of integer arithmetics anywhere, should remove
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

impl<'a, VI: 'a, C> IntegerExpression<VI, C>
where
    C: Clone + Ord + Add<Output = C> + Sub<Output = C> + Mul<Output = C>,
{
    fn eval(&'a self, int_state: &impl Fn(&VI) -> C) -> C {
        match &self {
            IntegerExpression::Variable(v) => int_state(v),
            IntegerExpression::Constant(c) => c.clone(),
            IntegerExpression::IntegerBinary { kind, left, right } => {
                let left = left.eval(int_state);
                let right = right.eval(int_state);
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
    pub fn var(v: impl Into<V>) -> Self {
        IntegerExpression::Variable(v.into())
    }

    pub fn fixed(c: impl Into<C>) -> Self {
        IntegerExpression::Constant(c.into())
    }

    fn cmp<VB, B>(
        kind: IntegerComparisonKind,
        left: IntegerExpression<V, C>,
        right: IntegerExpression<V, C>,
    ) -> BooleanExpression<V, VB, C, B> {
        BooleanExpression::IntegerBinary {
            kind,
            left: left.into(),
            right: right.into(),
        }
    }
}

impl<VI: Clone, C: Clone> IntegerExpression<VI, C> {
    pub fn eq<VB, B>(&self, rhs: impl Into<Self>) -> BooleanExpression<VI, VB, C, B> {
        IntegerExpression::cmp(IntegerComparisonKind::Equal, self.clone(), rhs.into())
    }
    pub fn less<VB, B>(&self, rhs: impl Into<Self>) -> BooleanExpression<VI, VB, C, B> {
        IntegerExpression::cmp(IntegerComparisonKind::Less, self.clone(), rhs.into())
    }
    pub fn less_eq<VB, B>(&self, rhs: impl Into<Self>) -> BooleanExpression<VI, VB, C, B> {
        IntegerExpression::cmp(IntegerComparisonKind::LessEq, self.clone(), rhs.into())
    }
    pub fn more<VB, B>(&self, rhs: impl Into<Self>) -> BooleanExpression<VI, VB, C, B> {
        IntegerExpression::cmp(IntegerComparisonKind::More, self.clone(), rhs.into())
    }
    pub fn more_eq<VB, B>(&self, rhs: impl Into<Self>) -> BooleanExpression<VI, VB, C, B> {
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

impl IntegerComparisonKind {
    pub fn inverse(self) -> Self {
        match self {
            IntegerComparisonKind::Equal => IntegerComparisonKind::Equal,
            IntegerComparisonKind::Less => IntegerComparisonKind::MoreEq,
            IntegerComparisonKind::LessEq => IntegerComparisonKind::More,
            IntegerComparisonKind::More => IntegerComparisonKind::LessEq,
            IntegerComparisonKind::MoreEq => IntegerComparisonKind::Less,
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub enum BooleanComparisonKind {
    And,
    Or,
    Xor,
    Eq,
}

#[derive(Debug, Clone)]
pub enum BooleanExpression<VI, VB, C = i64, B = bool> {
    IntegerBinary {
        kind: IntegerComparisonKind,
        left: Box<IntegerExpression<VI, C>>,
        right: Box<IntegerExpression<VI, C>>,
    },
    BooleanBinary {
        kind: BooleanComparisonKind,
        left: Box<BooleanExpression<VI, VB, C, B>>,
        right: Box<BooleanExpression<VI, VB, C, B>>,
    },
    Not(Box<BooleanExpression<VI, VB, C, B>>),
    Constant(B),
    Variable(VB),
}

impl<VI: Display, VB: Display, C: Display, B: Display> Display for BooleanExpression<VI, VB, C, B> {
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
            BooleanExpression::Constant(b) => write!(f, "{}", b),
            BooleanExpression::Variable(v) => write!(f, "{}", v),
        }
    }
}

impl<VI, VB, C, B> BooleanExpression<VI, VB, C, B>
where
    C: Clone + Ord + Add<Output = C> + Sub<Output = C> + Mul<Output = C>,
    B: Clone + Into<bool>,
{
    pub fn eval(&self, int_state: &impl Fn(&VI) -> C, bool_state: &impl Fn(&VB) -> bool) -> bool {
        match &self {
            BooleanExpression::IntegerBinary { kind, left, right } => {
                let left = left.eval(int_state);
                let right = right.eval(int_state);
                match &kind {
                    IntegerComparisonKind::Equal => left == right,
                    IntegerComparisonKind::Less => left < right,
                    IntegerComparisonKind::LessEq => left <= right,
                    IntegerComparisonKind::More => left > right,
                    IntegerComparisonKind::MoreEq => left >= right,
                }
            }
            BooleanExpression::BooleanBinary { kind, left, right } => {
                let left = left.eval(int_state, bool_state);
                let right = right.eval(int_state, bool_state);
                match &kind {
                    BooleanComparisonKind::And => left && right,
                    BooleanComparisonKind::Or => left || right,
                    BooleanComparisonKind::Xor => left ^ right,
                    BooleanComparisonKind::Eq => left == right,
                }
            }
            BooleanExpression::Not(e) => !e.eval(int_state, bool_state),
            BooleanExpression::Constant(b) => b.clone().into(),
            BooleanExpression::Variable(v) => bool_state(v),
        }
    }
}

impl<VI, VB, C, B> BitOr for BooleanExpression<VI, VB, C, B> {
    type Output = Self;

    fn bitor(self, rhs: Self) -> Self::Output {
        BooleanExpression::BooleanBinary {
            kind: BooleanComparisonKind::Or,
            left: self.into(),
            right: rhs.into(),
        }
    }
}

impl<VI, VB, C, B> BitAnd for BooleanExpression<VI, VB, C, B> {
    type Output = Self;

    fn bitand(self, rhs: Self) -> Self::Output {
        BooleanExpression::BooleanBinary {
            kind: BooleanComparisonKind::And,
            left: self.into(),
            right: rhs.into(),
        }
    }
}

impl<VI, VB, C, B> BitXor for BooleanExpression<VI, VB, C, B> {
    type Output = Self;

    fn bitxor(self, rhs: Self) -> Self::Output {
        BooleanExpression::BooleanBinary {
            kind: BooleanComparisonKind::Xor,
            left: self.into(),
            right: rhs.into(),
        }
    }
}

impl<VI, VB, C, B> Not for BooleanExpression<VI, VB, C, B> {
    type Output = Self;

    fn not(self) -> Self::Output {
        BooleanExpression::Not(Box::new(self))
    }
}

impl<VI, VB, C> Default for BooleanExpression<VI, VB, C> {
    fn default() -> Self {
        BooleanExpression::Constant(true)
    }
}

impl<VI, VB, C, B> BooleanExpression<VI, VB, C, B> {
    pub fn implies(
        self,
        other: BooleanExpression<VI, VB, C, B>,
    ) -> BooleanExpression<VI, VB, C, B> {
        !self | other
    }

    pub fn strictly_implies(
        self,
        other: BooleanExpression<VI, VB, C, B>,
    ) -> BooleanExpression<VI, VB, C, B> {
        self & other
    }

    pub fn eq(self, other: BooleanExpression<VI, VB, C, B>) -> BooleanExpression<VI, VB, C, B> {
        BooleanExpression::BooleanBinary {
            kind: BooleanComparisonKind::Eq,
            left: self.into(),
            right: other.into(),
        }
    }
    pub fn neq(self, other: BooleanExpression<VI, VB, C, B>) -> BooleanExpression<VI, VB, C, B> {
        !self.eq(other)
    }

    pub fn var(v: impl Into<VB>) -> BooleanExpression<VI, VB, C, B> {
        BooleanExpression::Variable(v.into())
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
            IntegerExpression::Variable(v) => variables.push(v.clone()),
            IntegerExpression::Constant(c) => constants.push(c.clone()),
            IntegerExpression::IntegerBinary { left, right, .. } => {
                left.leaves(variables, constants);
                right.leaves(variables, constants);
            }
        }
    }
}

impl<VI, VB, C, B> BooleanExpression<VI, VB, C, B>
where
    VI: Clone,
    VB: Clone,
    C: Clone,
    B: Clone,
{
    pub fn leaves(
        &self,
        int_var: &mut Vec<VI>,
        bool_var: &mut Vec<VB>,
        integers: &mut Vec<C>,
        booleans: &mut Vec<B>,
    ) {
        match self {
            BooleanExpression::IntegerBinary { left, right, .. } => {
                left.leaves(int_var, integers);
                right.leaves(int_var, integers);
            }
            BooleanExpression::BooleanBinary { left, right, .. } => {
                left.leaves(int_var, bool_var, integers, booleans);
                right.leaves(int_var, bool_var, integers, booleans);
            }
            BooleanExpression::Not(expr) => expr.leaves(int_var, bool_var, integers, booleans),
            BooleanExpression::Constant(c) => booleans.push(c.clone()),
            BooleanExpression::Variable(v) => bool_var.push(v.clone()),
        }
    }
}
