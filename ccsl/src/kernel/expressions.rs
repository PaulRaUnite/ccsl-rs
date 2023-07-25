use std::fmt;
use std::fmt::{Display, Formatter};
use std::iter::once;
use std::ops::{Add, BitAnd, BitOr, BitXor, Div, Mul, Not, Rem, Sub};

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum IntegerArithmeticsKind {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum IntegerExpression<VI, VB, C = i64, B = bool> {
    Variable(VI),
    Constant(C),
    IntegerBinary {
        kind: IntegerArithmeticsKind,
        left: Box<IntegerExpression<VI, VB, C, B>>,
        right: Box<IntegerExpression<VI, VB, C, B>>,
    },
    IfThenElse {
        cond: BooleanExpression<VI, VB, C, B>,
        then_clause: Box<IntegerExpression<VI, VB, C, B>>,
        else_clause: Box<IntegerExpression<VI, VB, C, B>>,
    },
}

impl<VI: Display, VB: Display, C: Display, B: Display> Display for IntegerExpression<VI, VB, C, B> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            IntegerExpression::Variable(v) => write!(f, "{}", v),
            IntegerExpression::Constant(c) => write!(f, "{}", c),
            IntegerExpression::IntegerBinary { kind, left, right } => {
                let sign = match kind {
                    IntegerArithmeticsKind::Add => "+",
                    IntegerArithmeticsKind::Sub => "-",
                    IntegerArithmeticsKind::Mul => "*",
                    IntegerArithmeticsKind::Div => "/",
                    IntegerArithmeticsKind::Rem => "%",
                };
                write!(f, "{} {} {}", left, sign, right)
            }
            IntegerExpression::IfThenElse {
                cond,
                then_clause,
                else_clause,
            } => {
                write!(f, "if {} then {} else {}", cond, then_clause, else_clause)
            }
        }
    }
}

impl<VI, VB, C, B> Add for IntegerExpression<VI, VB, C, B> {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        IntegerExpression::IntegerBinary {
            kind: IntegerArithmeticsKind::Add,
            left: Box::new(self),
            right: Box::new(rhs),
        }
    }
}

impl<VI, VB, C, B> Sub for IntegerExpression<VI, VB, C, B> {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        IntegerExpression::IntegerBinary {
            kind: IntegerArithmeticsKind::Sub,
            left: Box::new(self),
            right: Box::new(rhs),
        }
    }
}

impl<VI, VB, C, B> Mul for IntegerExpression<VI, VB, C, B> {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        IntegerExpression::IntegerBinary {
            kind: IntegerArithmeticsKind::Mul,
            left: Box::new(self),
            right: Box::new(rhs),
        }
    }
}

impl<VI, VB, C, B> Div for IntegerExpression<VI, VB, C, B> {
    type Output = Self;

    fn div(self, rhs: Self) -> Self::Output {
        IntegerExpression::IntegerBinary {
            kind: IntegerArithmeticsKind::Div,
            left: Box::new(self),
            right: Box::new(rhs),
        }
    }
}
impl<VI, VB, C, B> Rem for IntegerExpression<VI, VB, C, B> {
    type Output = Self;

    fn rem(self, rhs: Self) -> Self::Output {
        IntegerExpression::IntegerBinary {
            kind: IntegerArithmeticsKind::Rem,
            left: Box::new(self),
            right: Box::new(rhs),
        }
    }
}

impl<VI, VB, C, B> From<C> for IntegerExpression<VI, VB, C, B> {
    fn from(n: C) -> Self {
        IntegerExpression::Constant(n)
    }
}

impl<VI, VB, C, B> IntegerExpression<VI, VB, C, B>
where
    C: Clone
        + Ord
        + Add<Output = C>
        + Sub<Output = C>
        + Mul<Output = C>
        + Div<Output = C>
        + Rem<Output = C>
        + PartialEq,
    B: Clone
        + BitAnd<Output = B>
        + BitOr<Output = B>
        + From<bool>
        + Into<bool>
        + BitXor<Output = B>
        + PartialEq
        + Not<Output = B>,
{
    fn eval(&self, int_state: &impl Fn(&VI) -> C, bool_state: &impl Fn(&VB) -> B) -> C {
        match &self {
            IntegerExpression::Variable(v) => int_state(v),
            IntegerExpression::Constant(c) => c.clone(),
            IntegerExpression::IntegerBinary { kind, left, right } => {
                let left = left.eval(int_state, bool_state);
                let right = right.eval(int_state, bool_state);
                match &kind {
                    IntegerArithmeticsKind::Add => left + right,
                    IntegerArithmeticsKind::Sub => left - right,
                    IntegerArithmeticsKind::Mul => left * right,
                    IntegerArithmeticsKind::Div => left / right,
                    IntegerArithmeticsKind::Rem => left % right,
                }
            }
            IntegerExpression::IfThenElse {
                cond,
                then_clause,
                else_clause,
            } => {
                if cond.eval(int_state, bool_state).into() {
                    then_clause.eval(int_state, bool_state)
                } else {
                    else_clause.eval(int_state, bool_state)
                }
            }
        }
    }
}
impl<VI, VB, C, B> IntegerExpression<VI, VB, C, B> {
    pub fn var(v: impl Into<VI>) -> Self {
        IntegerExpression::Variable(v.into())
    }

    pub fn fixed(c: impl Into<C>) -> Self {
        IntegerExpression::Constant(c.into())
    }

    fn cmp(
        kind: IntegerComparisonKind,
        left: IntegerExpression<VI, VB, C, B>,
        right: IntegerExpression<VI, VB, C, B>,
    ) -> BooleanExpression<VI, VB, C, B> {
        BooleanExpression::IntegerBinary {
            kind,
            left: left.into(),
            right: right.into(),
        }
    }
}

impl<VI: Clone, VB: Clone, C: Clone, B: Clone> IntegerExpression<VI, VB, C, B> {
    pub fn eq(&self, rhs: impl Into<Self>) -> BooleanExpression<VI, VB, C, B> {
        IntegerExpression::cmp(IntegerComparisonKind::Equal, self.clone(), rhs.into())
    }
    pub fn less(&self, rhs: impl Into<Self>) -> BooleanExpression<VI, VB, C, B> {
        IntegerExpression::cmp(IntegerComparisonKind::Less, self.clone(), rhs.into())
    }
    pub fn less_eq(&self, rhs: impl Into<Self>) -> BooleanExpression<VI, VB, C, B> {
        IntegerExpression::cmp(IntegerComparisonKind::LessEq, self.clone(), rhs.into())
    }
    pub fn more(&self, rhs: impl Into<Self>) -> BooleanExpression<VI, VB, C, B> {
        IntegerExpression::cmp(IntegerComparisonKind::More, self.clone(), rhs.into())
    }
    pub fn more_eq(&self, rhs: impl Into<Self>) -> BooleanExpression<VI, VB, C, B> {
        IntegerExpression::cmp(IntegerComparisonKind::MoreEq, self.clone(), rhs.into())
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
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

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum BooleanComparisonKind {
    AND,
    OR,
    XOR,
    EQ,
    IMPLIES,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum BooleanExpression<VI, VB, C = i64, B = bool> {
    IntegerBinary {
        kind: IntegerComparisonKind,
        left: Box<IntegerExpression<VI, VB, C, B>>,
        right: Box<IntegerExpression<VI, VB, C, B>>,
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
                    IntegerComparisonKind::Equal => "=",
                    IntegerComparisonKind::Less => "<",
                    IntegerComparisonKind::LessEq => "<=",
                    IntegerComparisonKind::More => ">",
                    IntegerComparisonKind::MoreEq => ">=",
                };
                write!(f, "{} {} {}", left, sign, right)
            }
            BooleanExpression::BooleanBinary { kind, left, right } => {
                let sign = match kind {
                    BooleanComparisonKind::AND => "and",
                    BooleanComparisonKind::OR => "or",
                    BooleanComparisonKind::XOR => "xor",
                    BooleanComparisonKind::EQ => "=",
                    BooleanComparisonKind::IMPLIES => "=>",
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
    C: Clone
        + Ord
        + Add<Output = C>
        + Sub<Output = C>
        + Mul<Output = C>
        + Div<Output = C>
        + Rem<Output = C>
        + PartialEq,
    B: Clone
        + BitAnd<Output = B>
        + BitOr<Output = B>
        + From<bool>
        + Into<bool>
        + BitXor<Output = B>
        + PartialEq
        + Not<Output = B>,
{
    pub fn eval(&self, int_state: &impl Fn(&VI) -> C, bool_state: &impl Fn(&VB) -> B) -> B {
        match &self {
            BooleanExpression::IntegerBinary { kind, left, right } => {
                let left = left.eval(int_state, bool_state);
                let right = right.eval(int_state, bool_state);
                match &kind {
                    IntegerComparisonKind::Equal => left == right,
                    IntegerComparisonKind::Less => left < right,
                    IntegerComparisonKind::LessEq => left <= right,
                    IntegerComparisonKind::More => left > right,
                    IntegerComparisonKind::MoreEq => left >= right,
                }
                .into()
            }
            BooleanExpression::BooleanBinary { kind, left, right } => {
                let left = left.eval(int_state, bool_state);
                let right = right.eval(int_state, bool_state);
                match &kind {
                    BooleanComparisonKind::AND => left & right,
                    BooleanComparisonKind::OR => left | right,
                    BooleanComparisonKind::XOR => left ^ right,
                    BooleanComparisonKind::EQ => (left == right).into(),
                    BooleanComparisonKind::IMPLIES => !left | right,
                }
            }
            BooleanExpression::Not(e) => !e.eval(int_state, bool_state),
            BooleanExpression::Constant(b) => b.clone(),
            BooleanExpression::Variable(v) => bool_state(v),
        }
    }
}

impl<VI, VB, C, B> BitOr for BooleanExpression<VI, VB, C, B> {
    type Output = Self;

    fn bitor(self, rhs: Self) -> Self::Output {
        BooleanExpression::BooleanBinary {
            kind: BooleanComparisonKind::OR,
            left: self.into(),
            right: rhs.into(),
        }
    }
}

impl<VI, VB, C, B> BitAnd for BooleanExpression<VI, VB, C, B> {
    type Output = Self;

    fn bitand(self, rhs: Self) -> Self::Output {
        BooleanExpression::BooleanBinary {
            kind: BooleanComparisonKind::AND,
            left: self.into(),
            right: rhs.into(),
        }
    }
}

impl<VI, VB, C, B> BitXor for BooleanExpression<VI, VB, C, B> {
    type Output = Self;

    fn bitxor(self, rhs: Self) -> Self::Output {
        BooleanExpression::BooleanBinary {
            kind: BooleanComparisonKind::XOR,
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
        BooleanExpression::BooleanBinary {
            kind: BooleanComparisonKind::IMPLIES,
            left: Box::new(self),
            right: Box::new(other),
        }
    }

    pub fn eq(self, other: BooleanExpression<VI, VB, C, B>) -> BooleanExpression<VI, VB, C, B> {
        BooleanExpression::BooleanBinary {
            kind: BooleanComparisonKind::EQ,
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

impl<VI: Clone, VB: Clone, C: Clone, B: Clone> BooleanExpression<VI, VB, C, B> {
    pub fn if_then_else(
        &self,
        then_clause: impl Into<IntegerExpression<VI, VB, C, B>>,
        else_clause: impl Into<IntegerExpression<VI, VB, C, B>>,
    ) -> IntegerExpression<VI, VB, C, B> {
        IntegerExpression::IfThenElse {
            cond: self.clone(),
            then_clause: Box::new(then_clause.into()),
            else_clause: Box::new(else_clause.into()),
        }
    }
    pub fn if_then_elseb(
        &self,
        then_clause: impl Into<BooleanExpression<VI, VB, C, B>>,
        else_clause: impl Into<BooleanExpression<VI, VB, C, B>>,
    ) -> BooleanExpression<VI, VB, C, B> {
        (self.clone() & then_clause.into()) | (!self.clone() & else_clause.into())
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

impl<VI, VB, C, B> IntegerExpression<VI, VB, C, B>
where
    VI: Clone,
    VB: Clone,
{
    pub fn visit<SI, SB>(&self, int_var: &mut SI, bool_var: &mut SB)
    where
        SI: Extend<VI>,
        SB: Extend<VB>,
    {
        match self {
            IntegerExpression::Variable(v) => int_var.extend(once(v.clone())),
            IntegerExpression::Constant(_) => {}
            IntegerExpression::IntegerBinary { left, right, .. } => {
                left.visit(int_var, bool_var);
                right.visit(int_var, bool_var);
            }
            IntegerExpression::IfThenElse {
                cond,
                then_clause,
                else_clause,
            } => {
                cond.visit(int_var, bool_var);
                then_clause.visit(int_var, bool_var);
                else_clause.visit(int_var, bool_var);
            }
        }
    }
}

impl<VI, VB, C, B> BooleanExpression<VI, VB, C, B>
where
    VI: Clone,
    VB: Clone,
{
    pub fn visit<SI, SB>(&self, int_var: &mut SI, bool_var: &mut SB)
    where
        SI: Extend<VI>,
        SB: Extend<VB>,
    {
        match self {
            BooleanExpression::IntegerBinary { left, right, .. } => {
                left.visit(int_var, bool_var);
                right.visit(int_var, bool_var);
            }
            BooleanExpression::BooleanBinary { left, right, .. } => {
                left.visit(int_var, bool_var);
                right.visit(int_var, bool_var);
            }
            BooleanExpression::Not(expr) => expr.visit(int_var, bool_var),
            BooleanExpression::Constant(_) => {}
            BooleanExpression::Variable(v) => bool_var.extend(once(v.clone())),
        }
    }
}

impl<VI, VB, C, B> IntegerExpression<VI, VB, C, B> {
    pub fn map<NVI, NVB, NC, NB>(
        &self,
        vi: &mut impl FnMut(&VI) -> NVI,
        vb: &mut impl FnMut(&VB) -> NVB,
        ci: &mut impl FnMut(&C) -> NC,
        cb: &mut impl FnMut(&B) -> NB,
    ) -> IntegerExpression<NVI, NVB, NC, NB> {
        match self {
            IntegerExpression::Variable(v) => IntegerExpression::Variable(vi(v)),
            IntegerExpression::Constant(c) => IntegerExpression::Constant(ci(c)),
            IntegerExpression::IntegerBinary { kind, left, right } => {
                IntegerExpression::IntegerBinary {
                    kind: *kind,
                    left: Box::new(left.map(vi, vb, ci, cb)),
                    right: Box::new(right.map(vi, vb, ci, cb)),
                }
            }
            IntegerExpression::IfThenElse {
                cond,
                then_clause,
                else_clause,
            } => IntegerExpression::IfThenElse {
                cond: cond.map(vi, vb, ci, cb),
                then_clause: Box::new(then_clause.map(vi, vb, ci, cb)),
                else_clause: Box::new(else_clause.map(vi, vb, ci, cb)),
            },
        }
    }
}
impl<VI, VB, C, B> BooleanExpression<VI, VB, C, B> {
    pub fn map<NVI, NVB, NC, NB>(
        &self,
        vi: &mut impl FnMut(&VI) -> NVI,
        vb: &mut impl FnMut(&VB) -> NVB,
        ci: &mut impl FnMut(&C) -> NC,
        cb: &mut impl FnMut(&B) -> NB,
    ) -> BooleanExpression<NVI, NVB, NC, NB> {
        match self {
            BooleanExpression::IntegerBinary { kind, left, right } => {
                BooleanExpression::IntegerBinary {
                    kind: *kind,
                    left: Box::new(left.map(vi, vb, ci, cb)),
                    right: Box::new(right.map(vi, vb, ci, cb)),
                }
            }
            BooleanExpression::BooleanBinary { kind, left, right } => {
                BooleanExpression::BooleanBinary {
                    kind: *kind,
                    left: Box::new(left.map(vi, vb, ci, cb)),
                    right: Box::new(right.map(vi, vb, ci, cb)),
                }
            }
            BooleanExpression::Not(e) => BooleanExpression::Not(Box::new(e.map(vi, vb, ci, cb))),
            BooleanExpression::Constant(c) => BooleanExpression::Constant(cb(c)),
            BooleanExpression::Variable(v) => BooleanExpression::Variable(vb(v)),
        }
    }
}

impl<VI, VB> BooleanExpression<VI, VB> {
    pub fn map_var<NVI, NVB>(
        &self,
        vi: &mut impl FnMut(&VI) -> NVI,
        vb: &mut impl FnMut(&VB) -> NVB,
    ) -> BooleanExpression<NVI, NVB> {
        self.map(vi, vb, &mut |c| *c, &mut |c| *c)
    }
}

impl<VI, VB> IntegerExpression<VI, VB> {
    pub fn map_var<NVI, NVB>(
        &self,
        vi: &mut impl FnMut(&VI) -> NVI,
        vb: &mut impl FnMut(&VB) -> NVB,
    ) -> IntegerExpression<NVI, NVB> {
        self.map(vi, vb, &mut |c| *c, &mut |c| *c)
    }
}
