use ccsl::symbolic::ts::Constant;
use itertools::Itertools;
use std::fmt;
use std::fmt::{Display, Formatter};
use std::ops::{Add, BitAnd, BitOr, BitXor, Div, Mul, Not, Sub};

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum IntegerArithmeticsKind {
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Debug, Clone, From)]
pub enum Expression<V> {
    Integer(IntegerExpression<V>),
    Bool(BooleanExpression<V>),
}

impl<V: Display> Display for Expression<V> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Expression::Integer(i) => write!(f, "{}", i),
            Expression::Bool(b) => write!(f, "{}", b),
        }
    }
}

#[derive(Debug, Clone)]
pub enum IntegerExpression<V> {
    Variable(V),
    Constant(i64),
    IntegerBinary {
        kind: IntegerArithmeticsKind,
        left: Box<IntegerExpression<V>>,
        right: Box<IntegerExpression<V>>,
    },
    Exists {
        expr: Box<IntegerExpression<V>>,
        list: Vec<IntegerExpression<V>>,
    },
    IfThenElse {
        cond: BooleanExpression<V>,
        then_clause: Box<IntegerExpression<V>>,
        else_clause: Box<IntegerExpression<V>>,
    },
}

impl<V> From<i64> for IntegerExpression<V> {
    fn from(n: i64) -> Self {
        IntegerExpression::Constant(n)
    }
}

impl<V> Add for IntegerExpression<V> {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        IntegerExpression::IntegerBinary {
            kind: IntegerArithmeticsKind::Add,
            left: Box::new(self),
            right: Box::new(rhs),
        }
    }
}

impl<V> Sub for IntegerExpression<V> {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        IntegerExpression::IntegerBinary {
            kind: IntegerArithmeticsKind::Sub,
            left: Box::new(self),
            right: Box::new(rhs),
        }
    }
}

impl<V> Mul for IntegerExpression<V> {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        IntegerExpression::IntegerBinary {
            kind: IntegerArithmeticsKind::Mul,
            left: Box::new(self),
            right: Box::new(rhs),
        }
    }
}

impl<V> Div for IntegerExpression<V> {
    type Output = Self;

    fn div(self, rhs: Self) -> Self::Output {
        IntegerExpression::IntegerBinary {
            kind: IntegerArithmeticsKind::Div,
            left: Box::new(self),
            right: Box::new(rhs),
        }
    }
}

impl<V> IntegerExpression<V> {
    pub fn var(v: impl Into<V>) -> Self {
        IntegerExpression::Variable(v.into())
    }

    pub fn fixed(c: impl Into<i64>) -> Self {
        IntegerExpression::Constant(c.into())
    }

    fn cmp(
        kind: IntegerComparisonKind,
        left: IntegerExpression<V>,
        right: IntegerExpression<V>,
    ) -> BooleanExpression<V> {
        BooleanExpression::IntegerBinary {
            kind,
            left: left.into(),
            right: right.into(),
        }
    }
}

impl<V: Display> Display for IntegerExpression<V> {
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
                };
                write!(
                    f,
                    "{} {} {}",
                    left.maybe_parenthesize(Some(*kind)),
                    sign,
                    right.maybe_parenthesize(Some(*kind))
                )
            }
            IntegerExpression::Exists { expr, list } => {
                write!(f, "{expr} in {}", list.iter().join(","))
            }
            IntegerExpression::IfThenElse {
                cond,
                then_clause,
                else_clause,
            } => write!(
                f,
                "if {} then {} else {}",
                cond,
                then_clause.maybe_parenthesize(None),
                else_clause.maybe_parenthesize(None)
            ),
        }
    }
}

impl<V: Clone> IntegerExpression<V> {
    pub fn eq(&self, rhs: impl Into<Self>) -> BooleanExpression<V> {
        IntegerExpression::cmp(IntegerComparisonKind::Equal, self.clone(), rhs.into())
    }
    pub fn less(&self, rhs: impl Into<Self>) -> BooleanExpression<V> {
        IntegerExpression::cmp(IntegerComparisonKind::Less, self.clone(), rhs.into())
    }
    pub fn less_eq(&self, rhs: impl Into<Self>) -> BooleanExpression<V> {
        IntegerExpression::cmp(IntegerComparisonKind::LessEq, self.clone(), rhs.into())
    }
    pub fn more(&self, rhs: impl Into<Self>) -> BooleanExpression<V> {
        IntegerExpression::cmp(IntegerComparisonKind::More, self.clone(), rhs.into())
    }
    pub fn more_eq(&self, rhs: impl Into<Self>) -> BooleanExpression<V> {
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

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum BooleanComparisonKind {
    And,
    Or,
    Xor,
    Eq,
    Imply,
}

#[derive(Debug, Clone)]
pub enum BooleanExpression<V> {
    IntegerBinary {
        kind: IntegerComparisonKind,
        left: Box<IntegerExpression<V>>,
        right: Box<IntegerExpression<V>>,
    },
    BooleanBinary {
        kind: BooleanComparisonKind,
        left: Box<BooleanExpression<V>>,
        right: Box<BooleanExpression<V>>,
    },
    Not(Box<BooleanExpression<V>>),
    Constant(bool),
    Variable(V),
    Exclusive(Vec<BooleanExpression<V>>),

    IfThenElse {
        cond: Box<BooleanExpression<V>>,
        then_clause: Box<BooleanExpression<V>>,
        else_clause: Box<BooleanExpression<V>>,
    },
}

impl<V> From<bool> for BooleanExpression<V> {
    fn from(value: bool) -> Self {
        BooleanExpression::Constant(value)
    }
}

impl<V: Display> Display for BooleanExpression<V> {
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
                write!(
                    f,
                    "{} {} {}",
                    left.maybe_parenthesize(None),
                    sign,
                    right.maybe_parenthesize(None)
                )
            }
            BooleanExpression::BooleanBinary { kind, left, right } => {
                let sign = match kind {
                    BooleanComparisonKind::And => "and",
                    BooleanComparisonKind::Or => "or",
                    BooleanComparisonKind::Xor => "xor",
                    BooleanComparisonKind::Eq => "=",
                    BooleanComparisonKind::Imply => "=>",
                };
                write!(
                    f,
                    "{} {} {}",
                    left.maybe_parenthesize(Some(*kind)),
                    sign,
                    right.maybe_parenthesize(Some(*kind))
                )
            }
            BooleanExpression::Not(exp) => write!(f, "not {}", exp.maybe_parenthesize(None)),
            BooleanExpression::Constant(b) => write!(f, "{}", b),
            BooleanExpression::Variable(v) => write!(f, "{}", v),
            BooleanExpression::Exclusive(list) => write!(
                f,
                "#({})",
                list.iter().map(|e| e.maybe_parenthesize(None)).join(",")
            ),
            BooleanExpression::IfThenElse {
                cond,
                then_clause,
                else_clause,
            } => write!(
                f,
                "if {} then {} else {}",
                cond,
                then_clause.maybe_parenthesize(None),
                else_clause.maybe_parenthesize(None)
            ),
        }
    }
}

impl<V> BooleanExpression<V> {
    fn complex_expression(&self, outer_operation: Option<BooleanComparisonKind>) -> bool {
        match self {
            BooleanExpression::IntegerBinary { .. } => true,
            BooleanExpression::BooleanBinary { kind, .. } => Some(*kind) != outer_operation,
            BooleanExpression::Not(expr) => false,
            BooleanExpression::Constant(_) => false,
            BooleanExpression::Variable(_) => false,
            BooleanExpression::Exclusive(_) => false,
            BooleanExpression::IfThenElse { .. } => true, // TODO: is it??
        }
    }
    fn maybe_parenthesize(&self, outer_operation: Option<BooleanComparisonKind>) -> String
    where
        V: Display,
    {
        if self.complex_expression(outer_operation) {
            format!("({})", self)
        } else {
            self.to_string()
        }
    }
}
impl<V> IntegerExpression<V> {
    fn complex_expression(&self, outer_operation: Option<IntegerArithmeticsKind>) -> bool {
        match self {
            IntegerExpression::Variable(_) => false,
            IntegerExpression::Constant(_) => false,
            IntegerExpression::IntegerBinary { kind, .. } => Some(*kind) != outer_operation,
            IntegerExpression::Exists { .. } => true,
            IntegerExpression::IfThenElse { .. } => true,
        }
    }
    fn maybe_parenthesize(&self, outer_operation: Option<IntegerArithmeticsKind>) -> String
    where
        V: Display,
    {
        if self.complex_expression(outer_operation) {
            format!("({})", self)
        } else {
            self.to_string()
        }
    }
}

impl<V> BitOr for BooleanExpression<V> {
    type Output = Self;

    fn bitor(self, rhs: Self) -> Self::Output {
        BooleanExpression::BooleanBinary {
            kind: BooleanComparisonKind::Or,
            left: self.into(),
            right: rhs.into(),
        }
    }
}

impl<V> BitAnd for BooleanExpression<V> {
    type Output = Self;

    fn bitand(self, rhs: Self) -> Self::Output {
        BooleanExpression::BooleanBinary {
            kind: BooleanComparisonKind::And,
            left: self.into(),
            right: rhs.into(),
        }
    }
}

impl<V> BitXor for BooleanExpression<V> {
    type Output = Self;

    fn bitxor(self, rhs: Self) -> Self::Output {
        BooleanExpression::BooleanBinary {
            kind: BooleanComparisonKind::Xor,
            left: self.into(),
            right: rhs.into(),
        }
    }
}

impl<V> Not for BooleanExpression<V> {
    type Output = Self;

    fn not(self) -> Self::Output {
        BooleanExpression::Not(Box::new(self))
    }
}

impl<V> Default for BooleanExpression<V> {
    fn default() -> Self {
        BooleanExpression::Constant(true)
    }
}

impl<V> BooleanExpression<V> {
    pub fn implies(self, other: impl Into<BooleanExpression<V>>) -> BooleanExpression<V> {
        BooleanExpression::BooleanBinary {
            kind: BooleanComparisonKind::Imply,
            left: self.into(),
            right: Box::new(other.into()),
        }
    }

    pub fn eq(self, other: impl Into<BooleanExpression<V>>) -> BooleanExpression<V> {
        BooleanExpression::BooleanBinary {
            kind: BooleanComparisonKind::Eq,
            left: self.into(),
            right: Box::new(other.into()),
        }
    }
    pub fn neq(self, other: impl Into<BooleanExpression<V>>) -> BooleanExpression<V> {
        !self.eq(other.into())
    }

    pub fn var(v: impl Into<V>) -> BooleanExpression<V> {
        BooleanExpression::Variable(v.into())
    }
}

impl<V: Clone> BooleanExpression<V> {
    pub fn if_then_else(
        &self,
        then_clause: impl Into<IntegerExpression<V>>,
        else_clause: impl Into<IntegerExpression<V>>,
    ) -> IntegerExpression<V> {
        IntegerExpression::IfThenElse {
            cond: self.clone(),
            then_clause: Box::new(then_clause.into()),
            else_clause: Box::new(else_clause.into()),
        }
    }
    pub fn if_then_elseb(
        &self,
        then_clause: impl Into<BooleanExpression<V>>,
        else_clause: impl Into<BooleanExpression<V>>,
    ) -> BooleanExpression<V> {
        BooleanExpression::IfThenElse {
            cond: Box::new(self.clone()),
            then_clause: Box::new(then_clause.into()),
            else_clause: Box::new(else_clause.into()),
        }
    }
}

type CCSLBoolExp<V> = ccsl::kernel::expressions::BooleanExpression<V, V>;
type CCSLIntExp<V> = ccsl::kernel::expressions::IntegerExpression<V, V>;

impl<V> From<Constant> for Expression<V> {
    fn from(value: Constant) -> Self {
        match value {
            Constant::Bool(v) => Expression::Bool(BooleanExpression::Constant(v)),
            Constant::Int(v) => Expression::Integer(IntegerExpression::Constant(v)),
        }
    }
}
impl<V> From<CCSLBoolExp<V>> for BooleanExpression<V> {
    fn from(value: CCSLBoolExp<V>) -> Self {
        match value {
            CCSLBoolExp::IntegerBinary { kind, left, right } => BooleanExpression::IntegerBinary {
                kind: match kind {
                    ccsl::kernel::expressions::IntegerComparisonKind::Equal => {
                        IntegerComparisonKind::Equal
                    }
                    ccsl::kernel::expressions::IntegerComparisonKind::Less => {
                        IntegerComparisonKind::Less
                    }
                    ccsl::kernel::expressions::IntegerComparisonKind::LessEq => {
                        IntegerComparisonKind::LessEq
                    }
                    ccsl::kernel::expressions::IntegerComparisonKind::More => {
                        IntegerComparisonKind::More
                    }
                    ccsl::kernel::expressions::IntegerComparisonKind::MoreEq => {
                        IntegerComparisonKind::MoreEq
                    }
                },
                left: Box::new((*left).into()),
                right: Box::new((*right).into()),
            },
            CCSLBoolExp::BooleanBinary { kind, left, right } => BooleanExpression::BooleanBinary {
                kind: match kind {
                    ccsl::kernel::expressions::BooleanComparisonKind::AND => {
                        BooleanComparisonKind::And
                    }
                    ccsl::kernel::expressions::BooleanComparisonKind::OR => {
                        BooleanComparisonKind::Or
                    }
                    ccsl::kernel::expressions::BooleanComparisonKind::XOR => {
                        BooleanComparisonKind::Xor
                    }
                    ccsl::kernel::expressions::BooleanComparisonKind::EQ => {
                        BooleanComparisonKind::Eq
                    }
                    ccsl::kernel::expressions::BooleanComparisonKind::IMPLIES => {
                        BooleanComparisonKind::Imply
                    }
                },
                left: Box::new((*left).into()),
                right: Box::new((*right).into()),
            },
            CCSLBoolExp::Not(expr) => BooleanExpression::Not(Box::new((*expr).into())),
            CCSLBoolExp::Constant(c) => BooleanExpression::Constant(c),
            CCSLBoolExp::Variable(v) => BooleanExpression::Variable(v),
        }
    }
}
impl<V> From<CCSLIntExp<V>> for IntegerExpression<V> {
    fn from(value: CCSLIntExp<V>) -> Self {
        match value {
            CCSLIntExp::Variable(v) => IntegerExpression::Variable(v),
            CCSLIntExp::Constant(c) => IntegerExpression::Constant(c),
            CCSLIntExp::IntegerBinary { kind, left, right } => IntegerExpression::IntegerBinary {
                kind: match kind {
                    ccsl::kernel::expressions::IntegerArithmeticsKind::Add => {
                        IntegerArithmeticsKind::Add
                    }
                    ccsl::kernel::expressions::IntegerArithmeticsKind::Sub => {
                        IntegerArithmeticsKind::Sub
                    }
                    ccsl::kernel::expressions::IntegerArithmeticsKind::Mul => {
                        IntegerArithmeticsKind::Mul
                    }
                    _ => panic!(),
                },
                left: Box::new((*left).into()),
                right: Box::new((*right).into()),
            },
            CCSLIntExp::IfThenElse {
                cond,
                then_clause,
                else_clause,
            } => IntegerExpression::IfThenElse {
                cond: cond.into(),
                then_clause: Box::new((*then_clause).into()),
                else_clause: Box::new((*else_clause).into()),
            },
        }
    }
}
