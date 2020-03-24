use crate::parser::Rule;

pub type VarIntType = u64;

#[derive(Debug)]
pub enum ExprRuntimeError {
    IdentNotFound(String),
    OverFlow,
    ZeroDivision,
}

impl std::fmt::Display for ExprRuntimeError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        match self {
            Self::IdentNotFound(s) => write!(f, "Ident \"{}\" was not found", s),
            Self::OverFlow => write!(f, "Expr overflowed"),
            Self::ZeroDivision => write!(f, "Divided by zero"),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Expr {
    IdentOrNum(IdentOrNum),
    TrueExpr(TrueExpr),
}

#[derive(Debug, Clone)]
pub enum ExprOp {
    Add, // +
    Sub, // -
    Mul, // *
    Div, // /
    Mod, // %
}

#[derive(Debug, Clone)]
pub enum IdentOrNum {
    Ident(String),
    Num(VarIntType),
}

#[derive(Debug, Clone)]
pub struct TrueExpr {
    lhs: Box<Expr>,
    op: ExprOp,
    rhs: Box<Expr>,
}

#[derive(Debug, Clone)]
pub enum CompOp {
    LessThan,     // <
    GreaterThan,  // >
    Equal,        // ==
    NotEqual,     // !=
    LessEqual,    // <=
    GreaterEqual, // >=
}

#[derive(Debug)]
pub struct CompOpParseError(String);

impl std::fmt::Display for CompOpParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        write!(f, "cannot parse as CompOp: {}", self.0)
    }
}

impl std::str::FromStr for CompOp {
    type Err = CompOpParseError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "<" => Ok(Self::LessThan),
            ">" => Ok(Self::GreaterThan),
            "==" => Ok(Self::Equal),
            "!=" => Ok(Self::NotEqual),
            "<=" => Ok(Self::LessEqual),
            ">=" => Ok(Self::GreaterEqual),
            _ => Err(CompOpParseError(s.to_owned())),
        }
    }
}

#[derive(Debug, Clone)]
pub struct CompExpr {
    lhs: Expr,
    op: CompOp,
    rhs: Expr,
}

pub trait FromStmt {
    fn parse_stmt(stmt: pest::iterators::Pair<Rule>) -> Self;
}

fn consume(
    pair: pest::iterators::Pair<Rule>,
    climber: &pest::prec_climber::PrecClimber<Rule>,
) -> Expr {
    let primary = |pair| consume(pair, climber);
    let infix = |lhs: Expr, op: pest::iterators::Pair<Rule>, rhs: Expr| -> Expr {
        Expr::TrueExpr(TrueExpr {
            lhs: Box::new(lhs),
            op: match op.as_rule() {
                Rule::Plus => ExprOp::Add,
                Rule::Minus => ExprOp::Sub,
                Rule::Times => ExprOp::Mul,
                Rule::Div => ExprOp::Div,
                Rule::Mod => ExprOp::Mod,
                _ => unreachable!(),
            },
            rhs: Box::new(rhs),
        })
    };

    match pair.as_rule() {
        Rule::Expr => climber.climb(pair.into_inner(), primary, infix),
        Rule::ExprUnit => pair.into_inner().next().map(primary).unwrap(),
        Rule::IdentOrNum => Expr::IdentOrNum(IdentOrNum::parse_stmt(pair)),
        _ => unreachable!(),
    }
}

impl FromStmt for Expr {
    fn parse_stmt(stmt: pest::iterators::Pair<Rule>) -> Self {
        use pest::prec_climber::{Assoc, Operator, PrecClimber};
        let climber = PrecClimber::new(vec![
            Operator::new(Rule::Plus, Assoc::Left) | Operator::new(Rule::Minus, Assoc::Left),
            Operator::new(Rule::Times, Assoc::Left)
                | Operator::new(Rule::Div, Assoc::Left)
                | Operator::new(Rule::Mod, Assoc::Left),
        ]);
        consume(stmt, &climber)
    }
}

impl FromStmt for ExprOp {
    fn parse_stmt(stmt: pest::iterators::Pair<Rule>) -> Self {
        match stmt.as_rule() {
            Rule::Plus => Self::Add,
            Rule::Minus => Self::Sub,
            Rule::Times => Self::Mul,
            Rule::Div => Self::Div,
            Rule::Mod => Self::Mod,
            _ => unreachable!(stmt),
        }
    }
}

impl FromStmt for IdentOrNum {
    fn parse_stmt(stmt: pest::iterators::Pair<Rule>) -> Self {
        let it = stmt.into_inner().next().unwrap();
        match it.as_rule() {
            Rule::Ident => Self::Ident(it.as_str().to_owned()),
            Rule::Num => Self::Num(it.as_str().parse().unwrap()),
            other => {
                panic!("Semantic error: unexpected rule : {:?}", other);
            }
        }
    }
}

impl FromStmt for CompOp {
    fn parse_stmt(stmt: pest::iterators::Pair<Rule>) -> Self {
        use std::str::FromStr;
        Self::from_str(stmt.as_str()).unwrap()
    }
}

impl FromStmt for CompExpr {
    fn parse_stmt(stmt: pest::iterators::Pair<Rule>) -> Self {
        let mut it = stmt.into_inner();
        Self {
            lhs: Expr::parse_stmt(it.next().unwrap()),
            op: CompOp::parse_stmt(it.next().unwrap()),
            rhs: Expr::parse_stmt(it.next().unwrap()),
        }
    }
}

pub trait Eval {
    type T;
    fn eval(&self, call_stack: &crate::runner::CallStack) -> Result<Self::T, ExprRuntimeError>;
}

impl Eval for CompExpr {
    type T = bool;
    fn eval(&self, call_stack: &crate::runner::CallStack) -> Result<Self::T, ExprRuntimeError> {
        let lhs = self.lhs.eval(call_stack)?;
        let rhs = self.rhs.eval(call_stack)?;
        Ok(match self.op {
            CompOp::LessThan => lhs < rhs,
            CompOp::GreaterThan => lhs > rhs,
            CompOp::Equal => lhs == rhs,
            CompOp::NotEqual => lhs != rhs,
            CompOp::LessEqual => lhs <= rhs,
            CompOp::GreaterEqual => lhs >= rhs,
        })
    }
}

impl Eval for Expr {
    type T = VarIntType;
    fn eval(&self, call_stack: &crate::runner::CallStack) -> Result<Self::T, ExprRuntimeError> {
        match self {
            Self::IdentOrNum(ion) => ion.eval(call_stack),
            Self::TrueExpr(x) => x.eval(call_stack),
        }
    }
}

impl Eval for IdentOrNum {
    type T = VarIntType;
    fn eval(&self, call_stack: &crate::runner::CallStack) -> Result<Self::T, ExprRuntimeError> {
        Ok(match self {
            Self::Ident(name) => {
                call_stack
                    .get_var(name)
                    .ok_or_else(|| ExprRuntimeError::IdentNotFound(name.clone()))?
                    .value
            }
            Self::Num(num) => *num,
        })
    }
}

impl Eval for TrueExpr {
    type T = VarIntType;
    fn eval<'a>(&self, call_stack: &crate::runner::CallStack) -> Result<Self::T, ExprRuntimeError> {
        let lhs = self.lhs.eval(call_stack)?;
        let rhs = self.rhs.eval(call_stack)?;
        match self.op {
            ExprOp::Add => lhs.checked_add(rhs).ok_or(ExprRuntimeError::OverFlow),
            ExprOp::Sub => lhs.checked_sub(rhs).ok_or(ExprRuntimeError::OverFlow),
            ExprOp::Mul => lhs.checked_mul(rhs).ok_or(ExprRuntimeError::OverFlow),
            ExprOp::Div => lhs.checked_div(rhs).ok_or(ExprRuntimeError::ZeroDivision),
            ExprOp::Mod => lhs.checked_rem(rhs).ok_or(ExprRuntimeError::OverFlow),
            _ => unimplemented!(),
        }
    }
}
