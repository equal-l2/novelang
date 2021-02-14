use crate::lex::Token;
use crate::types::Typed;

mod eval;
mod items;
mod parse;

pub use eval::VarsMap;

#[derive(Debug)]
pub enum EvalError {
    VariableNotFound(String),
    OverFlow,
    ZeroDivision,
    TypeError(String),
}

impl std::fmt::Display for EvalError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Failed to eval because ")?;
        Ok(match self {
            Self::VariableNotFound(s) => write!(f, "variable {} was not found", s),
            Self::OverFlow => write!(f, "of overflow"),
            Self::ZeroDivision => write!(f, "of zero division"),
            Self::TypeError(s) => write!(f, "of type error: {}", s),
        }?)
    }
}

#[derive(Debug, Clone)]
pub struct Expr {
    content: items::Rel,
}

pub enum ParseError {
    InvalidToken(Token),
    EmptyExpr,
    NoPairParen(Token),
    InvalidExpr(Option<Token>),
    NodeExhausted,
}

impl Expr {
    pub fn from_tokens(tks: &[Token]) -> Result<Self, ParseError> {
        use parse::FromTokens;
        Ok(Self {
            content: items::Rel::from_tokens(&mut tks.iter().peekable()),
        })
    }

    pub fn eval_on<T: eval::VarsMap>(&self, vmap: &T) -> Result<Typed, EvalError> {
        use eval::Eval;
        self.content.eval_on(vmap)
        /*
        match self {
            Self::Value(v) => Ok(v.clone()),
            Self::Ident(s) => vmap
                .get(s)
                .cloned()
                .ok_or_else(|| EvalError::VariableNotFound(s.clone())),
            Self::Unary(ex, op) => {
                let val = ex.eval_on(vmap)?;
                match op {
                    Ops::Ari(AriOps::Add) => Ok(val),
                    Ops::Ari(AriOps::Sub) => Ok(match val {
                        Typed::Num(n) => Typed::Num(-n),
                        Typed::Bool(b) => Typed::Bool(!b),
                    }),
                    _ => Err(EvalError::InvalidUnary(op.clone())),
                }
            }
            Self::Binary(lop, rop, op) => {
                let lval = lop.eval_on(vmap)?;
                let rval = rop.eval_on(vmap)?;

                let ltype = lval.typename();
                let rtype = rval.typename();

                let typeerror = |op: &str| {
                    Err(EvalError::TypeError(format!(
                        "Operator \"{}\" cannot be applied to {}-{}",
                        op, ltype, rtype,
                    )))
                };

                match (lval, rval) {
                    (Typed::Num(lhs), Typed::Num(rhs)) => match op {
                        Ops::Ari(op) => Ok(Typed::Num(match op {
                            AriOps::Add => lhs.checked_add(rhs).ok_or(EvalError::OverFlow)?,
                            AriOps::Sub => lhs.checked_sub(rhs).ok_or(EvalError::OverFlow)?,
                            AriOps::Mul => lhs.checked_mul(rhs).ok_or(EvalError::OverFlow)?,
                            AriOps::Div => lhs.checked_div(rhs).ok_or(EvalError::ZeroDivision)?,
                            AriOps::Mod => lhs.checked_rem(rhs).ok_or(EvalError::ZeroDivision)?,
                        })),
                        Ops::Rel(op) => Ok(Typed::Bool(match op {
                            RelOps::LessThan => lhs < rhs,
                            RelOps::GreaterThan => lhs > rhs,
                            RelOps::Equal => lhs == rhs,
                            RelOps::NotEqual => lhs != rhs,
                            RelOps::LessEqual => lhs <= rhs,
                            RelOps::GreaterEqual => lhs >= rhs,
                        })),
                    },
                    (Typed::Bool(lhs), Typed::Bool(rhs)) => match op {
                        Ops::Rel(op) => Ok(Typed::Bool(match op {
                            RelOps::Equal => lhs == rhs,
                            RelOps::NotEqual => lhs != rhs,
                            _ => return typeerror(op.as_str()),
                        })),
                        _ => return typeerror(op.as_str()),
                    },
                    _ => return typeerror(op.as_str()),
                }
            }
        }*/
    }
}
