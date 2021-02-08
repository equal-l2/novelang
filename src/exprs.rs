use crate::lex::{self, AriOps, Item, Items, Ops, RelOps, Token};
use crate::types::Typed;

mod rp_node;
pub use rp_node::RPNode;

enum OpOrd<'a> {
    Mul(&'a AriOps),
    Add(&'a AriOps),
}

impl<'a> From<&'a AriOps> for OpOrd<'a> {
    fn from(op: &'a AriOps) -> Self {
        match op {
            AriOps::Add | AriOps::Sub => Self::Add(op),
            AriOps::Mul | AriOps::Div | AriOps::Mod => Self::Mul(op),
        }
    }
}

// Order by precedence
// The greater precedes.
use std::cmp::Ordering;
impl PartialOrd for Ops {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        if self == other {
            Some(Ordering::Equal)
        } else {
            Some(match (self, other) {
                (Self::Ari(this), Self::Ari(that)) => {
                    match (OpOrd::from(this), OpOrd::from(that)) {
                        (OpOrd::Add(_), OpOrd::Add(_)) | (OpOrd::Mul(_), OpOrd::Mul(_)) => {
                            Ordering::Equal
                        }
                        (OpOrd::Add(_), OpOrd::Mul(_)) => Ordering::Less, // "+" < "*"
                        (OpOrd::Mul(_), OpOrd::Add(_)) => Ordering::Greater, // "*" > "+"
                    }
                }
                (Self::Ari(_), Self::Rel(_)) => Ordering::Greater, // "+" > "<="
                (Self::Rel(_), Self::Ari(_)) => Ordering::Less,
                (Self::Rel(_), Self::Rel(_)) => Ordering::Equal,
            })
        }
    }
}

#[derive(Debug, Clone)]
pub struct Expr {
    pub content: Vec<RPNode>,
}

pub enum Error {
    InvalidToken(Token),
    EmptyExpr,
    NoPairParen(Token),
}

pub trait VarsMap {
    fn get(&self, name: &str) -> Option<&Typed>;
}

#[derive(Debug)]
pub enum EvalError {
    VariableNotFound(String),
    InvalidExpr(crate::exprs::Expr),
    OverFlow,
    ZeroDivision,
    TypeError(String),
}

impl std::fmt::Display for EvalError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Failed to eval because ")?;
        match self {
            Self::VariableNotFound(s) => write!(f, "variable {} was not found", s),
            Self::InvalidExpr(e) => write!(f, "expr {:?} is invalid", e),
            Self::OverFlow => write!(f, "of overflow"),
            Self::ZeroDivision => write!(f, "of zero division"),
            Self::TypeError(s) => write!(f, "of type error : {}", s),
        }?;
        Ok(())
    }
}

impl Expr {
    pub fn from_tokens(tks: &[Token]) -> Result<Self, Error> {
        if tks.is_empty() {
            return Err(Error::EmptyExpr);
        }

        //eprintln!(
        //    "Expr from: {:?}",
        //    tks.iter().map(|tk| &tk.item).collect::<Vec<_>>()
        //);

        // shunting-yard algorithm
        // https://en.wikipedia.org/w/index.php?title=Shunting-yard_algorithm&oldid=1002380861
        let mut stack: Vec<&Token> = vec![];
        let mut buf: Vec<&Token> = vec![];
        for token in tks {
            match &token.item {
                Items::Ident(_)
                | Items::Num(_, _)
                | Items::Key(lex::Keywords::True)
                | Items::Key(lex::Keywords::False) => buf.push(token),
                Items::Ops(incoming) => {
                    loop {
                        match stack.last() {
                            Some(Token { item, .. }) => match item {
                                Items::Ops(op) if incoming <= op => {
                                    buf.push(stack.pop().unwrap());
                                }
                                _ => break,
                            },
                            None => break,
                        }
                    }
                    stack.push(token);
                }
                Items::LParen => stack.push(token),
                Items::RParen => loop {
                    if let Some(i) = stack.pop() {
                        if i.item == Items::LParen {
                            break;
                        }
                        buf.push(i);
                    } else {
                        return Err(Error::NoPairParen(token.clone()));
                    }
                },
                _ => {
                    return Err(Error::InvalidToken(token.clone()));
                }
            }
        }

        let content = buf
            .into_iter()
            .chain(stack.into_iter().rev())
            .map(|tk| {
                Ok(match &tk.item {
                    Items::Ident(s) => RPNode::Ident(s.clone()),
                    Items::Num(n, _) => RPNode::Num(*n),
                    Items::Ops(op) => RPNode::Ops(op.clone()),
                    Items::LParen => {
                        return Err(Error::NoPairParen(tk.clone()));
                    }
                    Items::Key(lex::Keywords::True) => RPNode::Bool(true),
                    Items::Key(lex::Keywords::False) => RPNode::Bool(false),
                    _ => unreachable!(tk),
                })
            })
            .collect::<Result<_, _>>()?;

        //eprintln!("Expr to: {:?}", content);

        Ok(Self { content })
    }

    pub fn eval_on<T: VarsMap>(&self, vmap: &T) -> Result<Typed, EvalError> {
        let resolve_ident = |name: &str| {
            if let Some(v) = vmap.get(name) {
                Ok(v.clone())
            } else {
                Err(EvalError::VariableNotFound(name.to_owned()))
            }
        };
        let list = &self.content;
        match list.len() {
            0 => return Err(EvalError::InvalidExpr(self.clone())),
            1 => {
                return match list.last().unwrap() {
                    RPNode::Num(num) => Ok(Typed::Num(*num)),
                    RPNode::Bool(b) => Ok(Typed::Bool(*b)),
                    RPNode::Ident(name) => resolve_ident(name),
                    _ => Err(EvalError::InvalidExpr(self.clone())),
                }
            }
            _ => {}
        };

        let mut stack = vec![];
        let wrap = |v| {
            if let RPNode::Ident(name) = v {
                resolve_ident(&name).map(|v| match v {
                    Typed::Num(n) => RPNode::Num(n),
                    Typed::Bool(b) => RPNode::Bool(b),
                })
            } else {
                Ok(v)
            }
        };
        for n in list {
            if let RPNode::Ops(op) = n {
                let rhs = stack.pop().map(wrap).transpose()?;
                let lhs = stack.pop().map(wrap).transpose()?;
                match (lhs, rhs) {
                    (Some(lhs), Some(rhs)) => {
                        let lhs_type = lhs.typename();
                        let rhs_type = rhs.typename();
                        let typeerror = |op: &str| {
                            Err(EvalError::TypeError(format!(
                                "Operator \"{}\" cannot be applied to {}-{}",
                                op, lhs_type, rhs_type,
                            )))
                        };
                        match (&lhs, &rhs) {
                            (RPNode::Num(lhs), RPNode::Num(rhs)) => stack.push(match op {
                                Ops::Ari(op) => RPNode::Num(match op {
                                    AriOps::Add => {
                                        lhs.checked_add(*rhs).ok_or(EvalError::OverFlow)?
                                    }
                                    AriOps::Sub => {
                                        lhs.checked_sub(*rhs).ok_or(EvalError::OverFlow)?
                                    }
                                    AriOps::Mul => {
                                        lhs.checked_mul(*rhs).ok_or(EvalError::OverFlow)?
                                    }
                                    AriOps::Div => {
                                        lhs.checked_div(*rhs).ok_or(EvalError::ZeroDivision)?
                                    }
                                    AriOps::Mod => {
                                        lhs.checked_rem(*rhs).ok_or(EvalError::OverFlow)?
                                    }
                                }),
                                Ops::Rel(op) => RPNode::Bool(match op {
                                    RelOps::LessThan => lhs < rhs,
                                    RelOps::GreaterThan => lhs > rhs,
                                    RelOps::Equal => lhs == rhs,
                                    RelOps::NotEqual => lhs != rhs,
                                    RelOps::LessEqual => lhs <= rhs,
                                    RelOps::GreaterEqual => lhs >= rhs,
                                }),
                            }),
                            (RPNode::Bool(lhs), RPNode::Bool(rhs)) => stack.push(match op {
                                Ops::Rel(op) => RPNode::Bool(match op {
                                    RelOps::Equal => lhs == rhs,
                                    RelOps::NotEqual => lhs != rhs,
                                    _ => return typeerror(lex::Item::as_str(op)),
                                }),
                                _ => return typeerror(op.as_str()),
                            }),
                            _ => return typeerror(op.as_str()),
                        }
                    }
                    _ => {
                        return Err(EvalError::InvalidExpr(self.clone()));
                    }
                }
            } else {
                stack.push(n.clone());
            }
        }

        // check if expression is sound
        if stack.len() == 1 {
            match stack.last().unwrap() {
                RPNode::Num(num) => Ok(Typed::Num(*num)),
                RPNode::Bool(b) => Ok(Typed::Bool(*b)),
                _ => Err(EvalError::InvalidExpr(self.clone())),
            }
        } else {
            Err(EvalError::InvalidExpr(self.clone()))
        }
    }
}
