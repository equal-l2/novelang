use crate::lex;
use crate::run::VarIntType;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RPOps {
    Ari(lex::AriOps),
    Rel(lex::RelOps),
}

enum OpOrd {
    Mul(lex::AriOps),
    Add(lex::AriOps),
}

impl From<lex::AriOps> for OpOrd {
    fn from(op: lex::AriOps) -> Self {
        use lex::AriOps;
        match op {
            AriOps::Add => Self::Add(op),
            AriOps::Sub => Self::Add(op),
            AriOps::Mul => Self::Mul(op),
            AriOps::Div => Self::Mul(op),
            AriOps::Mod => Self::Mul(op),
        }
    }
}

// Order by precedence
// The lesser precedes.
use std::cmp::Ordering;
impl PartialOrd for RPOps {
    fn partial_cmp(&self, other: &RPOps) -> Option<Ordering> {
        if self == other {
            Some(Ordering::Equal)
        } else {
            Some(match (self, other) {
                (Self::Ari(this), Self::Ari(that)) => {
                    match (OpOrd::from(*this), OpOrd::from(*that)) {
                        (OpOrd::Add(_), OpOrd::Add(_)) => Ordering::Equal,
                        (OpOrd::Add(_), OpOrd::Mul(_)) => Ordering::Greater,
                        (OpOrd::Mul(_), OpOrd::Add(_)) => Ordering::Less,
                        (OpOrd::Mul(_), OpOrd::Mul(_)) => Ordering::Equal,
                    }
                }
                (Self::Ari(_), Self::Rel(_)) => Ordering::Less,
                (Self::Rel(_), Self::Ari(_)) => Ordering::Greater,
                (Self::Rel(_), Self::Rel(_)) => Ordering::Equal,
            })
        }
    }
}

#[derive(Debug, Clone)]
pub enum RPNode {
    Ident(String),
    Num(VarIntType),
    Bool(bool),
    Ops(RPOps),
}

#[derive(Debug, Clone)]
pub struct Expr {
    pub content: Vec<RPNode>,
}

pub enum Error {
    InvalidToken(lex::Token),
    EmptyExpr,
}

impl Expr {
    pub fn from_tokens(tks: &[lex::Token]) -> Result<Self, Error> {
        if tks.is_empty() {
            return Err(Error::EmptyExpr);
        }
        //for t in tks {
        //    eprint!("{} ", t);
        //}
        //eprintln!();
        let nodes: Vec<_> = tks
            .into_iter()
            .map(|t| match &t.item {
                lex::Item::Ident(name) => Ok(RPNode::Ident(name.clone())),
                lex::Item::Num(num) => Ok(RPNode::Num(*num)),
                lex::Item::Ari(op) => Ok(RPNode::Ops(RPOps::Ari(*op))),
                lex::Item::Rel(op) => Ok(RPNode::Ops(RPOps::Rel(*op))),
                _ => {
                    eprintln!("Stranger: {}", t);
                    Err(Error::InvalidToken(t.clone()))
                }
            })
            .collect::<Result<_, _>>()?;

        // http://www.gg.e-mansion.com/~kkatoh/program/novel2/novel208.html
        let mut stack = vec![];
        let mut content = vec![];
        for n in nodes {
            match &n {
                RPNode::Ident(_) | RPNode::Num(_) => content.push(n),
                RPNode::Ops(incoming) => {
                    loop {
                        if stack.is_empty() {
                            break;
                        }
                        if let RPNode::Ops(op) = stack.last().unwrap() {
                            if incoming > op {
                                content.push(stack.pop().unwrap());
                            } else {
                                break;
                            }
                        }
                    }
                    stack.push(n);
                }
                _ => unreachable!(),
            }
        }
        while let Some(op) = stack.pop() {
            content.push(op);
        }
        Ok(Expr { content })
    }
}
