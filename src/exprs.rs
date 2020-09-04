use crate::lex::{AriOps, Item, Ops, Token};
use crate::run::VarIntType;

enum OpOrd {
    Mul(AriOps),
    Add(AriOps),
}

impl From<AriOps> for OpOrd {
    fn from(op: AriOps) -> Self {
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
impl PartialOrd for Ops {
    fn partial_cmp(&self, other: &Ops) -> Option<Ordering> {
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
    Ops(Ops),
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

impl Expr {
    pub fn from_tokens(tks: &[Token]) -> Result<Self, Error> {
        if tks.is_empty() {
            return Err(Error::EmptyExpr);
        }

        //println!("{:?}", tks.iter().map(|t| &t.item).collect::<Vec<_>>());

        // http://www.gg.e-mansion.com/~kkatoh/program/novel2/novel208.html
        let mut stack = vec![];
        let mut buf = vec![];
        for token in tks {
            match &token.item {
                Item::Ident(_) | Item::Num(_) => buf.push(token),
                Item::LParen => stack.push(token),
                Item::Ops(incoming) => {
                    loop {
                        match stack.last() {
                            Some(Token {
                                item: Item::Ops(op),
                                ..
                            }) if incoming > op => {
                                buf.push(stack.pop().unwrap());
                            }
                            _ => break,
                        }
                    }
                    stack.push(token);
                }
                Item::RParen => loop {
                    if let Some(i) = stack.pop() {
                        if i.item == Item::LParen {
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
                    Item::Ident(s) => RPNode::Ident(s.clone()),
                    Item::Num(n) => RPNode::Num(*n),
                    Item::Ops(op) => RPNode::Ops(*op),
                    Item::LParen => {
                        return Err(Error::NoPairParen(tk.clone()));
                    }
                    _ => unreachable!(tk),
                })
            })
            .collect::<Result<_, _>>()?;
        Ok(Expr { content })
    }
}
