use std::iter::Peekable;

use crate::exprs::{items::*, Expr};
use crate::lex::{self, Items, Token};

use super::ParseError;

macro_rules! ensure_start {
    ($tks: ident) => {
        match $tks.peek() {
            Some(tk) => {
                if !Self::can_start_with(&tk.item) {
                    return Err(ParseError::InvalidToken((*tk).clone()));
                }
            }
            None => {
                return Err(ParseError::TokenExhausted);
            }
        }
    };
}

type Result<T> = std::result::Result<T, ParseError>;

pub(super) trait TryFromTokens<'a> {
    fn can_start_with(item: &Items) -> bool;
    fn try_from_tokens<T>(tks: &mut Peekable<T>) -> Result<Self>
    where
        T: Iterator<Item = &'a Token>,
        Self: Sized;
}

impl<'a> TryFromTokens<'a> for Expr {
    fn can_start_with(item: &Items) -> bool {
        Rel::can_start_with(item)
    }
    fn try_from_tokens<T>(tks: &mut Peekable<T>) -> Result<Self>
    where
        T: Iterator<Item = &'a Token>,
        Self: Sized,
    {
        let expr = Rel::try_from_tokens(tks)?;

        if let Some(tk) = tks.next() {
            return Err(ParseError::TrailingToken { from: tk.clone() });
        }

        Ok(Self { content: expr })
    }
}

impl<'a> TryFromTokens<'a> for Rel {
    fn can_start_with(item: &Items) -> bool {
        AddSub::can_start_with(item)
    }

    fn try_from_tokens<T>(tks: &mut Peekable<T>) -> Result<Self>
    where
        T: Iterator<Item = &'a Token>,
    {
        use lex::{Ops, RelOps};

        ensure_start!(tks);

        let lop = AddSub::try_from_tokens(tks)?;
        Ok(match tks.peek() {
            Some(Token {
                item: Items::Ops(Ops::Rel(op)),
                ..
            }) => {
                let _ = tks.next().unwrap();
                let rop = AddSub::try_from_tokens(tks)?;
                match op {
                    RelOps::Equal => Self::Equal(lop, rop),
                    RelOps::NotEqual => Self::NotEqual(lop, rop),
                    RelOps::LessEqual => Self::LessEqual(lop, rop),
                    RelOps::GreaterEqual => Self::GreaterEqual(lop, rop),
                    RelOps::LessThan => Self::LessThan(lop, rop),
                    RelOps::GreaterThan => Self::GreaterThan(lop, rop),
                }
            }
            _ => Self::Single(lop),
        })
    }
}

impl<'a> TryFromTokens<'a> for AddSub {
    fn can_start_with(item: &Items) -> bool {
        MulDiv::can_start_with(item)
    }

    fn try_from_tokens<T>(tks: &mut Peekable<T>) -> Result<Self>
    where
        T: Iterator<Item = &'a Token>,
    {
        use lex::{AriOps, Ops};

        ensure_start!(tks);

        let lop = MulDiv::try_from_tokens(tks)?;
        Ok(match tks.peek() {
            Some(Token {
                item: Items::Ops(Ops::Ari(op)),
                ..
            }) => match op {
                AriOps::Add | AriOps::Sub => {
                    let _ = tks.next().unwrap();
                    let rop = Self::try_from_tokens(tks)?;
                    match op {
                        AriOps::Add => Self::Add(lop, Box::new(rop)),
                        AriOps::Sub => Self::Sub(lop, Box::new(rop)),
                        _ => unreachable!(),
                    }
                }
                _ => Self::Single(lop),
            },
            _ => Self::Single(lop),
        })
    }
}

impl<'a> TryFromTokens<'a> for MulDiv {
    fn can_start_with(item: &Items) -> bool {
        Node::can_start_with(item)
    }

    fn try_from_tokens<T>(tks: &mut Peekable<T>) -> Result<Self>
    where
        T: Iterator<Item = &'a Token>,
    {
        use lex::{AriOps, Ops};

        ensure_start!(tks);

        let lop = Node::try_from_tokens(tks)?;
        Ok(match tks.peek() {
            Some(Token {
                item: Items::Ops(Ops::Ari(op)),
                ..
            }) => match op {
                AriOps::Mul | AriOps::Div | AriOps::Mod => {
                    let _ = tks.next().unwrap();
                    let rop = Self::try_from_tokens(tks)?;
                    match op {
                        AriOps::Mul => Self::Mul(lop, Box::new(rop)),
                        AriOps::Div => Self::Div(lop, Box::new(rop)),
                        AriOps::Mod => Self::Mod(lop, Box::new(rop)),
                        _ => unreachable!(),
                    }
                }
                _ => Self::Single(lop),
            },
            _ => Self::Single(lop),
        })
    }
}

impl<'a> TryFromTokens<'a> for Node {
    fn can_start_with(item: &Items) -> bool {
        use lex::{AriOps, Ops};
        matches!(item, Items::Ops(Ops::Ari(AriOps::Add | AriOps::Sub)))
            || Core::can_start_with(item)
    }

    fn try_from_tokens<T>(tks: &mut Peekable<T>) -> Result<Self>
    where
        T: Iterator<Item = &'a Token>,
    {
        use lex::{AriOps, Ops};

        ensure_start!(tks);

        Ok(
            if let Some(Token {
                item: Items::Ops(Ops::Ari(op)),
                ..
            }) = tks.peek()
            {
                let tk = tks.next().unwrap();
                match op {
                    AriOps::Add | AriOps::Sub => {
                        let operand = Self::try_from_tokens(tks)?;
                        match op {
                            AriOps::Add => Self::Plus(Box::new(operand)),
                            AriOps::Sub => Self::Minus(Box::new(operand)),
                            _ => unreachable!(),
                        }
                    }
                    _ => return Err(ParseError::InvalidToken(tk.clone())),
                }
            } else {
                let operand = Core::try_from_tokens(tks)?;
                Self::Single(operand)
            },
        )
    }
}

impl<'a> TryFromTokens<'a> for Core {
    fn can_start_with(item: &Items) -> bool {
        use lex::Keywords;
        matches!(
            item,
            Items::Str(_)
                | Items::Num(_, _)
                | Items::Ident(_)
                | Items::Key(Keywords::True | Keywords::False)
                | Items::LParen
        )
    }

    fn try_from_tokens<T>(tks: &mut Peekable<T>) -> Result<Self>
    where
        T: Iterator<Item = &'a Token>,
    {
        use lex::Keywords;

        ensure_start!(tks);

        let tk = tks.next().unwrap();
        Ok(match &tk.item {
            Items::Str(s) => Self::Str(s.clone()),
            Items::Num(n, _) => Self::Num(*n),
            Items::Ident(s) => Self::Ident(s.clone()),
            Items::Key(Keywords::True) => Self::True,
            Items::Key(Keywords::False) => Self::False,
            Items::LParen => {
                let rel = Rel::try_from_tokens(tks)?;

                let next_tk = tks.next();
                match next_tk {
                    Some(Token {
                        item: Items::RParen,
                        ..
                    }) => Self::Paren(Box::new(rel)),
                    _ => Err(ParseError::NoPairParen { lparen: tk.clone() })?,
                }
            }
            _ => todo!("{:?}", &tk.item),
        })
    }
}
