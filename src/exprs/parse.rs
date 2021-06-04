use std::iter::Peekable;

use crate::lex::{self, Items, Token};

use super::items::*;

pub trait FromTokens<'a> {
    fn from_tokens<T>(tks: &mut Peekable<T>) -> Self
    where
        T: Iterator<Item = &'a Token>;
}

impl<'a> FromTokens<'a> for Rel {
    fn from_tokens<T>(tks: &mut Peekable<T>) -> Self
    where
        T: Iterator<Item = &'a Token>,
    {
        use lex::{Ops, RelOps};
        let lop = AddSub::from_tokens(tks);
        match tks.peek() {
            Some(Token {
                item: Items::Ops(Ops::Rel(op)),
                ..
            }) => {
                let _ = tks.next().unwrap();
                let rop = AddSub::from_tokens(tks);
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
        }
    }
}

impl<'a> FromTokens<'a> for AddSub {
    fn from_tokens<T>(tks: &mut Peekable<T>) -> Self
    where
        T: Iterator<Item = &'a Token>,
    {
        use lex::{AriOps, Ops};
        let lop = MulDiv::from_tokens(tks);
        match tks.peek() {
            Some(Token {
                item: Items::Ops(Ops::Ari(op)),
                ..
            }) => match op {
                AriOps::Add | AriOps::Sub => {
                    let _ = tks.next().unwrap();
                    let rop = Self::from_tokens(tks);
                    match op {
                        AriOps::Add => Self::Add(lop, Box::new(rop)),
                        AriOps::Sub => Self::Sub(lop, Box::new(rop)),
                        _ => unreachable!(),
                    }
                }
                _ => Self::Single(lop),
            },
            _ => Self::Single(lop),
        }
    }
}

impl<'a> FromTokens<'a> for MulDiv {
    fn from_tokens<T>(tks: &mut Peekable<T>) -> Self
    where
        T: Iterator<Item = &'a Token>,
    {
        use lex::{AriOps, Ops};
        let lop = Node::from_tokens(tks);
        match tks.peek() {
            Some(Token {
                item: Items::Ops(Ops::Ari(op)),
                ..
            }) => match op {
                AriOps::Mul | AriOps::Div | AriOps::Mod => {
                    let _ = tks.next().unwrap();
                    let rop = Self::from_tokens(tks);
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
        }
    }
}

impl<'a> FromTokens<'a> for Node {
    fn from_tokens<T>(tks: &mut Peekable<T>) -> Self
    where
        T: Iterator<Item = &'a Token>,
    {
        use lex::{AriOps, Ops};
        if let Some(Token {
            item: Items::Ops(Ops::Ari(op)),
            ..
        }) = tks.peek()
        {
            match op {
                AriOps::Add | AriOps::Sub => {
                    let _ = tks.next().unwrap();
                    let operand = Self::from_tokens(tks);
                    match op {
                        AriOps::Add => Self::Plus(Box::new(operand)),
                        AriOps::Sub => Self::Minus(Box::new(operand)),
                        _ => unreachable!(),
                    }
                }
                _ => todo!("{:?}", op),
            }
        } else {
            let operand = Core::from_tokens(tks);
            Self::Single(operand)
        }
    }
}

impl<'a> FromTokens<'a> for Core {
    fn from_tokens<T>(tks: &mut Peekable<T>) -> Self
    where
        T: Iterator<Item = &'a Token>,
    {
        use lex::Keywords;
        let tk = tks.next().unwrap();
        match &tk.item {
            Items::Str(s) => Self::Str(s.clone()),
            Items::Num(n, _) => Self::Num(*n),
            Items::Ident(s) => Self::Ident(s.clone()),
            Items::Key(Keywords::True) => Self::True,
            Items::Key(Keywords::False) => Self::False,
            Items::LParen => {
                let rel = Rel::from_tokens(tks);
                let _ = tks.next().unwrap(); // discard ')' (with no error handling :))
                Self::Paren(Box::new(rel))
            }
            _ => todo!("{:?}", &tk.item),
        }
    }
}
