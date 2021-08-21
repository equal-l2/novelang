use std::iter::Peekable;

use crate::exprs::{items::*, Expr};
use crate::lex::{self, Items, Token};

use super::ParseError;

macro_rules! ensure_start {
    ($tks: ident) => {
        let front = $tks.peek();
        log::trace!("parsing {}", std::any::type_name::<Self>(),);
        match front {
            Some((_, tk)) => {
                if !Self::can_start_with(&tk.item) {
                    return Err(ParseError::UnexpectedToken((*tk).clone()));
                }
            }
            None => {
                return Err(ParseError::TokenExhausted);
            }
        }
    };
}

trait LookItem {
    fn item(self) -> Option<Items>;
}

impl<T> LookItem for Option<&(T, &Token)> {
    fn item(self) -> Option<Items> {
        self.map(|t| t.1.item.clone())
    }
}

impl<T> LookItem for Option<(T, &Token)> {
    fn item(self) -> Option<Items> {
        self.map(|t| t.1.item.clone())
    }
}

type Result<T> = std::result::Result<T, ParseError>;

pub(super) trait TryFromTokens<'a> {
    fn can_start_with(item: &Items) -> bool;
    fn try_from_tokens<T>(tks: &mut Peekable<T>) -> Result<Self>
    where
        T: Iterator<Item = (usize, &'a Token)>,
        Self: Sized;
}

impl<'a> TryFromTokens<'a> for Expr {
    fn can_start_with(item: &Items) -> bool {
        Log::can_start_with(item)
    }
    fn try_from_tokens<T>(tks: &mut Peekable<T>) -> Result<Self>
    where
        T: Iterator<Item = (usize, &'a Token)>,
        Self: Sized,
    {
        ensure_start!(tks);

        Ok(Self {
            content: TopItem::try_from_tokens(tks)?,
        })
    }
}

impl<'a> TryFromTokens<'a> for Log {
    fn can_start_with(item: &Items) -> bool {
        Equ::can_start_with(item)
    }

    fn try_from_tokens<T>(tks: &mut Peekable<T>) -> Result<Self>
    where
        T: Iterator<Item = (usize, &'a Token)>,
    {
        ensure_start!(tks);

        let mut lop = Log::Single(Equ::try_from_tokens(tks)?);
        loop {
            if let Some(Items::Op(lex::Ops::Log(op))) = tks.peek().item() {
                let _ = tks.next().unwrap();
                let rop = Equ::try_from_tokens(tks)?;
                match op {
                    lex::LogOps::And => lop = Self::And(Box::new(lop), rop),
                    lex::LogOps::Or => lop = Self::Or(Box::new(lop), rop),
                }
            } else {
                return Ok(lop);
            }
        }
    }
}

impl<'a> TryFromTokens<'a> for Equ {
    fn can_start_with(item: &Items) -> bool {
        Rel::can_start_with(item)
    }

    fn try_from_tokens<T>(tks: &mut Peekable<T>) -> Result<Self>
    where
        T: Iterator<Item = (usize, &'a Token)>,
    {
        ensure_start!(tks);

        let mut lop = Equ::Single(Rel::try_from_tokens(tks)?);
        loop {
            if let Some(Items::Op(lex::Ops::Equ(op))) = tks.peek().item() {
                let _ = tks.next().unwrap();
                let rop = Rel::try_from_tokens(tks)?;
                match op {
                    lex::EquOps::Equal => lop = Self::Equal(Box::new(lop), rop),
                    lex::EquOps::NotEqual => lop = Self::NotEqual(Box::new(lop), rop),
                }
            } else {
                return Ok(lop);
            }
        }
    }
}

impl<'a> TryFromTokens<'a> for Rel {
    fn can_start_with(item: &Items) -> bool {
        AddSub::can_start_with(item)
    }

    fn try_from_tokens<T>(tks: &mut Peekable<T>) -> Result<Self>
    where
        T: Iterator<Item = (usize, &'a Token)>,
    {
        ensure_start!(tks);

        let lop = AddSub::try_from_tokens(tks)?;
        Ok(
            if let Some(Items::Op(lex::Ops::Rel(op))) = tks.peek().item() {
                let _ = tks.next().unwrap();
                let rop = AddSub::try_from_tokens(tks)?;
                match op {
                    lex::RelOps::LessEqual => Self::LessEqual(lop, rop),
                    lex::RelOps::GreaterEqual => Self::GreaterEqual(lop, rop),
                    lex::RelOps::LessThan => Self::LessThan(lop, rop),
                    lex::RelOps::GreaterThan => Self::GreaterThan(lop, rop),
                }
            } else {
                Self::Single(lop)
            },
        )
    }
}

impl<'a> TryFromTokens<'a> for AddSub {
    fn can_start_with(item: &Items) -> bool {
        MulDiv::can_start_with(item)
    }

    fn try_from_tokens<T>(tks: &mut Peekable<T>) -> Result<Self>
    where
        T: Iterator<Item = (usize, &'a Token)>,
    {
        ensure_start!(tks);

        let mut lop = AddSub::Single(MulDiv::try_from_tokens(tks)?);
        loop {
            if let Some(Items::Op(lex::Ops::Add(op))) = tks.peek().item() {
                let _ = tks.next().unwrap();
                let rop = MulDiv::try_from_tokens(tks)?;
                match op {
                    lex::AddOps::Add => lop = Self::Add(Box::new(lop), rop),
                    lex::AddOps::Sub => lop = Self::Sub(Box::new(lop), rop),
                }
            } else {
                return Ok(lop);
            }
        }
    }
}

impl<'a> TryFromTokens<'a> for MulDiv {
    fn can_start_with(item: &Items) -> bool {
        Node::can_start_with(item)
    }

    fn try_from_tokens<T>(tks: &mut Peekable<T>) -> Result<Self>
    where
        T: Iterator<Item = (usize, &'a Token)>,
    {
        ensure_start!(tks);

        let mut lop = MulDiv::Single(Node::try_from_tokens(tks)?);
        loop {
            if let Some(Items::Op(lex::Ops::Mul(op))) = tks.peek().item() {
                let _ = tks.next().unwrap();
                let rop = Node::try_from_tokens(tks)?;
                match op {
                    lex::MulOps::Mul => lop = Self::Mul(Box::new(lop), rop),
                    lex::MulOps::Div => lop = Self::Div(Box::new(lop), rop),
                    lex::MulOps::Mod => lop = Self::Mod(Box::new(lop), rop),
                }
            } else {
                return Ok(lop);
            }
        }
    }
}

impl<'a> TryFromTokens<'a> for Node {
    fn can_start_with(item: &Items) -> bool {
        use lex::{AddOps, Ops};
        matches!(item, Items::Op(Ops::Add(AddOps::Add | AddOps::Sub)))
            || Value::can_start_with(item)
    }

    fn try_from_tokens<T>(tks: &mut Peekable<T>) -> Result<Self>
    where
        T: Iterator<Item = (usize, &'a Token)>,
    {
        use lex::{AddOps, Ops};

        ensure_start!(tks);

        Ok(if let Some(Items::Op(Ops::Add(op))) = tks.peek().item() {
            let _ = tks.next().unwrap();
            let operand = Self::try_from_tokens(tks)?;
            match op {
                AddOps::Add => Self::Plus(Box::new(operand)),
                AddOps::Sub => Self::Minus(Box::new(operand)),
            }
        } else {
            let operand = Value::try_from_tokens(tks)?;
            Self::Single(operand)
        })
    }
}

impl<'a> TryFromTokens<'a> for Value {
    fn can_start_with(item: &Items) -> bool {
        Core::can_start_with(item)
    }

    fn try_from_tokens<T>(tks: &mut Peekable<T>) -> Result<Self>
    where
        T: Iterator<Item = (usize, &'a Token)>,
    {
        ensure_start!(tks);

        let mut val = Self::Single(Core::try_from_tokens(tks)?);
        loop {
            if let Some(Items::LBra) = tks.peek().item() {
                let _ = tks.next().unwrap();
                let r = TopNum::try_from_tokens(tks)?;
                if let Some(Items::RBra, ..) = tks.peek().item() {
                    let _ = tks.next().unwrap();
                    val = Self::ArrElem(Box::new(val), Box::new(r));
                } else {
                    return Err(tks.next().map_or(ParseError::TokenExhausted, |(_, tk)| {
                        ParseError::UnexpectedToken(tk.clone())
                    }));
                }
            } else {
                return Ok(val);
            }
        }
    }
}

impl<'a> TryFromTokens<'a> for Core {
    fn can_start_with(item: &Items) -> bool {
        use lex::Keyword;
        matches!(
            item,
            Items::Str(_)
                | Items::Num(_, _)
                | Items::Ident(_)
                | Items::Key(Keyword::True | Keyword::False)
                | Items::LPar
                | Items::LBra
        )
    }

    fn try_from_tokens<T>(tks: &mut Peekable<T>) -> Result<Self>
    where
        T: Iterator<Item = (usize, &'a Token)>,
    {
        use lex::Keyword;

        ensure_start!(tks);

        let tok = tks.next().unwrap().1;
        Ok(match &tok.item {
            Items::Str(s) => Self::Str(s.clone()),
            Items::Num(n, _) => Self::Num(*n),
            Items::Ident(s) => Self::Ident(s.clone()),
            Items::Key(Keyword::True) => Self::True,
            Items::Key(Keyword::False) => Self::False,
            Items::LPar => {
                let expr = TopItem::try_from_tokens(tks)?;
                let next_tk = tks.next();
                match next_tk.item() {
                    Some(Items::RPar) => Self::Paren(Box::new(expr)),
                    _ => {
                        return Err(ParseError::NoPairParen {
                            lparen: tok.clone(),
                        })
                    }
                }
            }
            Items::LBra => {
                // TODO: allow empty array
                let mut v = vec![TopItem::try_from_tokens(tks)?];
                let tk_opt = tks.next().clone();
                if let Some((_, tk)) = tk_opt {
                    match tk.item {
                        Items::Comma => {
                            loop {
                                let next = TopItem::try_from_tokens(tks)?;
                                v.push(next);
                                if let Some((_, t)) = tks.next() {
                                    match t.item {
                                        Items::Comma => {
                                            // continue
                                        }
                                        Items::RBra => break Self::Arr(v),
                                        _ => return Err(ParseError::UnexpectedToken((*t).clone())),
                                    }
                                } else {
                                    return Err(ParseError::TokenExhausted);
                                }
                            }
                        }
                        Items::RBra => Self::Arr(v),
                        _ => return Err(ParseError::UnexpectedToken(tk.clone())),
                    }
                } else {
                    return Err(ParseError::TokenExhausted);
                }
            }
            _ => todo!("{:?}", &tok.item),
        })
    }
}
