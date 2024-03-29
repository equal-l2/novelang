use std::iter::Peekable;

use super::{
    lex::{self, LangItem, Token},
    Expr, LookItem, Span, Spannable,
};

use crate::exprs::items::*;

#[derive(Debug)]
enum ErrorKind {
    UnexpectedToken,
    NoPairParen,
    TokenExhausted,
}

#[derive(Debug)]
pub struct ExprError {
    kind: ErrorKind,
    span: Span,
}

#[allow(clippy::missing_const_for_fn)]
impl ExprError {
    fn exhausted(from: usize, to: usize) -> Self {
        Self {
            kind: ErrorKind::TokenExhausted,
            span: Span(from, to),
        }
    }

    fn unexpected(idx: usize) -> Self {
        Self {
            kind: ErrorKind::UnexpectedToken,
            span: idx.into(),
        }
    }

    fn no_pair_paren(from: usize, to: usize) -> Self {
        Self {
            kind: ErrorKind::NoPairParen,
            span: Span(from, to),
        }
    }
}

impl From<ExprError> for (super::Error, Span) {
    fn from(err: ExprError) -> Self {
        (
            super::Error(
                match err.kind {
                    ErrorKind::UnexpectedToken => "Failed to parse expr because of this token",
                    ErrorKind::NoPairParen => "Paren doesn't have its pair",
                    ErrorKind::TokenExhausted => "Expression abruptly ended",
                }
                .into(),
            ),
            err.span,
        )
    }
}

macro_rules! ensure_start {
    ($tks: ident, $last: expr) => {
        let front = $tks.peek();
        match front {
            Some((i, tk)) => {
                if !Self::can_start_with(&tk.item) {
                    return Err(ExprError::unexpected(*i));
                }
            }
            None => {
                return Err(ExprError::exhausted($last, $last));
            }
        }
    };
}

impl<T> LookItem for Option<&(T, &Token)> {
    fn item(self) -> Option<LangItem> {
        self.map(|t| t.1.item.clone())
    }
}

impl<T> LookItem for Option<(T, &Token)> {
    fn item(self) -> Option<LangItem> {
        self.map(|t| t.1.item.clone())
    }
}

type Result<T> = std::result::Result<T, ExprError>;

pub(super) trait TryFromTokens<'a> {
    fn can_start_with(item: &LangItem) -> bool;
    fn try_from_tokens<T>(tks: &mut Peekable<T>, last: usize) -> Result<Self>
    where
        T: Iterator<Item = (usize, &'a Token)>,
        Self: Sized;
}

impl<'a> TryFromTokens<'a> for Expr {
    fn can_start_with(item: &LangItem) -> bool {
        Log::can_start_with(item)
    }
    fn try_from_tokens<T>(tks: &mut Peekable<T>, last: usize) -> Result<Self>
    where
        T: Iterator<Item = (usize, &'a Token)>,
        Self: Sized,
    {
        ensure_start!(tks, last);

        Ok(Self {
            content: Box::from(TopItem::try_from_tokens(tks, last)?),
        })
    }
}

impl<'a> TryFromTokens<'a> for Log {
    fn can_start_with(item: &LangItem) -> bool {
        Equ::can_start_with(item)
    }

    fn try_from_tokens<T>(tks: &mut Peekable<T>, last: usize) -> Result<Self>
    where
        T: Iterator<Item = (usize, &'a Token)>,
    {
        ensure_start!(tks, last);

        let mut lop = Self::Single(Equ::try_from_tokens(tks, last)?);
        loop {
            if let Some(LangItem::Op(lex::Ops::Log(op))) = tks.peek().item() {
                let _ = tks.next().unwrap();
                let rop = Equ::try_from_tokens(tks, last)?;
                match op {
                    lex::LogOps::And => lop = Self::And(Box::from(lop), rop),
                    lex::LogOps::Or => lop = Self::Or(Box::from(lop), rop),
                }
            } else {
                return Ok(lop);
            }
        }
    }
}

impl<'a> TryFromTokens<'a> for Equ {
    fn can_start_with(item: &LangItem) -> bool {
        Rel::can_start_with(item)
    }

    fn try_from_tokens<T>(tks: &mut Peekable<T>, last: usize) -> Result<Self>
    where
        T: Iterator<Item = (usize, &'a Token)>,
    {
        ensure_start!(tks, last);

        let mut lop = Self::Single(Rel::try_from_tokens(tks, last)?);
        loop {
            if let Some(LangItem::Op(lex::Ops::Equ(op))) = tks.peek().item() {
                let _ = tks.next().unwrap();
                let rop = Rel::try_from_tokens(tks, last)?;
                match op {
                    lex::EquOps::Equal => lop = Self::Equal(Box::from(lop), rop),
                    lex::EquOps::NotEqual => lop = Self::NotEqual(Box::from(lop), rop),
                }
            } else {
                return Ok(lop);
            }
        }
    }
}

impl<'a> TryFromTokens<'a> for Rel {
    fn can_start_with(item: &LangItem) -> bool {
        AddSub::can_start_with(item)
    }

    fn try_from_tokens<T>(tks: &mut Peekable<T>, last: usize) -> Result<Self>
    where
        T: Iterator<Item = (usize, &'a Token)>,
    {
        ensure_start!(tks, last);

        let lop = AddSub::try_from_tokens(tks, last)?;
        Ok(
            if let Some(LangItem::Op(lex::Ops::Rel(op))) = tks.peek().item() {
                let _ = tks.next().unwrap();
                let rop = AddSub::try_from_tokens(tks, last)?;
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
    fn can_start_with(item: &LangItem) -> bool {
        MulDiv::can_start_with(item)
    }

    fn try_from_tokens<T>(tks: &mut Peekable<T>, last: usize) -> Result<Self>
    where
        T: Iterator<Item = (usize, &'a Token)>,
    {
        ensure_start!(tks, last);

        let mut lop = Self::Single(MulDiv::try_from_tokens(tks, last)?);
        loop {
            if let Some(LangItem::Op(lex::Ops::Add(op))) = tks.peek().item() {
                let _ = tks.next().unwrap();
                let rop = MulDiv::try_from_tokens(tks, last)?;
                match op {
                    lex::AddOps::Add => lop = Self::Add(Box::from(lop), rop),
                    lex::AddOps::Sub => lop = Self::Sub(Box::from(lop), rop),
                }
            } else {
                return Ok(lop);
            }
        }
    }
}

impl<'a> TryFromTokens<'a> for MulDiv {
    fn can_start_with(item: &LangItem) -> bool {
        Node::can_start_with(item)
    }

    fn try_from_tokens<T>(tks: &mut Peekable<T>, last: usize) -> Result<Self>
    where
        T: Iterator<Item = (usize, &'a Token)>,
    {
        ensure_start!(tks, last);

        let mut lop = Self::Single(Node::try_from_tokens(tks, last)?);
        loop {
            if let Some(LangItem::Op(lex::Ops::Mul(op))) = tks.peek().item() {
                let _ = tks.next().unwrap();
                let rop = Node::try_from_tokens(tks, last)?;
                match op {
                    lex::MulOps::Mul => lop = Self::Mul(Box::from(lop), rop),
                    lex::MulOps::Div => lop = Self::Div(Box::from(lop), rop),
                    lex::MulOps::Mod => lop = Self::Mod(Box::from(lop), rop),
                }
            } else {
                return Ok(lop);
            }
        }
    }
}

impl<'a> TryFromTokens<'a> for Node {
    fn can_start_with(item: &LangItem) -> bool {
        use lex::{AddOps, Ops};
        matches!(item, LangItem::Op(Ops::Add(AddOps::Add | AddOps::Sub)))
            || Value::can_start_with(item)
    }

    fn try_from_tokens<T>(tks: &mut Peekable<T>, last: usize) -> Result<Self>
    where
        T: Iterator<Item = (usize, &'a Token)>,
    {
        use lex::{AddOps, Ops};

        ensure_start!(tks, last);

        Ok(
            if let Some(LangItem::Op(Ops::Add(op))) = tks.peek().item() {
                let (from, _) = tks.next().unwrap();
                let operand = Self::try_from_tokens(tks, last)?;
                let to = operand.span().1;
                let span = Span(from, to);
                match op {
                    AddOps::Add => Self::Plus(Box::from(operand), span),
                    AddOps::Sub => Self::Minus(Box::from(operand), span),
                }
            } else {
                let operand = Value::try_from_tokens(tks, last)?;
                Self::Single(operand)
            },
        )
    }
}

impl<'a> TryFromTokens<'a> for Value {
    fn can_start_with(item: &LangItem) -> bool {
        Core::can_start_with(item)
    }

    fn try_from_tokens<T>(tks: &mut Peekable<T>, last: usize) -> Result<Self>
    where
        T: Iterator<Item = (usize, &'a Token)>,
    {
        ensure_start!(tks, last);

        let mut val = Self::Single(Core::try_from_tokens(tks, last)?);
        let from = val.span().0;
        loop {
            if tks.peek().item() == Some(LangItem::LBra) {
                let _ = tks.next().unwrap();
                let r = TopNum::try_from_tokens(tks, last)?;
                if let Some(LangItem::RBra, ..) = tks.peek().item() {
                    let (idx, _) = tks.next().unwrap();
                    val = Self::ArrElem(Box::from(val), Box::from(r), Span(from, idx));
                } else {
                    return Err(tks
                        .next()
                        .map_or(ExprError::exhausted(from, last), |(i, _)| {
                            ExprError::unexpected(i)
                        }));
                }
            } else {
                return Ok(val);
            }
        }
    }
}

impl<'a> TryFromTokens<'a> for Core {
    fn can_start_with(item: &LangItem) -> bool {
        matches!(
            item,
            LangItem::Str(_)
                | LangItem::Num(_, _)
                | LangItem::Ident(_)
                | LangItem::Bool(_)
                | LangItem::LPar
                | LangItem::LBra
        )
    }

    fn try_from_tokens<T>(tks: &mut Peekable<T>, last: usize) -> Result<Self>
    where
        T: Iterator<Item = (usize, &'a Token)>,
    {
        use lex::Boolean;

        ensure_start!(tks, last);

        let (start, tok) = tks.next().unwrap();
        Ok(match &tok.item {
            LangItem::Str(s) => Self::Str(s.clone(), start.into()),
            LangItem::Num(n, _) => Self::Num(*n, start.into()),
            LangItem::Ident(s) => Self::Ident(crate::target::Ident(s.clone(), start.into())),
            LangItem::Bool(Boolean::True) => Self::True(start.into()),
            LangItem::Bool(Boolean::False) => Self::False(start.into()),
            LangItem::LPar => {
                let expr = TopItem::try_from_tokens(tks, last)?;
                let next_tk = tks.next();
                match next_tk.item() {
                    Some(LangItem::RPar) => {
                        Self::Paren(Box::from(expr), Span(start, next_tk.unwrap().0))
                    }
                    Some(_) => {
                        let to = next_tk.unwrap().0;
                        return Err(ExprError::no_pair_paren(start, to));
                    }
                    None => return Err(ExprError::exhausted(start, last)),
                }
            }
            LangItem::LBra => {
                // TODO: allow empty array
                let mut v = vec![TopItem::try_from_tokens(tks, last)?];
                let tk_opt = tks.next();
                if let Some((idx, tk)) = tk_opt {
                    match tk.item {
                        LangItem::Comma => {
                            loop {
                                let next = TopItem::try_from_tokens(tks, last)?;
                                v.push(next);
                                if let Some((i, t)) = tks.next() {
                                    match t.item {
                                        LangItem::Comma => {
                                            // continue
                                        }
                                        LangItem::RBra => break Self::Arr(v, Span(start, i)),
                                        _ => return Err(ExprError::unexpected(i)),
                                    }
                                } else {
                                    return Err(ExprError::exhausted(start, last));
                                }
                            }
                        }
                        LangItem::RBra => Self::Arr(v, Span(start, idx)),
                        _ => return Err(ExprError::unexpected(idx)),
                    }
                } else {
                    return Err(ExprError::exhausted(start, last));
                }
            }
            // I'm not sure if this arm would catch anything
            _ => unimplemented!("{:?}", &tok.item),
        })
    }
}
