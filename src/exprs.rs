#[derive(Clone, Debug)]
pub struct Expr {
    pub content: Box<items::TopItem>,
}

#[derive(Clone, Debug)]
pub struct Span(pub usize, pub usize);

impl From<usize> for Span {
    fn from(i: usize) -> Self {
        Self(i, i)
    }
}

impl From<String> for Expr {
    fn from(s: String) -> Self {
        use items::*;
        Self {
            content: Box::from(Log::Single(Equ::Single(Rel::Single(AddSub::Single(MulDiv::Single(
                Node::Single(Value::Single(Core::Str(s, Span(0, 0)))),
            )))))),
        }
    }
}

pub mod items {
    use super::Span;
    pub type TopItem = Log;
    pub type TopNum = AddSub;

    #[derive(Clone, Debug)]
    pub enum Log {
        Single(Equ),
        And(Box<Self>, Equ),
        Or(Box<Self>, Equ),
    }

    #[derive(Clone, Debug)]
    pub enum Equ {
        Single(Rel),
        Equal(Box<Self>, Rel),
        NotEqual(Box<Self>, Rel),
    }

    #[derive(Clone, Debug)]
    pub enum Rel {
        Single(AddSub),
        LessEqual(AddSub, AddSub),
        GreaterEqual(AddSub, AddSub),
        LessThan(AddSub, AddSub),
        GreaterThan(AddSub, AddSub),
    }

    #[derive(Clone, Debug)]
    pub enum AddSub {
        Single(MulDiv),
        Add(Box<Self>, MulDiv),
        Sub(Box<Self>, MulDiv),
    }

    #[derive(Clone, Debug)]
    pub enum MulDiv {
        Single(Node),
        Mul(Box<Self>, Node),
        Div(Box<Self>, Node),
        Mod(Box<Self>, Node),
    }

    #[derive(Clone, Debug)]
    pub enum Node {
        Single(Value),
        Plus(Box<Self>, Span),
        Minus(Box<Self>, Span),
    }

    #[derive(Clone, Debug)]
    pub enum Value {
        Single(Core),
        ArrElem(Box<Self>, Box<TopNum>, Span),
    }

    #[derive(Clone, Debug)]
    pub enum Core {
        Str(String, Span),
        Num(crate::types::IntType, Span),
        Ident(String, Span),
        True(Span),
        False(Span),
        Paren(Box<TopItem>, Span),
        Arr(Vec<TopItem>, Span),
    }
}

pub mod span {
    use super::items::*;
    use super::Expr;
    pub use super::Span;

    pub trait Spannable {
        fn get_span(&self) -> Span;
    }

    impl Spannable for Expr {
        fn get_span(&self) -> Span {
            self.content.get_span()
        }
    }

    impl Spannable for Log {
        fn get_span(&self) -> Span {
            match self {
                Log::Single(i) => i.get_span(),
                Log::And(l, r) | Log::Or(l, r) => {
                    let from = l.get_span().0;
                    let to = r.get_span().1;
                    Span(from, to)
                }
            }
        }
    }

    impl Spannable for Equ {
        fn get_span(&self) -> Span {
            match self {
                Equ::Single(i) => i.get_span(),
                Equ::Equal(l, r) | Equ::NotEqual(l, r) => {
                    let from = l.get_span().0;
                    let to = r.get_span().1;
                    Span(from, to)
                }
            }
        }
    }

    impl Spannable for Rel {
        fn get_span(&self) -> Span {
            match self {
                Rel::Single(i) => i.get_span(),
                Rel::LessEqual(l, r)
                | Rel::GreaterEqual(l, r)
                | Rel::LessThan(l, r)
                | Rel::GreaterThan(l, r) => {
                    let from = l.get_span().0;
                    let to = r.get_span().1;
                    Span(from, to)
                }
            }
        }
    }

    impl Spannable for AddSub {
        fn get_span(&self) -> Span {
            match self {
                AddSub::Single(i) => i.get_span(),
                AddSub::Add(l, r) | AddSub::Sub(l, r) => {
                    let from = l.get_span().0;
                    let to = r.get_span().1;
                    Span(from, to)
                }
            }
        }
    }

    impl Spannable for MulDiv {
        fn get_span(&self) -> Span {
            match self {
                MulDiv::Single(i) => i.get_span(),
                MulDiv::Mul(l, r) | MulDiv::Div(l, r) | MulDiv::Mod(l, r) => {
                    let from = l.get_span().0;
                    let to = r.get_span().1;
                    Span(from, to)
                }
            }
        }
    }

    impl Spannable for Node {
        fn get_span(&self) -> Span {
            match self {
                Node::Single(i) => i.get_span(),
                Node::Plus(_, s) | Node::Minus(_, s) => s.clone(),
            }
        }
    }

    impl Spannable for Value {
        fn get_span(&self) -> Span {
            match self {
                Value::Single(i) => i.get_span(),
                Value::ArrElem(_, _, s) => s.clone(),
            }
        }
    }

    impl Spannable for Core {
        fn get_span(&self) -> Span {
            match self {
                Core::Str(_, s)
                | Core::Num(_, s)
                | Core::Ident(_, s)
                | Core::True(s)
                | Core::False(s)
                | Core::Paren(_, s)
                | Core::Arr(_, s) => s.clone(),
            }
        }
    }
}

pub mod display {
    use std::fmt::{Display, Formatter, Result};

    use super::{items::*, Expr};

    impl Display for Expr {
        fn fmt(&self, f: &mut Formatter<'_>) -> Result {
            write!(f, "{}", self.content)
        }
    }

    impl Display for Log {
        fn fmt(&self, f: &mut Formatter<'_>) -> Result {
            use Log::*;
            match self {
                Single(i) => write!(f, "{}", i),
                And(l, r) => write!(f, "{} && {}", l, r),
                Or(l, r) => write!(f, "{} || {}", l, r),
            }
        }
    }

    impl Display for Equ {
        fn fmt(&self, f: &mut Formatter<'_>) -> Result {
            use Equ::*;
            match self {
                Single(i) => write!(f, "{}", i),
                Equal(l, r) => write!(f, "{} == {}", l, r),
                NotEqual(l, r) => write!(f, "{} != {}", l, r),
            }
        }
    }

    impl Display for Rel {
        fn fmt(&self, f: &mut Formatter<'_>) -> Result {
            use Rel::*;
            match self {
                Single(i) => write!(f, "{}", i),
                LessEqual(l, r) => write!(f, "{} <= {}", l, r),
                GreaterEqual(l, r) => write!(f, "{} >= {}", l, r),
                LessThan(l, r) => write!(f, "{} < {}", l, r),
                GreaterThan(l, r) => write!(f, "{} > {}", l, r),
            }
        }
    }

    impl Display for AddSub {
        fn fmt(&self, f: &mut Formatter<'_>) -> Result {
            use AddSub::*;
            match self {
                Single(i) => write!(f, "{}", i),
                Add(l, r) => write!(f, "{} + {}", l, r),
                Sub(l, r) => write!(f, "{} - {}", l, r),
            }
        }
    }

    impl Display for MulDiv {
        fn fmt(&self, f: &mut Formatter<'_>) -> Result {
            use MulDiv::*;
            match self {
                Single(i) => write!(f, "{}", i),
                Mul(l, r) => write!(f, "{} * {}", l, r),
                Div(l, r) => write!(f, "{} / {}", l, r),
                Mod(l, r) => write!(f, "{} % {}", l, r),
            }
        }
    }

    impl Display for Node {
        fn fmt(&self, f: &mut Formatter<'_>) -> Result {
            use Node::*;
            match self {
                Single(i) => write!(f, "{}", i),
                Plus(i, _) => write!(f, "+{}", i),
                Minus(i, _) => write!(f, "-{}", i),
            }
        }
    }

    impl Display for Value {
        fn fmt(&self, f: &mut Formatter<'_>) -> Result {
            use Value::*;
            match self {
                Single(i) => write!(f, "{}", i),
                ArrElem(l, r, _) => write!(f, "{}{}", l, r),
            }
        }
    }

    impl Display for Core {
        fn fmt(&self, f: &mut Formatter<'_>) -> Result {
            use Core::*;
            match self {
                Str(i, _) => write!(f, "{}", i),
                Num(i, _) => write!(f, "{}", i),
                Ident(i, _) => write!(f, "{}", i),
                True(_) => write!(f, "true"),
                False(_) => write!(f, "false"),
                Paren(i, _) => write!(f, "{}", i),
                Arr(v, _) => {
                    write!(f, "[")?;
                    if !v.is_empty() {
                        write!(f, "{}", v[0])?;
                    }
                    for e in &v[1..] {
                        write!(f, " {}", e)?;
                    }
                    write!(f, "]")?;
                    Ok(())
                }
            }
        }
    }
}
