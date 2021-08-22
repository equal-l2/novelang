#[derive(Debug, Clone)]
pub struct Expr {
    pub content: items::TopItem,
}

impl From<String> for Expr {
    fn from(s: String) -> Self {
        use items::*;
        Self {
            content: Log::Single(Equ::Single(Rel::Single(AddSub::Single(MulDiv::Single(
                Node::Single(Value::Single(Core::Str(s))),
            ))))),
        }
    }
}

pub mod items {
    pub type TopItem = Log;
    pub type TopNum = AddSub;

    #[derive(Debug, Clone)]
    pub enum Log {
        Single(Equ),
        And(Box<Self>, Equ),
        Or(Box<Self>, Equ),
    }

    #[derive(Debug, Clone)]
    pub enum Equ {
        Single(Rel),
        Equal(Box<Self>, Rel),
        NotEqual(Box<Self>, Rel),
    }

    #[derive(Debug, Clone)]
    pub enum Rel {
        Single(AddSub),
        LessEqual(AddSub, AddSub),
        GreaterEqual(AddSub, AddSub),
        LessThan(AddSub, AddSub),
        GreaterThan(AddSub, AddSub),
    }

    #[derive(Debug, Clone)]
    pub enum AddSub {
        Single(MulDiv),
        Add(Box<Self>, MulDiv),
        Sub(Box<Self>, MulDiv),
    }

    #[derive(Debug, Clone)]
    pub enum MulDiv {
        Single(Node),
        Mul(Box<Self>, Node),
        Div(Box<Self>, Node),
        Mod(Box<Self>, Node),
    }

    #[derive(Debug, Clone)]
    pub enum Node {
        Single(Value),
        Plus(Box<Self>),
        Minus(Box<Self>),
    }

    #[derive(Debug, Clone)]
    pub enum Value {
        Single(Core),
        ArrElem(Box<Self>, Box<TopNum>),
    }

    #[derive(Debug, Clone)]
    pub enum Core {
        Str(String),
        Num(crate::types::IntType),
        Ident(String),
        True,
        False,
        Paren(Box<TopItem>),
        Arr(Vec<TopItem>),
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
                Plus(i) => write!(f, "+{}", i),
                Minus(i) => write!(f, "-{}", i),
            }
        }
    }

    impl Display for Value {
        fn fmt(&self, f: &mut Formatter<'_>) -> Result {
            use Value::*;
            match self {
                Single(i) => write!(f, "{}", i),
                ArrElem(l, r) => write!(f, "{}{}", l, r),
            }
        }
    }

    impl Display for Core {
        fn fmt(&self, f: &mut Formatter<'_>) -> Result {
            use Core::*;
            match self {
                Str(i) => write!(f, "{}", i),
                Num(i) => write!(f, "{}", i),
                Ident(i) => write!(f, "{}", i),
                True => write!(f, "true"),
                False => write!(f, "false"),
                Paren(i) => write!(f, "{}", i),
                Arr(v) => {
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
