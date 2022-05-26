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
