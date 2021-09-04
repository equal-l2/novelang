pub use crate::exprs::Expr;

use crate::exprs::items::*;
use crate::types::Typed;

pub type Result = std::result::Result<Typed, EvalError>;

pub trait VarsMap {
    fn get(&self, name: &str) -> Typed;
    fn get_arr_elem<L: Eval, R: Eval>(&self, l: &L, r: &R) -> Result;
}

#[derive(Debug)]
pub enum EvalError {
    OverFlow,
    ZeroDivision,
    IndexOutOfBounds(super::IntType),
    UnexpectedValue(super::IntType),
}

impl std::fmt::Display for EvalError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::OverFlow => write!(f, "overflow"),
            Self::ZeroDivision => write!(f, "zero division"),
            Self::IndexOutOfBounds(n) => write!(f, "index out of bounds: {}", n),
            Self::UnexpectedValue(n) => write!(f, "unexpected value: {}", n),
        }
    }
}

pub trait Eval {
    fn eval<T: VarsMap>(&self, vmap: &T) -> Result;
}

impl Eval for Expr {
    fn eval<T: VarsMap>(&self, vmap: &T) -> Result {
        self.content.eval(vmap)
    }
}

impl Eval for Log {
    fn eval<T: VarsMap>(&self, vmap: &T) -> Result {
        Ok(match self {
            Self::Single(l) => l.eval(vmap)?,
            Self::And(l, r) => {
                let l = l.eval(vmap)?;
                let r = r.eval(vmap)?;
                match (&l, &r) {
                    (Typed::Bool(l), Typed::Bool(r)) => Typed::Bool(*l && *r),
                    _ => panic!(
                        "cannot eval composite logic between {} with {}",
                        l.typename(),
                        r.typename()
                    ),
                }
            }
            Self::Or(l, r) => {
                let l = l.eval(vmap)?;
                let r = r.eval(vmap)?;
                match (&l, &r) {
                    (Typed::Bool(l), Typed::Bool(r)) => Typed::Bool(*l || *r),
                    _ => panic!(
                        "cannot eval composite logic between {} with {}",
                        l.typename(),
                        r.typename()
                    ),
                }
            }
        })
    }
}

impl Eval for Equ {
    fn eval<T: VarsMap>(&self, vmap: &T) -> Result {
        Ok(match self {
            Self::Single(l) => l.eval(vmap)?,
            Self::Equal(l, r) => {
                let l = l.eval(vmap)?;
                let r = r.eval(vmap)?;
                match (&l, &r) {
                    (Typed::Bool(l), Typed::Bool(r)) => Typed::Bool(l == r),
                    (Typed::Num(l), Typed::Num(r)) => Typed::Bool(l == r),
                    (Typed::Str(l), Typed::Str(r)) => Typed::Bool(l == r),
                    (Typed::Arr(l), Typed::Arr(r)) => Typed::Bool(l == r),
                    _ => panic!("cannot compare {} with {}", l.typename(), r.typename()),
                }
            }
            Self::NotEqual(l, r) => {
                let l = l.eval(vmap)?;
                let r = r.eval(vmap)?;
                match (&l, &r) {
                    (Typed::Bool(l), Typed::Bool(r)) => Typed::Bool(l != r),
                    (Typed::Num(l), Typed::Num(r)) => Typed::Bool(l != r),
                    (Typed::Str(l), Typed::Str(r)) => Typed::Bool(l != r),
                    (Typed::Arr(l), Typed::Arr(r)) => Typed::Bool(l != r),
                    _ => panic!("cannot compare {} with {}", l.typename(), r.typename()),
                }
            }
        })
    }
}

macro_rules! def_cmp {
    ($vmap: expr, $l: expr, $r: expr, $( $pat:pat )|+) => {
        {
            let l = $l.eval($vmap)?;
            let r = $r.eval($vmap)?;
            let ord = l.partial_cmp(&r).expect("Compare never fails as it is already checked");
            Ok(Typed::Bool(matches!(ord, $($pat)|+)))
        }
    };
}

impl Eval for Rel {
    fn eval<T: VarsMap>(&self, vmap: &T) -> Result {
        use std::cmp::Ordering;
        Ok(match self {
            Self::Single(l) => l.eval(vmap)?,
            Self::LessEqual(l, r) => def_cmp!(vmap, l, r, Ordering::Less | Ordering::Equal)?,
            Self::GreaterEqual(l, r) => def_cmp!(vmap, l, r, Ordering::Greater | Ordering::Equal)?,
            Self::LessThan(l, r) => def_cmp!(vmap, l, r, Ordering::Less)?,
            Self::GreaterThan(l, r) => def_cmp!(vmap, l, r, Ordering::Greater)?,
        })
    }
}

macro_rules! def_ari {
    ($vmap: expr, $l: expr, $r: expr, $method: ident, $err: path, $op: literal) => {{
        let l = $l.eval($vmap)?;
        let r = $r.eval($vmap)?;
        match (&l, &r) {
            (Typed::Num(this), Typed::Num(that)) => match this.$method(*that) {
                Some(n) => Ok(Typed::Num(n)),
                None => Err($err),
            },
            _ => panic!(
                "cannot perform {} between {} and {}",
                $op,
                l.typename(),
                r.typename()
            ),
        }
    }};
}

impl Eval for AddSub {
    fn eval<T: VarsMap>(&self, vmap: &T) -> Result {
        Ok(match self {
            Self::Single(l) => l.eval(vmap)?,
            Self::Add(l, r) => {
                let l = l.eval(vmap)?;
                let r = r.eval(vmap)?;
                match (&l, &r) {
                    (Typed::Num(this), Typed::Num(that)) => match this.checked_add(*that) {
                        Some(n) => Ok(Typed::Num(n)),
                        None => Err(EvalError::OverFlow),
                    },
                    (Typed::Str(this), Typed::Str(that)) => Ok(Typed::Str(this.clone() + that)),
                    _ => panic!(
                        "cannot perform addition between {} and {}",
                        l.typename(),
                        r.typename()
                    ),
                }
            }?,
            Self::Sub(l, r) => {
                def_ari!(vmap, l, r, checked_sub, EvalError::OverFlow, "subtraction")?
            }
        })
    }
}

impl Eval for MulDiv {
    fn eval<T: VarsMap>(&self, vmap: &T) -> Result {
        Ok(match self {
            Self::Single(l) => l.eval(vmap)?,
            Self::Mul(l, r) => {
                let l = l.eval(vmap)?;
                let r = r.eval(vmap)?;
                match (&l, &r) {
                    (Typed::Num(this), Typed::Num(that)) => match this.checked_mul(*that) {
                        Some(n) => Ok(Typed::Num(n)),
                        None => Err(EvalError::OverFlow),
                    },
                    (Typed::Num(n), t) | (t, Typed::Num(n)) => {
                        let n = *n;
                        if n == 0 {
                            Err(EvalError::UnexpectedValue(n))
                        } else {
                            let (n, t) = {
                                let t = t.clone();
                                if n < 0 {
                                    (-n, -t)
                                } else {
                                    (n, t)
                                }
                            };
                            Ok(match t {
                                Typed::Str(s) => Typed::Str(s.repeat(n as usize)),
                                Typed::Arr(v) => {
                                    let ret = std::iter::repeat(v)
                                        .take(n as usize)
                                        .reduce(|mut v1, v2| {
                                            v1.extend(v2);
                                            v1
                                        })
                                        .expect("doesn't it work?");
                                    Typed::Arr(ret)
                                }
                                _ => unreachable!(),
                            })
                        }
                    }
                    _ => panic!(
                        "cannot perform {} between {} and {}",
                        "multiplication",
                        l.typename(),
                        r.typename()
                    ),
                }
            }?,
            Self::Div(l, r) => {
                def_ari!(vmap, l, r, checked_div, EvalError::ZeroDivision, "division")?
            }
            Self::Mod(l, r) => def_ari!(vmap, l, r, checked_rem, EvalError::ZeroDivision, "mod")?,
        })
    }
}

impl Eval for Node {
    fn eval<T: VarsMap>(&self, vmap: &T) -> Result {
        Ok(match self {
            Self::Single(l) => l.eval(vmap)?,
            Self::Plus(l, _) => l.eval(vmap)?,
            Self::Minus(l, _) => -l.eval(vmap)?,
        })
    }
}

impl Eval for Value {
    fn eval<T: VarsMap>(&self, vmap: &T) -> Result {
        Ok(match self {
            Self::Single(l) => l.eval(vmap)?,
            Self::ArrElem(l, r, _) => vmap.get_arr_elem(&(**l), &(**r))?,
        })
    }
}

impl Eval for Core {
    fn eval<T: VarsMap>(&self, vmap: &T) -> Result {
        Ok(match self {
            Self::Str(s, _) => Typed::Str(s.clone()),
            Self::Num(n, _) => Typed::Num(*n),
            Self::Ident(name, _) => vmap.get(name),
            Self::True(_) => Typed::Bool(true),
            Self::False(_) => Typed::Bool(false),
            Self::Paren(expr, _) => expr.eval(vmap)?,
            Self::Arr(i, _) => {
                let v = i
                    .iter()
                    .map(|e| e.eval(vmap))
                    .collect::<std::result::Result<_, _>>()?;
                Typed::Arr(v)
            }
        })
    }
}
