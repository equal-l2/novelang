use super::{Expr, ScopeStack, Type};
use crate::exprs::items::*;
use crate::span::{Span, Spannable};

pub struct Error {
    pub kind: ErrorKind,
    pub span: Span,
}

#[derive(Debug)]
pub enum ErrorKind {
    VariableNotFound(crate::types::IdentName),
    UnaryUndefined(&'static str, Type),
    BinaryUndefined(&'static str, Type, Type),
    NonNumIndex(Type),
    NotIndexable(Type),
    ArrayTypeDiffer(Type),
}

impl std::fmt::Display for ErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ErrorKind::VariableNotFound(name) => write!(f, "Variable {} was not found", name),
            ErrorKind::UnaryUndefined(op, ty) => {
                write!(f, "Unary operator {} is not defined for type {}", op, ty)
            }
            ErrorKind::BinaryUndefined(op, ty_l, ty_r) => write!(
                f,
                "Binary operator {} is not defined between types {} and {}",
                op, ty_l, ty_r
            ),
            ErrorKind::NonNumIndex(ty) => write!(f, "Type {} cannot be used as an index", ty),
            ErrorKind::NotIndexable(ty) => write!(f, "Type {} cannot be indexed", ty),
            ErrorKind::ArrayTypeDiffer(_) => write!(f, "PLACEHOLDER"),
        }
    }
}

type Result = std::result::Result<Type, Error>;

pub(super) trait TypeCheck {
    fn check_type(&self, stack: &ScopeStack) -> Result;
}

impl TypeCheck for Expr {
    fn check_type(&self, stack: &ScopeStack) -> Result {
        self.content.check_type(stack)
    }
}

impl TypeCheck for Log {
    fn check_type(&self, stack: &ScopeStack) -> Result {
        match self {
            Self::Single(i) => i.check_type(stack),
            Self::And(l, r) | Self::Or(l, r) => {
                let l_ty = l.check_type(stack)?;
                let r_ty = r.check_type(stack)?;

                if l_ty == r_ty && l_ty == Type::Bool {
                    Ok(Type::Bool)
                } else {
                    let op = match self {
                        Self::Single(_) => unreachable!(),
                        Self::And(_, _) => "&&",
                        Self::Or(_, _) => "||",
                    };
                    Err(Error {
                        kind: ErrorKind::BinaryUndefined(op, l_ty, r_ty),
                        span: self.span(),
                    })
                }
            }
        }
    }
}

impl TypeCheck for Equ {
    fn check_type(&self, stack: &ScopeStack) -> Result {
        match self {
            Self::Single(i) => i.check_type(stack),
            Self::Equal(l, r) | Self::NotEqual(l, r) => {
                let l_ty = l.check_type(stack)?;
                let r_ty = r.check_type(stack)?;

                if l_ty == r_ty {
                    Ok(Type::Bool)
                } else {
                    let op = match self {
                        Self::Single(_) => unreachable!(),
                        Self::Equal(_, _) => "==",
                        Self::NotEqual(_, _) => "!=",
                    };
                    Err(Error {
                        kind: ErrorKind::BinaryUndefined(op, l_ty, r_ty),
                        span: self.span(),
                    })
                }
            }
        }
    }
}

impl TypeCheck for Rel {
    fn check_type(&self, stack: &ScopeStack) -> Result {
        match self {
            Self::Single(i) => i.check_type(stack),
            Self::LessEqual(l, r)
            | Self::GreaterEqual(l, r)
            | Self::LessThan(l, r)
            | Self::GreaterThan(l, r) => {
                let l_ty = l.check_type(stack)?;
                let r_ty = r.check_type(stack)?;

                if l_ty == r_ty && !matches!(l_ty, Type::Sub | Type::Arr(_)) {
                    Ok(Type::Bool)
                } else {
                    let op = match self {
                        Self::Single(_) => unreachable!(),
                        Self::LessEqual(_, _) => "<=",
                        Self::GreaterEqual(_, _) => ">=",
                        Self::LessThan(_, _) => "<",
                        Self::GreaterThan(_, _) => ">",
                    };
                    Err(Error {
                        kind: ErrorKind::BinaryUndefined(op, l_ty, r_ty),
                        span: self.span(),
                    })
                }
            }
        }
    }
}

impl TypeCheck for AddSub {
    fn check_type(&self, stack: &ScopeStack) -> Result {
        match self {
            Self::Single(i) => i.check_type(stack),
            Self::Add(l, r) => {
                let l_ty = l.check_type(stack)?;
                let r_ty = r.check_type(stack)?;

                if l_ty == r_ty && l_ty != Type::Sub {
                    Ok(l_ty)
                } else {
                    Err(Error {
                        kind: ErrorKind::BinaryUndefined("+", l_ty, r_ty),
                        span: self.span(),
                    })
                }
            }
            Self::Sub(l, r) => {
                let l_ty = l.check_type(stack)?;
                let r_ty = r.check_type(stack)?;

                if l_ty == r_ty && l_ty == Type::Num {
                    Ok(l_ty)
                } else {
                    Err(Error {
                        kind: ErrorKind::BinaryUndefined("-", l_ty, r_ty),
                        span: self.span(),
                    })
                }
            }
        }
    }
}

impl TypeCheck for MulDiv {
    fn check_type(&self, stack: &ScopeStack) -> Result {
        match self {
            Self::Single(i) => i.check_type(stack),
            Self::Mul(l, r) => {
                let l_ty = l.check_type(stack)?;
                let r_ty = r.check_type(stack)?;

                match (&l_ty, &r_ty) {
                    (Type::Num, Type::Num) => Ok(Type::Num),
                    (Type::Num, Type::Str) | (Type::Str, Type::Num) => Ok(Type::Str),
                    (Type::Num, Type::Arr(t)) | (Type::Arr(t), Type::Num) => {
                        Ok(Type::Arr(t.clone()))
                    }
                    _ => Err(Error {
                        kind: ErrorKind::BinaryUndefined("*", l_ty, r_ty),
                        span: self.span(),
                    }),
                }
            }
            Self::Div(l, r) | Self::Mod(l, r) => {
                let l_ty = l.check_type(stack)?;
                let r_ty = r.check_type(stack)?;

                if l_ty == r_ty && l_ty == Type::Num {
                    Ok(l_ty)
                } else {
                    let op = if matches!(self, Self::Div(_, _)) {
                        "/"
                    } else {
                        "%"
                    };
                    Err(Error {
                        kind: ErrorKind::BinaryUndefined(op, l_ty, r_ty),
                        span: self.span(),
                    })
                }
            }
        }
    }
}

impl TypeCheck for Node {
    fn check_type(&self, stack: &ScopeStack) -> Result {
        match self {
            Self::Single(i) => i.check_type(stack),
            Self::Plus(i, _) | Self::Minus(i, _) => {
                let ty = i.check_type(stack)?;
                if matches!(ty, Type::Sub) {
                    let op = if matches!(self, Self::Plus(_, _)) {
                        "+"
                    } else {
                        "-"
                    };
                    Err(Error {
                        kind: ErrorKind::UnaryUndefined(op, ty),
                        span: self.span(),
                    })
                } else {
                    Ok(ty)
                }
            }
        }
    }
}

impl TypeCheck for Value {
    fn check_type(&self, stack: &ScopeStack) -> Result {
        match self {
            Self::Single(i) => i.check_type(stack),
            Self::ArrElem(l, r, _) => {
                let l_ty = l.check_type(stack)?;
                let r_ty = r.check_type(stack)?;
                if r_ty == Type::Num {
                    match l_ty {
                        Type::Str => Ok(Type::Str),
                        Type::Arr(t) => Ok(*t),
                        _ => Err(Error {
                            kind: ErrorKind::NotIndexable(l_ty),
                            span: self.span(),
                        }),
                    }
                } else {
                    Err(Error {
                        kind: ErrorKind::NonNumIndex(r_ty),
                        span: self.span(),
                    })
                }
            }
        }
    }
}

impl TypeCheck for Core {
    fn check_type(&self, stack: &ScopeStack) -> Result {
        match self {
            Self::Str(_, _) => Ok(Type::Str),
            Self::Num(_, _) => Ok(Type::Num),
            Self::Ident(name, _) => stack
                .get_type_info(&name.clone().into())
                .map(|ti| ti.ty)
                .map_err(|kind| Error {
                    kind,
                    span: self.span(),
                }),
            Self::True(_) | Self::False(_) => Ok(Type::Bool),
            Self::Paren(i, _) => i.check_type(stack),
            Self::Arr(i, _) => {
                let v = i
                    .iter()
                    .map(|e| e.check_type(stack))
                    .collect::<std::result::Result<Vec<_>, _>>()?;
                let first = &v[0];
                if v.iter().all(|e| e == first) {
                    Ok(Type::Arr(Box::from(first.clone())))
                } else {
                    Err(Error {
                        kind: ErrorKind::ArrayTypeDiffer(first.clone()),
                        span: self.span(),
                    })
                }
            }
        }
    }
}
