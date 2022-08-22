use super::{Expr, ScopeStack, Type};
use crate::exprs::items::*;

#[derive(Debug)]
pub(super) enum TypeError {
    VarNotFound(crate::IdentName),
    UnaryUndefined(Type),
    BinaryUndefined(Type, Type),
    Unexpected { expected: Type, actual: Type },
    ArrayTypeDiffer(Type),
}

type Result = std::result::Result<Type, TypeError>;

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
                    Err(TypeError::BinaryUndefined(l_ty, r_ty))
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
                    Err(TypeError::BinaryUndefined(l_ty, r_ty))
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
                    Err(TypeError::BinaryUndefined(l_ty, r_ty))
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
                    Err(TypeError::BinaryUndefined(l_ty, r_ty))
                }
            }
            Self::Sub(l, r) => {
                let l_ty = l.check_type(stack)?;
                let r_ty = r.check_type(stack)?;

                if l_ty == r_ty && l_ty == Type::Num {
                    Ok(l_ty)
                } else {
                    Err(TypeError::BinaryUndefined(l_ty, r_ty))
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
                    _ => Err(TypeError::BinaryUndefined(l_ty, r_ty)),
                }
            }
            Self::Div(l, r) | Self::Mod(l, r) => {
                let l_ty = l.check_type(stack)?;
                let r_ty = r.check_type(stack)?;

                if l_ty == r_ty && l_ty == Type::Num {
                    Ok(l_ty)
                } else {
                    Err(TypeError::BinaryUndefined(l_ty, r_ty))
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
                    Err(TypeError::UnaryUndefined(ty))
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
                        _ => Err(TypeError::BinaryUndefined(l_ty, r_ty)),
                    }
                } else {
                    Err(TypeError::BinaryUndefined(l_ty, r_ty))
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
                .ok_or_else(|| TypeError::VarNotFound(name.clone())),
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
                    Err(TypeError::ArrayTypeDiffer(first.clone()))
                }
            }
        }
    }
}
