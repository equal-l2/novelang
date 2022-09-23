use super::super::Type;
use crate::parse::stmt::sub::{
    Arg as ParsedArg, Args as ParsedArgs, Res as ParsedRes, Sub as ParsedSub,
};
use crate::types::IdentName;

#[derive(Clone, Debug)]
pub struct Arg {
    pub ident: IdentName,
    pub ty: Type,
}

#[derive(Clone, Debug)]
pub struct Sub {
    pub name: IdentName,
    pub args: Option<Vec<Arg>>,
    pub res_type: Option<Type>,
}

impl From<ParsedSub> for Sub {
    fn from(sub: ParsedSub) -> Self {
        Self {
            name: sub.name.into(),
            args: sub
                .args
                .map(|ParsedArgs(v)| v.into_iter().map(Into::into).collect()),
            res_type: sub.res.map(|ParsedRes(t)| t.into()),
        }
    }
}

impl From<ParsedArg> for Arg {
    fn from(arg: ParsedArg) -> Self {
        Self {
            ident: arg.ident.into(),
            ty: arg.ty.into(),
        }
    }
}
