use super::super::Type;
use crate::parse::stmt::sub::{Sub as ParsedSub, SubArg, SubArgs, SubName, SubRes};
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
        let SubName(ident) = sub.name;
        Self {
            name: ident.into(),
            args: sub
                .args
                .map(|SubArgs(v)| v.into_iter().map(Into::into).collect()),
            res_type: sub.res.map(|SubRes(t)| t.into()),
        }
    }
}

impl From<SubArg> for Arg {
    fn from(arg: SubArg) -> Self {
        Self {
            ident: arg.ident.into(),
            ty: arg.ty.into(),
        }
    }
}
