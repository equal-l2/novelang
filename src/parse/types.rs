use super::from_tokens::*;
use crate::span::{Span, Spannable};

#[derive(Clone, Debug)]
pub struct Type {
    ty: Ty,
    span: Span,
}

impl Type {
    pub fn ty(&self) -> &Ty {
        &self.ty
    }
}

impl Spannable for Type {
    fn span(&self) -> Span {
        self.span.clone()
    }
}

#[derive(Clone, Debug)]
pub enum Ty {
    // TODO: add array and custom types
    Bool,
    Num,
    Str,
}

impl FromTokens for Type {
    fn try_parse<'a, T>(tks: &mut std::iter::Peekable<T>, last: usize) -> Result<Self>
    where
        Self: Sized,
        T: Iterator<Item = (usize, &'a Token)>,
    {
        use crate::lex::items::TypeName;

        let (i, tk) = tks
            .next()
            .ok_or_else(|| (Error("Type abruptly ended".into()), last.into()))?;
        Ok(match tk.item {
            LangItem::Type(TypeName::Bool) => Self {
                ty: Ty::Bool,
                span: i.into(),
            },
            LangItem::Type(TypeName::Num) => Self {
                ty: Ty::Num,
                span: i.into(),
            },
            LangItem::Type(TypeName::Str) => Self {
                ty: Ty::Str,
                span: i.into(),
            },
            _ => return Err((Error("expected type".into()), i.into())),
        })
    }
}
