use crate::lex::{LangItem, Token, TypeName};
use crate::span::{Span, Spannable};

pub struct TypeError {
    kind: ErrorKind,
    span: Span,
}

enum ErrorKind {
    UnexpectedToken,
    TokenExhausted,
}

impl From<TypeError> for (super::Error, Span) {
    fn from(err: TypeError) -> Self {
        (
            super::Error(
                match err.kind {
                    ErrorKind::UnexpectedToken => "Failed to parse type because of this token",
                    ErrorKind::TokenExhausted => "Type abruptly ended",
                }
                .into(),
            ),
            err.span,
        )
    }
}

type Result<T> = std::result::Result<T, TypeError>;

#[derive(Clone, Debug)]
pub struct Type {
    ty: Ty,
    span: Span,
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
    Nothing,
}

impl Type {
    pub fn nothing() -> Self {
        Self {
            ty: Ty::Nothing,
            span: Span::default(),
        }
    }

    pub fn try_parse<'a, T>(tks: &mut std::iter::Peekable<T>, last: usize) -> Result<Self>
    where
        T: Iterator<Item = (usize, &'a Token)>,
        Self: Sized,
    {
        let (i, tk) = tks.next().ok_or_else(|| TypeError {
            kind: ErrorKind::TokenExhausted,
            span: last.into(),
        })?;
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
            _ => {
                return Err(TypeError {
                    kind: ErrorKind::UnexpectedToken,
                    span: i.into(),
                })
            }
        })
    }

    pub fn ty(&self) -> &Ty {
        &self.ty
    }
}
