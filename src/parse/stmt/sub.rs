use super::super::{Error, Ident, Result};
use crate::lex::{Keyword, LangItem, Token, TypeName};
use crate::span::{Span, Spannable};

#[derive(Clone, Debug)]
pub struct Arg {
    pub ident: Ident,
    pub ty: Type,
}

#[derive(Clone, Debug)]
pub struct Sub {
    pub name: Ident,
    pub args: Option<Vec<Arg>>,
    pub res: Option<Type>,
}

impl Sub {
    pub fn try_parse<'a, T>(tks: &mut std::iter::Peekable<T>, last: usize) -> Result<Self>
    where
        T: Iterator<Item = (usize, &'a Token)>,
    {
        let (i, tk) = tks
            .next()
            .ok_or_else(|| (Error("expected subrountine name".into()), last.into()))?;
        let name = if let LangItem::Ident(name) = &tk.item {
            name
        } else {
            return Err((Error("Expected subroutine name".into()), i.into()));
        };

        let (args, ret_type) = try_parse_args_and_result(tks, last)?;

        expects_semi!(tks, last);

        Ok(Sub {
            name: Ident(name.clone(), i.into()),
            args,
            res: ret_type,
        })
    }
}

fn try_parse_args_and_result<'a, T>(
    tks: &mut std::iter::Peekable<T>,
    last: usize,
) -> Result<(Option<Vec<Arg>>, Option<Type>)>
where
    T: Iterator<Item = (usize, &'a Token)>,
{
    let (_, tk) = tks
        .peek()
        .ok_or_else(|| (Error("Unexpected end of Sub".into()), last.into()))?;

    // parse arguments
    let args = if LangItem::Key(Keyword::With) == tk.item {
        // `With` will be handled in try_parse_args
        Some(try_parse_args(tks, last)?)
    } else {
        None
    };

    let (_, tk) = tks
        .peek()
        .ok_or_else(|| (Error("Unexpected end of Sub".into()), last.into()))?;

    let ret_type = if LangItem::Key(Keyword::Results) == tk.item {
        let _ = tks.next();
        expects!("expected In", LangItem::Key(Keyword::In), tks, last);
        Some(Type::try_parse(tks, last)?)
    } else {
        eprintln!("{}", tk);
        None
    };

    Ok((args, ret_type))
}

fn try_parse_args<'a, T>(tks: &mut std::iter::Peekable<T>, last: usize) -> Result<Vec<Arg>>
where
    T: Iterator<Item = (usize, &'a Token)>,
{
    let mut ret = vec![];
    loop {
        let (i, tk) = tks
            .peek()
            .ok_or_else(|| (Error("Arg abruptly ended".into()), last.into()))?;

        match tk.item {
            LangItem::Key(Keyword::With) | LangItem::Comma => {
                // start of an arg
                let _ = tks.next();
                let an_arg = try_parse_an_arg(tks, last)?;
                ret.push(an_arg);
            }
            LangItem::Key(Keyword::Results) | LangItem::Semi => {
                // the end of args
                break;
            }
            _ => {
                return Err((
                    Error("Failed to parse arg because of this token".into()),
                    (*i).into(),
                ))
            }
        }
    }

    Ok(ret)
}

fn try_parse_an_arg<'a, T>(tks: &mut std::iter::Peekable<T>, last: usize) -> Result<Arg>
where
    T: Iterator<Item = (usize, &'a Token)>,
{
    let err_exhaust = || (Error("Arg abruptly ended".into()), last.into());

    let (i, tk) = tks.next().ok_or_else(err_exhaust)?;

    let ident = if let LangItem::Ident(name) = &tk.item {
        Ok(Ident(name.clone(), i.into()))
    } else {
        Err((Error("expected Ident".into()), i.into()))
    }?;

    let (i, tk) = tks.next().ok_or_else(err_exhaust)?;

    if !matches!(tk.item, LangItem::Key(Keyword::In)) {
        return Err((Error("expected In".into()), i.into()));
    }

    let ty = Type::try_parse(tks, last)?;

    Ok(Arg { ident, ty })
}

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
}

impl Type {
    fn try_parse<'a, T>(tks: &mut std::iter::Peekable<T>, last: usize) -> Result<Self>
    where
        T: Iterator<Item = (usize, &'a Token)>,
        Self: Sized,
    {
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

    pub fn ty(&self) -> &Ty {
        &self.ty
    }
}
