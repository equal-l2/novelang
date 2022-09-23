use super::super::types::Type;
use super::from_tokens::*;

#[derive(Clone, Debug)]
pub struct Arg {
    pub ident: Ident,
    pub ty: Type,
}

#[derive(Debug, Clone)]
pub struct Args(pub Vec<Arg>);

#[derive(Debug, Clone)]
pub struct Res(pub Type);

#[derive(Clone, Debug)]
pub struct Sub {
    pub name: Ident,
    pub args: Option<Args>,
    pub res: Option<Res>,
}

type ArgsResPair = (Option<Args>, Option<Res>);

impl FromTokens for Sub {
    fn try_parse<'a, T>(tks: &mut std::iter::Peekable<T>, last: usize) -> Result<Self>
    where
        Self: Sized,
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

        let (args, ret_type) = ArgsResPair::try_parse(tks, last)?;

        expects_semi!(tks, last);

        Ok(Sub {
            name: Ident(name.clone(), i.into()),
            args,
            res: ret_type,
        })
    }
}

impl FromTokens for ArgsResPair {
    fn try_parse<'a, T>(tks: &mut std::iter::Peekable<T>, last: usize) -> Result<Self>
    where
        Self: Sized,
        T: Iterator<Item = (usize, &'a Token)>,
    {
        let (_, tk) = tks
            .peek()
            .ok_or_else(|| (Error("Unexpected end of Sub".into()), last.into()))?;

        // parse arguments
        let args = if LangItem::Key(Keyword::With) == tk.item {
            // `With` will be handled in try_parse_args
            Some(Args::try_parse(tks, last)?)
        } else {
            None
        };

        let (_, tk) = tks
            .peek()
            .ok_or_else(|| (Error("Unexpected end of Sub".into()), last.into()))?;

        let ret_type = if LangItem::Key(Keyword::Results) == tk.item {
            let _ = tks.next();
            expects!("expected In", LangItem::Key(Keyword::In), tks, last);
            Some(Res(Type::try_parse(tks, last)?))
        } else {
            eprintln!("{}", tk);
            None
        };

        Ok((args, ret_type))
    }
}

impl FromTokens for Args {
    fn try_parse<'a, T>(tks: &mut std::iter::Peekable<T>, last: usize) -> Result<Self>
    where
        Self: Sized,
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
                    let an_arg = Arg::try_parse(tks, last)?;
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

        Ok(Self(ret))
    }
}

impl FromTokens for Arg {
    fn try_parse<'a, T>(tks: &mut std::iter::Peekable<T>, last: usize) -> Result<Self>
    where
        Self: Sized,
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
}
