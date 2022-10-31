use super::super::types::Type;
use super::prelude::*;

#[derive(Clone, Debug)]
pub struct SubArg {
    pub ident: Ident,
    pub ty: Type,
}

#[derive(Debug, Clone)]
pub struct SubArgs(pub Vec<SubArg>);

#[derive(Debug, Clone)]
pub struct SubRes(pub Type);

#[derive(Debug, Clone)]
pub struct SubName(pub Ident);

#[derive(Clone, Debug)]
pub struct Sub {
    pub name: SubName,
    pub args: Option<SubArgs>,
    pub res: Option<SubRes>,
}

type ArgsResPair = (Option<SubArgs>, Option<SubRes>);

impl TryFromTokens for Sub {
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
            name: SubName(Ident(name.clone(), i.into())),
            args,
            res: ret_type,
        })
    }
}

impl TryFromTokens for ArgsResPair {
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
            Some(SubArgs::try_parse(tks, last)?)
        } else {
            None
        };

        let (_, tk) = tks
            .peek()
            .ok_or_else(|| (Error("Unexpected end of Sub".into()), last.into()))?;

        let ret_type = if LangItem::Key(Keyword::Results) == tk.item {
            let _ = tks.next();
            expects!("expected In", LangItem::Key(Keyword::In), tks, last);
            Some(SubRes(Type::try_parse(tks, last)?))
        } else {
            eprintln!("{}", tk);
            None
        };

        Ok((args, ret_type))
    }
}

impl TryFromTokens for SubArgs {
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
                    let an_arg = SubArg::try_parse(tks, last)?;
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

impl TryFromTokens for SubArg {
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

        Ok(SubArg { ident, ty })
    }
}
