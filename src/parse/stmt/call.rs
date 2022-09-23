use super::from_tokens::*;

#[derive(Debug, Clone, derive_more::Into)]
pub struct Args(pub Vec<Expr>);

#[derive(Debug, Clone, derive_more::Into)]
pub struct Res(pub Target);

#[derive(Debug, Clone)]
pub struct Call {
    pub callee: Expr,
    pub args: Option<Args>,
    pub res: Option<Res>,
}

type ArgsResPair = (Option<Args>, Option<Res>);

impl FromTokens for Call {
    fn try_parse<'a, T>(tks: &mut std::iter::Peekable<T>, last: usize) -> Result<Self>
    where
        T: Iterator<Item = (usize, &'a Token)>,
    {
        let callee = utils::parse_expr(tks, last)?;

        let (args, res) = ArgsResPair::try_parse(tks, last)?;

        expects_semi!(tks, last);

        Ok(Self { callee, args, res })
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
            .ok_or_else(|| (Error("Unexpected end of Call".into()), last.into()))?;

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

        let res = if LangItem::Key(Keyword::Results) == tk.item {
            let _ = tks.next();
            expects!("expected In", LangItem::Key(Keyword::In), tks, last);
            Some(Res(super::super::target::parse_target(tks, last)?))
        } else {
            eprintln!("{}", tk);
            None
        };

        Ok((args, res))
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
                    let an_arg = utils::parse_expr(tks, last)?;
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
