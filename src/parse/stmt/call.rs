use super::from_tokens::*;
use crate::span::Spannable;

#[derive(Debug, Clone)]
pub struct CallArgs(pub Vec<Expr>);

#[derive(Debug, Clone)]
pub struct CallRes(pub Target);

#[derive(Debug, Clone)]
pub struct Callee(pub Expr);

impl Spannable for Callee {
    fn span(&self) -> crate::span::Span {
        self.0.span()
    }
}

#[derive(Debug, Clone)]
pub struct Call {
    pub callee: Callee,
    pub args: Option<CallArgs>,
    pub res: Option<CallRes>,
}

type ArgsResPair = (Option<CallArgs>, Option<CallRes>);

impl FromTokens for Call {
    fn try_parse<'a, T>(tks: &mut std::iter::Peekable<T>, last: usize) -> Result<Self>
    where
        T: Iterator<Item = (usize, &'a Token)>,
    {
        let callee = Callee(parse_expr(tks, last)?);

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
            Some(CallArgs::try_parse(tks, last)?)
        } else {
            None
        };

        let (_, tk) = tks
            .peek()
            .ok_or_else(|| (Error("Unexpected end of Sub".into()), last.into()))?;

        let res = if LangItem::Key(Keyword::Results) == tk.item {
            let _ = tks.next();
            expects!("expected In", LangItem::Key(Keyword::In), tks, last);
            Some(CallRes(Target::try_parse(tks, last)?))
        } else {
            eprintln!("{}", tk);
            None
        };

        Ok((args, res))
    }
}

impl FromTokens for CallArgs {
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
                    let an_arg = parse_expr(tks, last)?;
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
