use crate::lex::{Keyword, LangItem, Token};
use crate::parse::{utils, Error, Expr, Result, Target};

#[derive(Debug, Clone)]
pub struct Call {
    pub callee: Expr,
    pub args: Option<Vec<Expr>>,
    pub res: Option<Target>,
}

impl Call {
    pub fn try_parse<'a, T>(tks: &mut std::iter::Peekable<T>, last: usize) -> Result<Self>
    where
        T: Iterator<Item = (usize, &'a Token)>,
    {
        let callee = utils::parse_expr(tks, last)?;

        let (args, res) = try_parse_args_and_res(tks, last)?;

        expects_semi!(tks, last);

        Ok(Self { callee, args, res })
    }
}

fn try_parse_args_and_res<'a, T>(
    tks: &mut std::iter::Peekable<T>,
    last: usize,
) -> Result<(Option<Vec<Expr>>, Option<Target>)>
where
    T: Iterator<Item = (usize, &'a Token)>,
{
    let (_, tk) = tks
        .peek()
        .ok_or_else(|| (Error("Unexpected end of Call".into()), last.into()))?;

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

    let res = if LangItem::Key(Keyword::Results) == tk.item {
        let _ = tks.next();
        expects!("expected In", LangItem::Key(Keyword::In), tks, last);
        Some(super::super::target::parse_target(tks, last)?)
    } else {
        eprintln!("{}", tk);
        None
    };

    Ok((args, res))
}

fn try_parse_args<'a, T>(tks: &mut std::iter::Peekable<T>, last: usize) -> Result<Vec<Expr>>
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

    Ok(ret)
}
