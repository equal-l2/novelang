use super::from_tokens::*;

#[derive(Debug, Clone)]
pub struct Return(pub Option<Expr>);

impl FromTokens for Return {
    fn try_parse<'a, T>(tks: &mut std::iter::Peekable<T>, last: usize) -> Result<Self>
    where
        Self: Sized,
        T: Iterator<Item = (usize, &'a Token)>,
    {
        let (_, tk) = tks
            .peek()
            .ok_or_else(|| (Error("Arg abruptly ended".into()), last.into()))?;

        let ret = if tk.item == LangItem::Key(Keyword::With) {
            // with a return value
            let _ = tks.next();
            Some(parse_expr(tks, last)?)
        } else {
            None
        };

        expects_semi!(tks, last);

        Ok(Self(ret))
    }
}
