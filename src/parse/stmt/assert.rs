use super::from_tokens::*;

#[derive(Debug, Clone)]
pub struct Assert {
    pub mesg: Expr,
    pub cond: Expr,
}

impl FromTokens for Assert {
    fn try_parse<'a, T>(tks: &mut std::iter::Peekable<T>, last: usize) -> Result<Self>
    where
        Self: Sized,
        T: Iterator<Item = (usize, &'a Token)>,
    {
        let expr1 = parse_expr(tks, last)?;

        let (_, tk) = tks
            .peek()
            .ok_or_else(|| (Error("expected With or Semicolon".into()), last.into()))?;

        let (mesg, cond) = if tk.item == LangItem::Key(Keyword::With) {
            // with string
            let _ = tks.next().unwrap();
            let expr2 = parse_expr(tks, last)?;
            (expr1, expr2)
        } else {
            // without string
            (Expr::from(expr1.to_string()), expr1)
        };
        expects_semi!(tks, last);

        Ok(Self { mesg, cond })
    }
}
