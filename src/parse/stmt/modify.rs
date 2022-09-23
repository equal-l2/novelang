use super::from_tokens::*;

#[derive(Debug, Clone)]
pub struct Modify {
    pub target: Target,
    pub expr: Expr,
}

impl FromTokens for Modify {
    fn try_parse<'a, T>(tks: &mut std::iter::Peekable<T>, last: usize) -> Result<Self>
    where
        Self: Sized,
        T: Iterator<Item = (usize, &'a Token)>,
    {
        let target = Target::try_parse(tks, last)?;
        expects!("To expected", LangItem::Key(Keyword::To), tks, last);

        let expr = parse_expr(tks, last)?;
        expects_semi!(tks, last);

        Ok(Self { target, expr })
    }
}
