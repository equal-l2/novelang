use super::prelude::*;

#[derive(Debug, Clone)]
pub struct Roll {
    pub count: Expr,
    pub faces: Expr,
    pub target: Target,
}

impl TryFromTokens for Roll {
    fn try_parse<'a, T>(tks: &mut std::iter::Peekable<T>, last: usize) -> Result<Self>
    where
        Self: Sized,
        T: Iterator<Item = (usize, &'a Token)>,
    {
        let count = parse_expr(tks, last)?;

        expects!("\"Dice\" expected", LangItem::Key(Keyword::Dice), tks, last);
        expects!("\"With\" expected", LangItem::Key(Keyword::With), tks, last);

        let faces = parse_expr(tks, last)?;

        expects!(
            "\"Faces\" expected",
            LangItem::Key(Keyword::Faces),
            tks,
            last
        );
        expects!("\"To\" expected", LangItem::Key(Keyword::To), tks, last);

        let target = Target::try_parse(tks, last)?;

        expects_semi!(tks, last);

        Ok(Self {
            count,
            faces,
            target,
        })
    }
}
