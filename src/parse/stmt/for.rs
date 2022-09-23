use super::from_tokens::*;

#[derive(Debug, Clone)]
pub struct For {
    pub counter: Ident,
    pub from: Expr,
    pub to: Expr,
}

impl FromTokens for For {
    fn try_parse<'a, T>(tks: &mut std::iter::Peekable<T>, last: usize) -> Result<Self>
    where
        Self: Sized,
        T: Iterator<Item = (usize, &'a Token)>,
    {
        let (i, tk) = tks
            .next()
            .ok_or_else(|| (Error("expected Ident".into()), last.into()))?;

        if let LangItem::Ident(name) = &tk.item {
            if name.as_ref().starts_with('_') {
                return Err((
                    Error("Identifier starts with _ is reserved".into()),
                    i.into(),
                ));
            }
            let counter = Ident(name.clone(), i.into());

            expects!("\"From\" expected", LangItem::Key(Keyword::From), tks, last);

            let from = parse_expr(tks, last)?;

            expects!("\"To\" expected", LangItem::Key(Keyword::To), tks, last);

            let to = parse_expr(tks, last)?;

            expects_semi!(tks, last);

            Ok(Self { counter, from, to })
        } else {
            Err((Error("Ident expected".into()), i.into()))
        }
    }
}
