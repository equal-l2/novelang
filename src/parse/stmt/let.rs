use super::from_tokens::*;

#[derive(Debug, Clone)]
pub struct Let {
    pub name: Ident,
    pub init: Expr,
    pub is_mut: bool,
}

impl FromTokens for Let {
    fn try_parse<'a, T>(tks: &mut std::iter::Peekable<T>, last: usize) -> Result<Self>
    where
        Self: Sized,
        T: Iterator<Item = (usize, &'a Token)>,
    {
        use crate::span::Spannable;
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
            expects!("\"Be\" expected", LangItem::Key(Keyword::Be), tks, last);

            let init = parse_expr(tks, last)?;

            let i = init.span().1;

            match tks.next() {
                Some((_, tk)) => match tk.item {
                    LangItem::Key(Keyword::AsMut) => {
                        expects_semi!(tks, last);
                        Ok(Let {
                            name: Ident(name.clone(), i.into()),
                            init,
                            is_mut: true,
                        })
                    }
                    LangItem::Semi => Ok(Let {
                        name: Ident(name.clone(), i.into()),
                        init,
                        is_mut: false,
                    }),
                    _ => return Err((Error("expected Semicolon or AsMut".into()), (i + 1).into())),
                },
                None => {
                    return Err((Error("expected Semicolon or AsMut".into()), (i + 1).into()));
                }
            }
        } else {
            return Err((Error("Ident expected".into()), i.into()));
        }
    }
}
