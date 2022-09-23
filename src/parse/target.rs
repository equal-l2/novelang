use super::from_tokens::*;
use super::utils::parse_expr;
use super::LookItem;

impl FromTokens for Target {
    fn try_parse<'a, T>(tks: &mut std::iter::Peekable<T>, last: usize) -> Result<Self>
    where
        Self: Sized,
        T: Iterator<Item = (usize, &'a Token)>,
    {
        let (i, tk) = tks
            .peek()
            .ok_or_else(|| (Error("expected Ident".into()), last.into()))?;
        if let LangItem::Ident(name) = &tk.item {
            let mut val = Target::Scalar(Ident(name.clone(), (*i).into()));
            let _ = tks.next().unwrap();
            loop {
                if tks.peek().item() == Some(LangItem::LBra) {
                    let _ = tks.next().unwrap();
                    let expr = parse_expr(tks, last)?;
                    expects!("expected RBra", LangItem::RBra, tks, last);
                    val = Target::Vector(Box::from(val), Box::from(expr));
                } else {
                    return Ok(val);
                }
            }
        } else {
            Err((Error("expected Ident".into()), (*i).into()))
        }
    }
}
