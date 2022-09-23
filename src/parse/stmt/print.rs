use super::super::look_item::*;
use super::from_tokens::*;

#[derive(Debug, Clone)]
pub struct Print(pub Vec<Expr>);

impl FromTokens for Print {
    fn try_parse<'a, T>(tks: &mut std::iter::Peekable<T>, last: usize) -> Result<Self>
    where
        Self: Sized,
        T: Iterator<Item = (usize, &'a Token)>,
    {
        let mut args = vec![parse_expr(tks, last)?];

        while let Some((i, tk)) = tks.peek() {
            match tk.item {
                LangItem::Semi => break,
                LangItem::Comma => {
                    let _ = tks.next().unwrap();

                    if matches!(tks.peek().item(), Some(LangItem::Semi)) {
                        break;
                    }

                    args.push(parse_expr(tks, last)?);
                }
                _ => {
                    return Err((Error("Unexpected token".into()), (*i).into()));
                }
            }
        }
        expects_semi!(tks, last);

        Ok(Self(args))
    }
}
