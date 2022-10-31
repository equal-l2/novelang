use super::prelude::*;

#[derive(Debug, Clone)]
pub struct Input {
    pub prompt: Option<String>,
    pub target: Target,
}

impl TryFromTokens for Input {
    fn try_parse<'a, T>(tks: &mut std::iter::Peekable<T>, last: usize) -> Result<Self>
    where
        Self: Sized,
        T: Iterator<Item = (usize, &'a Token)>,
    {
        // TODO: accept runtime prompt string
        let (_, tk) = tks
            .peek()
            .ok_or_else(|| (Error("expected To".into()), last.into()))?;

        let prompt = if let LangItem::Str(prompt) = &tk.item {
            let _ = tks.next().unwrap();
            Some(prompt.clone())
        } else {
            None
        };

        expects!("\"To\" expected", LangItem::Key(Keyword::To), tks, last);

        let target = Target::try_parse(tks, last)?;

        expects_semi!(tks, last);

        Ok(Self { prompt, target })
    }
}
