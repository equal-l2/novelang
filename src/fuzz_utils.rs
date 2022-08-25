use crate::lex;

use lex::items::*;

#[derive(Debug)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub enum FuzzItem {
    Key(Keyword),
    Cmd(Command),
    Op(Ops),
    Num(crate::types::IntType),
    Ident(crate::types::IdentName),
    Str(String),
    Semi,
    Comma,
    LPar,
    RPar,
    LBra,
    RBra,
}

fn into_lexed(input: Vec<FuzzItem>) -> crate::lex::Lexed {
    let mut lines = vec![];
    let mut tokens = vec![];

    for (idx, entry) in input.into_iter().enumerate() {
        let mapped = match entry {
            FuzzItem::Key(i) => LangItem::Key(i),
            FuzzItem::Cmd(i) => LangItem::Cmd(i),
            FuzzItem::Op(i) => LangItem::Op(i),
            FuzzItem::Num(i) => LangItem::Num(i, i.to_string().len()),

            FuzzItem::Ident(i) => LangItem::Ident(i),
            FuzzItem::Str(i) => LangItem::Str(i),
            FuzzItem::Semi => LangItem::Semi,
            FuzzItem::Comma => LangItem::Comma,
            FuzzItem::LPar => LangItem::LPar,
            FuzzItem::RPar => LangItem::RPar,
            FuzzItem::LBra => LangItem::LBra,
            FuzzItem::RBra => LangItem::RBra,
        };

        lines.push(mapped.to_string());
        tokens.push(lex::Token {
            item: mapped,
            loc: super::Location {
                row: idx + 1,
                col: 1,
            },
        });
    }

    lex::Lexed { tokens }
}

pub fn fuzz_entry_point(input: Vec<FuzzItem>) {
    let lexed = into_lexed(input);

    let parsed = match crate::parse::parse(&lexed) {
        Ok(i) => i,
        _ => return,
    };

    let block_checked = match crate::block::check_block(parsed) {
        Ok(i) => i,
        _ => return,
    };

    let _ = crate::semck::check_semantics(block_checked);
}
