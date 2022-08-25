pub mod items;

mod fragments;
mod macros;
mod utils;

#[cfg(test)]
mod test;

pub use items::*;

use super::{Location, Range};

pub(crate) fn loc_to_range(loc: Location, len: usize) -> Range {
    let head = loc.clone();
    let tail = Location {
        row: loc.row,
        col: loc.col + len,
    };
    Range(head, tail)
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
    pub loc: Location,
    pub item: LangItem,
}

impl Token {
    pub fn next_col(&self) -> Location {
        let loc = &self.loc;
        Location {
            row: loc.row,
            col: loc.col + self.item.len(),
        }
    }
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{{ {:?} ({},{}) }}",
            self.item, self.loc.row, self.loc.col
        )
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Lexed {
    pub tokens: Vec<Token>,
}

#[derive(Debug, Clone)]
pub enum Error {
    UnterminatedStr,
    UnexpectedChar(char),
    TooLongNum,
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::UnterminatedStr => write!(f, "String is not terminated")?,
            Error::UnexpectedChar(c) => write!(f, "Unexpected character '{}'", c)?,
            Error::TooLongNum => write!(
                f,
                "Number is too long for {}-bit integer",
                std::mem::size_of::<crate::types::IntType>() * 8
            )?,
        };
        Ok(())
    }
}

pub fn lex<S: AsRef<str>>(lines: &[S]) -> Result<Lexed, (Error, Range)> {
    let mut tks = Vec::new();
    for (idx, l) in lines.iter().enumerate() {
        let v: Vec<_> = l.as_ref().chars().collect();
        let mut i = 0;
        while i < v.len() {
            if v[i].is_whitespace() {
                i += 1;
            } else {
                let loc = Location {
                    row: idx + 1,
                    col: i + 1,
                };
                tks.push(Token {
                    loc: loc.clone(),
                    item: match v[i] {
                        '#' => {
                            // Indicates a line comment
                            // Discard the rest of the line
                            break;
                        }
                        ';' => {
                            i += 1;
                            LangItem::Semi
                        }
                        ',' => {
                            i += 1;
                            LangItem::Comma
                        }
                        '(' => {
                            i += 1;
                            LangItem::LPar
                        }
                        ')' => {
                            i += 1;
                            LangItem::RPar
                        }
                        '[' => {
                            i += 1;
                            LangItem::LBra
                        }
                        ']' => {
                            i += 1;
                            LangItem::RBra
                        }
                        '"' => {
                            i += 1;
                            match fragments::handle_string(&v[i..]) {
                                Ok((item, len)) => {
                                    i += len + 1; // Note end quote of the string
                                    item
                                }
                                Err((kind, len)) => {
                                    return Err((kind, loc_to_range(loc, len)));
                                }
                            }
                        }
                        _ => match fragments::handle_multichars(&v[i..]) {
                            Ok((item, len)) => {
                                i += len;
                                item
                            }
                            Err((kind, len)) => {
                                return Err((kind, loc_to_range(loc, len)));
                            }
                        },
                    },
                });
            }
        }
    }

    Ok(Lexed { tokens: tks })
}
