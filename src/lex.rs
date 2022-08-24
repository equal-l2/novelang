mod fragments;
mod items;
mod macros;
mod utils;

#[cfg(test)]
mod test;

pub use items::*;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Location {
    pub row: usize,
    pub col: usize,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
    pub loc: Location,
    pub item: LangItem,
}

impl Token {
    pub fn next_col_loc(&self) -> Location {
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
    pub lines: Vec<String>,
    pub tokens: Vec<Token>,
}

#[derive(Debug, Clone)]
pub struct LocWithLine {
    line: String,
    loc: Location,
}

impl std::fmt::Display for LocWithLine {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let row = self.loc.row;
        let col = self.loc.col;
        writeln!(f, "     |")?;
        writeln!(f, "{:<4} | {}", row, self.line)?;
        writeln!(f, "     | {:>1$}", "^", col)?;
        writeln!(f, "     |")?;
        Ok(())
    }
}

impl Lexed {
    pub fn generate_error_mesg(&self, idx: usize) -> LocWithLine {
        let loc = self.tokens.get(idx).map_or_else(
            || self.tokens.last().unwrap().next_col_loc(),
            |tk| tk.loc.clone(),
        );

        LocWithLine {
            line: self.lines[loc.row - 1].clone(),
            loc,
        }
    }
}

impl std::fmt::Display for Lexed {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut i = 0;
        for idx in 0..self.lines.len() {
            writeln!(f, "{:4>} |{}", idx + 1, self.lines[idx])?;
            while i < self.tokens.len() && self.tokens[i].loc.row == idx + 1 {
                write!(f, "{:?} ", self.tokens[i].item)?;
                i += 1;
            }
            writeln!(f)?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct Error {
    loc_info: LocWithLine,
    kind: ErrorKind,
}

impl std::error::Error for Error {}

#[derive(Debug, Clone)]
enum ErrorKind {
    UnterminatedStr,
    UnexpectedChar(char),
    TooLongNum,
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.kind {
            ErrorKind::UnterminatedStr => write!(f, "String is not terminated")?,
            ErrorKind::UnexpectedChar(c) => write!(f, "Unexpected character '{}'", c)?,
            ErrorKind::TooLongNum => write!(
                f,
                "Number is too long for {}-bit integer",
                std::mem::size_of::<crate::types::IntType>() * 8
            )?,
        };
        let l = &self.loc_info;
        writeln!(f, " ({}:{})\n{}", l.loc.row, l.loc.col, l)?;
        Ok(())
    }
}

pub fn lex<S: AsRef<str>>(s: S) -> Result<Lexed, Error> {
    let mut tks = Vec::new();
    let lines: Vec<_> = s.as_ref().lines().map(String::from).collect();
    for (idx, l) in lines.iter().enumerate() {
        let v: Vec<_> = l.chars().collect();
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
                                Err(kind) => {
                                    eprintln!("{:?}", tks);
                                    return Err(Error {
                                        loc_info: LocWithLine {
                                            line: l.clone(),
                                            loc,
                                        },
                                        kind,
                                    });
                                }
                            }
                        }
                        _ => match fragments::handle_multichars(&v[i..]) {
                            Ok((item, len)) => {
                                i += len;
                                item
                            }
                            Err(kind) => {
                                eprintln!("{:?}", tks);
                                return Err(Error {
                                    loc_info: LocWithLine {
                                        line: l.clone(),
                                        loc,
                                    },
                                    kind,
                                });
                            }
                        },
                    },
                });
            }
        }
    }

    Ok(Lexed { lines, tokens: tks })
}
