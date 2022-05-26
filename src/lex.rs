mod items;
mod macros;
mod utils;

pub use items::*;
use utils::*;

#[derive(Debug, Clone)]
pub struct Location {
    pub row: usize,
    pub col: usize,
}

#[derive(Debug, Clone)]
pub struct Token {
    pub loc: Location,
    pub item: Items,
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

#[derive(Debug, Clone)]
pub struct Lexed {
    pub lines: Vec<String>,
    pub tokens: Vec<Token>,
}

#[derive(Debug, Clone)]
pub struct LocInfo {
    line: String,
    loc: Location,
}

impl std::fmt::Display for LocInfo {
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
    pub fn generate_error_mesg(&self, idx: usize) -> LocInfo {
        let loc = self.tokens.get(idx).map_or_else(
            || self.tokens.last().unwrap().next_col_loc(),
            |tk| tk.loc.clone(),
        );

        LocInfo {
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
    loc_info: LocInfo,
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
                std::mem::size_of::<crate::IntType>() * 8
            )?,
        };
        let l = &self.loc_info;
        writeln!(f, " ({}:{})\n{}", l.loc.row, l.loc.col, l)?;
        Ok(())
    }
}

pub fn lex(s: String) -> Result<Lexed, Error> {
    let mut tks = Vec::new();
    let lines: Vec<_> = s.lines().map(String::from).collect();
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
                            break;
                        }
                        ';' => {
                            i += 1;
                            Items::Semi
                        }
                        ',' => {
                            i += 1;
                            Items::Comma
                        }
                        '(' => {
                            i += 1;
                            Items::LPar
                        }
                        ')' => {
                            i += 1;
                            Items::RPar
                        }
                        '[' => {
                            i += 1;
                            Items::LBra
                        }
                        ']' => {
                            i += 1;
                            Items::RBra
                        }
                        '"' => {
                            i += 1;
                            let mut s = String::new();
                            loop {
                                if i >= v.len() {
                                    return Err(Error {
                                        loc_info: LocInfo {
                                            line: l.clone(),
                                            loc,
                                        },
                                        kind: ErrorKind::UnterminatedStr,
                                    });
                                }
                                if v[i] == '"' {
                                    i += 1;
                                    break Items::Str(s);
                                }
                                s.push(v[i]);
                                i += 1;
                            }
                        }
                        _ => {
                            let vs = &v[i..];
                            let confirm_item = |len| len == vs.len() || !is_ident_char(vs[len]);
                            if is_item(&"die".chars().collect::<Vec<_>>(), vs) && confirm_item(3) {
                                // convert "die" to "dice"
                                i += 3;
                                Items::Key(Keyword::Dice)
                            } else if is_item(&"faces".chars().collect::<Vec<_>>(), vs)
                                && confirm_item(5)
                            {
                                // convert "faces" to "face"
                                i += 5;
                                Items::Key(Keyword::Face)
                            } else if let Some(res) = Keyword::parse_slice(vs) {
                                i += res.len();
                                Items::Key(res)
                            } else if let Some(res) = Command::parse_slice(vs) {
                                i += res.len();
                                Items::Cmd(res)
                            } else if let Some(res) = Ops::parse_slice(vs) {
                                i += res.len();
                                Items::Op(res)
                            } else if v[i].is_numeric() {
                                let mut s = String::new();
                                while i < v.len() && v[i].is_numeric() {
                                    s.push(v[i]);
                                    i += 1;
                                }
                                match s.parse() {
                                    Ok(i) => Items::Num(i, s.len()),
                                    Err(_) => {
                                        return Err(Error {
                                            loc_info: LocInfo {
                                                line: l.clone(),
                                                loc,
                                            },
                                            kind: ErrorKind::TooLongNum,
                                        })
                                    }
                                }
                            } else if is_ident_char(v[i]) {
                                let mut s = String::new();
                                while i < v.len() && is_ident_char(v[i]) {
                                    s.push(v[i]);
                                    i += 1;
                                }
                                Items::Ident(s)
                            } else {
                                eprintln!("{:?}", tks);
                                return Err(Error {
                                    loc_info: LocInfo {
                                        line: l.clone(),
                                        loc,
                                    },
                                    kind: ErrorKind::UnexpectedChar(v[i]),
                                });
                            }
                        }
                    },
                });
            }
        }
    }

    Ok(Lexed { lines, tokens: tks })
}
