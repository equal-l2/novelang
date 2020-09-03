trait ToItem
where
    Self: Sized + Copy + 'static,
{
    const DISCRIMINANTS: &'static [Self];

    fn as_str(&self) -> &str;

    fn check(s: &[char]) -> Option<Self> {
        'outer: for res in Self::DISCRIMINANTS {
            let res_chars: Vec<_> = res.as_str().chars().collect();
            if res_chars.len() <= s.len() {
                for i in 0..res_chars.len() {
                    if res_chars[i].to_lowercase().ne(s[i].to_lowercase()) {
                        continue 'outer;
                    }
                }
                return Some(*res);
            }
        }
        None
    }

    fn len(&self) -> usize {
        self.as_str().len()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Insts {
    Print,
    Sub,
    Call,
    While,
    Let,
    Modify,
    Input,
    If,
    ElIf,
    Else,
    End,
    Roll,
    Halt,
    Break,
    EnableWait,
    DisableWait,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Keywords {
    AsMut,
    Be,
    To,
    Dice,
    With,
    Face,
}

impl ToItem for Keywords {
    const DISCRIMINANTS: &'static [Self] = &[
        Self::AsMut,
        Self::Be,
        Self::To,
        Self::Dice,
        Self::With,
        Self::Face,
    ];

    fn as_str(&self) -> &str {
        match self {
            Self::AsMut => "asmut",
            Self::Be => "be",
            Self::To => "to",
            Self::Dice => "dice",
            Self::With => "with",
            Self::Face => "face",
        }
    }

    fn check(s: &[char]) -> Option<Self> {
        'outer: for res in Self::DISCRIMINANTS {
            let res_chars: Vec<_> = res.as_str().chars().collect();
            if res_chars.len() <= s.len() {
                for i in 0..res_chars.len() {
                    if res_chars[i].to_lowercase().ne(s[i].to_lowercase()) {
                        continue 'outer;
                    }
                }
                // For Reserved we need this check to separate Ident
                // (example: "be" is Reserved but "bed" is Ident)
                if res_chars.len() == s.len() || is_sep(s[res_chars.len()]) {
                    return Some(*res);
                }
            }
        }
        None
    }
}

impl ToItem for Insts {
    const DISCRIMINANTS: &'static [Self] = &[
        Self::Print,
        Self::Sub,
        Self::Call,
        Self::While,
        Self::Let,
        Self::Modify,
        Self::Input,
        Self::If,
        Self::ElIf,
        Self::Else,
        Self::End,
        Self::Roll,
        Self::Halt,
        Self::Break,
        Self::EnableWait,
        Self::DisableWait,
    ];

    fn as_str(&self) -> &str {
        match self {
            Self::Print => "print",
            Self::Sub => "sub",
            Self::Call => "call",
            Self::While => "while",
            Self::Let => "let",
            Self::Modify => "modify",
            Self::Input => "input",
            Self::If => "if",
            Self::ElIf => "elif",
            Self::Else => "else",
            Self::End => "end",
            Self::Roll => "roll",
            Self::Halt => "halt",
            Self::Break => "break",
            Self::EnableWait => "enablewait",
            Self::DisableWait => "disablewait",
        }
    }

    fn check(s: &[char]) -> Option<Self> {
        'outer: for res in Self::DISCRIMINANTS {
            let res_chars: Vec<_> = res.as_str().chars().collect();
            if res_chars.len() <= s.len() {
                for i in 0..res_chars.len() {
                    if res_chars[i].to_lowercase().ne(s[i].to_lowercase()) {
                        continue 'outer;
                    }
                }
                // For Reserved we need this check to separate Ident
                // (example: "be" is Reserved but "bed" is Ident)
                if res_chars.len() == s.len() || is_sep(s[res_chars.len()]) {
                    return Some(*res);
                }
            }
        }
        None
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AriOps {
    Add, // +
    Sub, // -
    Mul, // *
    Div, // /
    Mod, // %
}

impl ToItem for AriOps {
    const DISCRIMINANTS: &'static [Self] = &[
        Self::Add, // +
        Self::Sub, // -
        Self::Mul, // *
        Self::Div, // /
        Self::Mod, // %
    ];
    fn as_str(&self) -> &str {
        match self {
            Self::Add => "+",
            Self::Sub => "-",
            Self::Mul => "*",
            Self::Div => "/",
            Self::Mod => "%",
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RelOps {
    Equal,        // ==
    NotEqual,     // !=
    LessEqual,    // <=
    GreaterEqual, // >=
    LessThan,     // <
    GreaterThan,  // >
}

impl ToItem for RelOps {
    const DISCRIMINANTS: &'static [Self] = &[
        Self::Equal,        // ==
        Self::NotEqual,     // !=
        Self::LessEqual,    // <=
        Self::GreaterEqual, // >=
        Self::LessThan,     // <
        Self::GreaterThan,  // >
    ];
    fn as_str(&self) -> &str {
        match self {
            Self::Equal => "==",
            Self::NotEqual => "!=",
            Self::LessEqual => "<=",
            Self::GreaterEqual => ">=",
            Self::LessThan => "<",
            Self::GreaterThan => ">",
        }
    }
}

type NumType = i64;
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Item {
    Key(Keywords),
    Inst(Insts),
    Ari(AriOps),
    Rel(RelOps),
    Num(NumType),
    Ident(String),
    Str(String),
    Semi,
    Comma,
}

#[derive(Debug, Clone)]
pub struct Location {
    pub row: usize,
    pub col: usize,
}

#[derive(Debug, Clone)]
pub struct Token {
    pub loc: Location,
    pub item: Item,
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
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
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let row = self.loc.row;
        let col = self.loc.col;
        writeln!(f, "     |")?;
        writeln!(f, "{:<4} |{}", row, self.line)?;
        writeln!(f, "     |{:>1$}", "^", col)?;
        writeln!(f, "     |")?;
        Ok(())
    }
}

impl Lexed {
    pub fn generate_loc_info(&self, loc: &Location) -> LocInfo {
        LocInfo {
            line: self.lines[loc.row - 1].clone(),
            loc: loc.clone(),
        }
    }
    pub fn iter(&self) -> impl Iterator<Item = &Token> {
        self.tokens.iter()
    }
}

impl std::fmt::Display for Lexed {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
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
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match &self.kind {
            ErrorKind::UnterminatedStr => write!(f, "String is not terminated")?,
            ErrorKind::UnexpectedChar(c) => write!(f, "Unexpected character '{}'", c)?,
        };
        let l = &self.loc_info;
        writeln!(f, " ({}:{})\n{}", l.loc.row, l.loc.col, l)?;
        Ok(())
    }
}

const RESERVED_CHARS: &'static [char] =
    &['+', '-', '*', '/', '%', '"', '<', '>', '!', '=', ';', ','];

fn is_ident_char(c: char) -> bool {
    !c.is_whitespace() && !RESERVED_CHARS.contains(&c)
}

fn is_sep(c: char) -> bool {
    c.is_whitespace() || c == ';'
}

pub fn lex(s: String) -> Result<Lexed, Error> {
    let mut tks = Vec::new();
    let lines: Vec<_> = s.lines().map(String::from).collect();
    for (idx, l) in lines.iter().enumerate() {
        let v: Vec<_> = l.chars().collect();
        let mut i = 0;
        while i < v.len() {
            let loc = Location {
                row: idx + 1,
                col: i + 1,
            };
            if v[i] == '#' {
                break;
            } else if v[i].is_whitespace() {
                i += 1;
            } else if v[i] == ';' {
                tks.push(Token {
                    loc,
                    item: Item::Semi,
                });
                i += 1;
            } else if v[i] == ',' {
                tks.push(Token {
                    loc,
                    item: Item::Comma,
                });
                i += 1;
            } else if v[i] == '"' {
                i += 1;
                let mut s = String::new();
                while i < v.len() {
                    if v[i] == '"' {
                        break;
                    }
                    s.push(v[i]);
                    i += 1;
                }
                if v[i] != '"' {
                    let loc_info = LocInfo {
                        line: l.clone(),
                        loc,
                    };
                    return Err(Error {
                        loc_info,
                        kind: ErrorKind::UnterminatedStr,
                    });
                }
                tks.push(Token {
                    loc,
                    item: Item::Str(s),
                });
                i += 1;
            } else if let Some(res) = Keywords::check(&v[i..v.len()]) {
                tks.push(Token {
                    loc,
                    item: Item::Key(res),
                });
                i += res.len();
            } else if let Some(res) = Insts::check(&v[i..v.len()]) {
                tks.push(Token {
                    loc,
                    item: Item::Inst(res),
                });
                i += res.len();
            } else if let Some(res) = AriOps::check(&v[i..v.len()]) {
                tks.push(Token {
                    loc,
                    item: Item::Ari(res),
                });
                i += res.len();
            } else if let Some(res) = RelOps::check(&v[i..v.len()]) {
                tks.push(Token {
                    loc,
                    item: Item::Rel(res),
                });
                i += res.len();
            } else if v[i].is_numeric() {
                let mut s = String::new();
                while i < v.len() && v[i].is_numeric() {
                    s.push(v[i]);
                    i += 1;
                }
                tks.push(Token {
                    loc,
                    item: Item::Num(s.parse().unwrap()),
                });
            } else if is_ident_char(v[i]) {
                let mut s = String::new();
                while i < v.len() && is_ident_char(v[i]) {
                    s.push(v[i]);
                    i += 1;
                }
                tks.push(Token {
                    loc,
                    item: Item::Ident(s),
                });
            } else {
                eprintln!("{:?}", tks);
                let loc_info = LocInfo {
                    line: l.clone(),
                    loc,
                };
                return Err(Error {
                    loc_info,
                    kind: ErrorKind::UnexpectedChar(v[i]),
                });
            }
        }
    }
    Ok(Lexed { lines, tokens: tks })
}
