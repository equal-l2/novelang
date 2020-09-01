#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ResKind {
    // keywords
    AsMut,
    Be,
    To,
    Dice,
    With,
    Face,
    // insts
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

impl ResKind {
    fn check_reserved(s: &[char]) -> Option<ResKind> {
        'outer: for res in Self::iter() {
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

    fn as_str(&self) -> &str {
        match self {
            // keywords
            Self::AsMut => "asmut",
            Self::Be => "be",
            Self::To => "to",
            Self::Dice => "dice",
            Self::With => "with",
            Self::Face => "face",
            // insts
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

    fn iter() -> impl Iterator<Item = &'static Self> {
        [
            // keywords
            ResKind::AsMut,
            ResKind::Be,
            ResKind::To,
            ResKind::Dice,
            ResKind::With,
            ResKind::Face,
            // insts
            ResKind::Print,
            ResKind::Sub,
            ResKind::Call,
            ResKind::While,
            ResKind::Let,
            ResKind::Modify,
            ResKind::Input,
            ResKind::If,
            ResKind::ElIf,
            ResKind::Else,
            ResKind::End,
            ResKind::Roll,
            ResKind::Halt,
            ResKind::Break,
            ResKind::EnableWait,
            ResKind::DisableWait,
        ].into_iter()
    }
}

#[derive(Debug, Clone)]
enum OpKind {}

type NumType = i64;
#[derive(Debug, Clone)]
enum TokenKind {
    Res(ResKind),
    Op(OpKind),
    Num(NumType),
    Ident(String),
    Semi,
}

#[derive(Debug, Clone)]
pub struct Location {
    pub row: usize,
    pub col: usize,
}

#[derive(Clone)]
pub struct Token {
    pub loc: Location,
    kind: TokenKind,
}

impl std::fmt::Debug for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "Token {{ {:?} ({},{}) }}", self.kind, self.loc.row, self.loc.col)
    }
}

#[derive(Debug, Clone)]
pub struct Lexed {
    pub lines: Vec<String>,
    pub tokens: Vec<Token>,
}

#[derive(Debug, Clone)]
pub struct Error {}

fn is_qual(c: char) -> bool {
    !c.is_whitespace() && c != ';'
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
            }
            else if v[i] == ';' {
                tks.push(Token {
                    loc: Location {
                        row: idx + 1,
                        col: i + 1,
                    },
                    kind: TokenKind::Semi,
                });
                i += 1;
            } else if let Some(res) = ResKind::check_reserved(&v[i..l.len()]) {
                let res_len = res.len();
                if l.len() == res_len || v[i + res_len] == ';' || v[i + res_len].is_whitespace() {
                    tks.push(Token {
                        loc: Location {
                            row: idx + 1,
                            col: i + 1,
                        },
                        kind: TokenKind::Res(res),
                    });
                    i += res_len;
                }
            } else if v[i].is_numeric() {
                let orig_i = i;
                let mut s = String::new();
                while i < v.len() && v[i].is_numeric() {
                    s.push(v[i]);
                    i += 1;
                }
                tks.push(Token {
                    loc: Location {
                        row: idx + 1,
                        col: orig_i + 1,
                    },
                    kind: TokenKind::Num(s.parse().unwrap()),
                });
            } else {
                let orig_i = i;
                let mut s = String::new();
                while i < v.len() && is_qual(v[i]) {
                    s.push(v[i]);
                    i += 1;
                }
                tks.push(Token {
                    loc: Location {
                        row: idx + 1,
                        col: orig_i + 1,
                    },
                    kind: TokenKind::Ident(s),
                });
            }
        }
    }
    Ok(Lexed { lines, tokens: tks })
}
