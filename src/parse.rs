use crate::exprs::Expr;
use crate::lex::{self, LangItem};
use crate::span::{Span, Spannable};
use crate::target::{Ident, Target};

mod exprs;

#[macro_use]
mod utils;

use utils::*;

mod stmt;
pub use stmt::call::Call;
pub use stmt::sub::{Arg, Sub, Ty, Type};

mod target;
use target::parse_target;

mod look_item;
pub(self) use look_item::LookItem;

#[derive(Debug)]
pub struct Error(pub String);

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

type Result<T> = std::result::Result<T, (Error, Span)>;

#[derive(Debug, Clone, derive_more::From)]
pub enum Statement {
    // always valid as a statement
    Normal(NormalStmt),

    // can be invalid depending on the context because of involving a block
    // save the index of the starting token for diagnostics
    Block(BlockStmt, Span),

    // always invalid
    Ill,
}

#[derive(Debug, Clone)]
pub enum NormalStmt {
    Assert {
        mesg: Expr,
        cond: Expr,
    },
    Call(Call),
    Halt,
    Input {
        prompt: Option<String>,
        target: Target,
    },
    Let {
        name: Ident,
        init: Expr,
        is_mut: bool,
    },
    Modify {
        target: Target,
        expr: Expr,
    },
    Print {
        args: Vec<Expr>,
    },
    Roll {
        count: Expr,
        faces: Expr,
        target: Target,
    },
}

#[derive(Debug, Clone)]
pub enum BlockStmt {
    For {
        counter: Ident,
        from: Expr,
        to: Expr,
    },
    While {
        cond: Expr,
    },
    Break,
    Continue,

    If {
        cond: Expr,
    },
    ElIf {
        cond: Expr,
    },
    Else,

    Sub(Sub),

    Return(Option<Expr>),

    End,
}

#[derive(Debug, Clone)]
pub struct Parsed {
    pub stmts: Vec<Statement>,
}

pub fn parse(lexed: &lex::Lexed) -> Result<Parsed> {
    use lex::{Command, Keyword};

    // index 0 is reserved for placeholder
    let mut stmts = vec![Statement::Ill];

    let last = lexed.tokens.len();
    let mut tks = lexed.tokens.iter().enumerate().peekable();

    while let Some((idx, tok)) = tks.next() {
        if let LangItem::Cmd(inst) = &tok.item {
            match inst {
                Command::Assert => parse_normal!(stmts, {
                    // "Assert" (<mesg> "With") <cond> ";"
                    let expr1 = parse_expr(&mut tks, last)?;

                    let (_, tk) = tks
                        .peek()
                        .ok_or_else(|| (Error("expected With or Semicolon".into()), last.into()))?;

                    let (mesg, cond) = if tk.item == LangItem::Key(Keyword::With) {
                        // with string
                        let _ = tks.next().unwrap();
                        let expr2 = parse_expr(&mut tks, last)?;
                        (expr1, expr2)
                    } else {
                        // without string
                        (Expr::from(expr1.to_string()), expr1)
                    };
                    expects_semi!(tks, last);

                    NormalStmt::Assert { mesg, cond }
                }),

                lex::Command::Call => parse_normal!(stmts, {
                    // "Call" <name> ";"
                    NormalStmt::Call(Call::try_parse(&mut tks, last)?)
                }),

                lex::Command::Halt => parse_normal!(stmts, {
                    // "Halt" ";"
                    expects_semi!(tks, last);
                    NormalStmt::Halt
                }),

                lex::Command::Input => parse_normal!(stmts, {
                    // "Input" (<str>) "To" <target> ";"

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

                    let target = parse_target(&mut tks, last)?;

                    expects_semi!(tks, last);
                    NormalStmt::Input { prompt, target }
                }),

                lex::Command::Let => parse_normal!(stmts, {
                    // "Let" <name> "Be" <init> ("AsMut") ";"

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

                        let init = parse_expr(&mut tks, last)?;

                        let i = init.span().1;

                        match tks.next() {
                            Some((_, tk)) => match tk.item {
                                LangItem::Key(Keyword::AsMut) => {
                                    expects_semi!(tks, last);
                                    NormalStmt::Let {
                                        name: Ident(name.clone(), i.into()),
                                        init,
                                        is_mut: true,
                                    }
                                }
                                LangItem::Semi => NormalStmt::Let {
                                    name: Ident(name.clone(), i.into()),
                                    init,
                                    is_mut: false,
                                },
                                _ => {
                                    return Err((
                                        Error("expected Semicolon or AsMut".into()),
                                        (i + 1).into(),
                                    ))
                                }
                            },
                            None => {
                                return Err((
                                    Error("expected Semicolon or AsMut".into()),
                                    (i + 1).into(),
                                ));
                            }
                        }
                    } else {
                        return Err((Error("Ident expected".into()), i.into()));
                    }
                }),

                lex::Command::Modify => parse_normal!(stmts, {
                    // "Modify" <target> "To" <expr> ";"
                    let target = parse_target(&mut tks, last)?;
                    expects!("To expected", LangItem::Key(Keyword::To), tks, last);

                    let expr = parse_expr(&mut tks, last)?;
                    expects_semi!(tks, last);

                    NormalStmt::Modify { target, expr }
                }),

                Command::Print => parse_normal!(stmts, {
                    // "Print" (<expr> {"," <expr>}) ";"
                    let mut args = vec![parse_expr(&mut tks, last)?];

                    while let Some((i, tk)) = tks.peek() {
                        match tk.item {
                            LangItem::Semi => break,
                            LangItem::Comma => {
                                let _ = tks.next().unwrap();

                                if matches!(tks.peek().item(), Some(LangItem::Semi)) {
                                    break;
                                }

                                args.push(parse_expr(&mut tks, last)?);
                            }
                            _ => {
                                return Err((Error("Unexpected token".into()), (*i).into()));
                            }
                        }
                    }
                    expects_semi!(tks, last);
                    NormalStmt::Print { args }
                }),

                lex::Command::Roll => parse_normal!(stmts, {
                    // "Roll" <expr> "Dice" "With" <expr> "Faces" "To" <target> ";"

                    let count = parse_expr(&mut tks, last)?;

                    expects!("\"Dice\" expected", LangItem::Key(Keyword::Dice), tks, last);
                    expects!("\"With\" expected", LangItem::Key(Keyword::With), tks, last);

                    let faces = parse_expr(&mut tks, last)?;

                    expects!(
                        "\"Faces\" expected",
                        LangItem::Key(Keyword::Faces),
                        tks,
                        last
                    );
                    expects!("\"To\" expected", LangItem::Key(Keyword::To), tks, last);

                    let target = parse_target(&mut tks, last)?;

                    expects_semi!(tks, last);
                    NormalStmt::Roll {
                        count,
                        faces,
                        target,
                    }
                }),

                /* block statements */
                lex::Command::For => parse_block!(stmts, idx, {
                    // "For" <name> "from" <expr> "to" <expr> ";"

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
                        expects!("\"From\" expected", LangItem::Key(Keyword::From), tks, last);

                        let from = parse_expr(&mut tks, last)?;

                        expects!("\"To\" expected", LangItem::Key(Keyword::To), tks, last);

                        let to = parse_expr(&mut tks, last)?;

                        expects_semi!(tks, last);

                        BlockStmt::For {
                            counter: Ident(name.clone(), i.into()),
                            from,
                            to,
                        }
                    } else {
                        return Err((Error("Ident expected".into()), i.into()));
                    }
                }),

                lex::Command::While => parse_block!(stmts, idx, {
                    // "While" <cond> ";"
                    let expr = parse_expr(&mut tks, last)?;
                    expects_semi!(tks, last);
                    BlockStmt::While { cond: expr }
                }),

                lex::Command::Break => parse_block!(stmts, idx, {
                    // "Break" ";"
                    expects_semi!(tks, last);

                    BlockStmt::Break
                }),

                lex::Command::Continue => parse_block!(stmts, idx, {
                    // "Continue" ";"
                    expects_semi!(tks, last);
                    BlockStmt::Continue
                }),

                lex::Command::If => parse_block!(stmts, idx, {
                    // "If" <cond> ";"

                    let cond = parse_expr(&mut tks, last)?;
                    expects_semi!(tks, last);

                    BlockStmt::If { cond }
                }),

                lex::Command::Else => {
                    stmts.push({
                        // "Else" ("If" <cond>) ";"
                        let (_, tk) = tks.peek().ok_or_else(|| {
                            (Error("expected Semicolon or If".into()), last.into())
                        })?;

                        if tk.item == LangItem::Cmd(lex::Command::If) {
                            // "Else" "If" <cond> ";"
                            let _ = tks.next().unwrap();

                            let cond = parse_expr(&mut tks, last)?;
                            expects_semi!(tks, last);

                            Statement::Block(BlockStmt::ElIf { cond }, Span(idx, idx + 1))
                        } else {
                            // "Else" ";"
                            expects_semi!(tks, last);
                            Statement::Block(BlockStmt::Else, idx.into())
                        }
                    });
                }

                lex::Command::Sub => parse_block!(stmts, idx, {
                    // "Sub" <name> ";"
                    BlockStmt::Sub(Sub::try_parse(&mut tks, last)?)
                }),

                lex::Command::Return => parse_block!(stmts, idx, {
                    let (_, tk) = tks
                        .peek()
                        .ok_or_else(|| (Error("Arg abruptly ended".into()), last.into()))?;

                    let ret = if tk.item == LangItem::Key(Keyword::With) {
                        // with a return value
                        let _ = tks.next();
                        Some(parse_expr(&mut tks, last)?)
                    } else {
                        None
                    };

                    expects_semi!(tks, last);

                    BlockStmt::Return(ret)
                }),

                lex::Command::End => parse_block!(stmts, idx, {
                    // "End" ";"
                    expects_semi!(tks, last);
                    BlockStmt::End
                }),
            }
        } else {
            return Err((Error("Line must begin with Command".into()), idx.into()));
        }
    }
    Ok(Parsed { stmts })
}
