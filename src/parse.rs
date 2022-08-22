use crate::exprs::Expr;
use crate::lex;
use crate::lval::LVal;
use crate::span::{Span, Spannable};
use crate::IdentName;

mod exprs;

#[macro_use]
mod utils;

use utils::*;

#[derive(Debug)]
pub struct Error(pub String, pub Span);

#[derive(Debug, Clone, derive_more::From)]
pub enum Statement {
    // always valid as a statement
    Normal(NormalStmt),

    // can be invalid depending on the context because of involving a block
    // save the location of the starting token for diagnostics
    Block(BlockStmt, lex::Location),

    // always invalid
    Ill,
}

#[derive(Debug, Clone)]
pub enum NormalStmt {
    Assert {
        mesg: Expr,
        cond: Expr,
    },
    Call {
        // FUTURE: accept expression, allowing containing functions in an array
        name: IdentName,
    },
    Halt,
    Input {
        prompt: Option<String>,
        target: LVal,
    },
    Let {
        name: IdentName,
        init: Expr,
        is_mut: bool,
    },
    Modify {
        target: LVal,
        expr: Expr,
    },
    Print {
        args: Vec<Expr>,
    },
    Roll {
        count: Expr,
        face: Expr,
        target: LVal,
    },
}

#[derive(Debug, Clone)]
pub enum BlockStmt {
    For {
        counter: IdentName,
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

    Sub {
        name: IdentName,
    },
    Return,

    End,
}

#[derive(Debug, Clone)]
pub struct Parsed {
    pub stmts: Vec<Statement>,
}

trait LookItem {
    fn item(self) -> Option<lex::LangItem>;
}

impl LookItem for Option<&lex::Token> {
    fn item(self) -> Option<lex::LangItem> {
        self.map(|t| t.item.clone())
    }
}

fn parse_lval<'a, T>(tks: &mut std::iter::Peekable<T>, last: usize) -> Result<LVal, Error>
where
    T: Iterator<Item = (usize, &'a lex::Token)>,
{
    use lex::LangItem;

    let (i, tk) = tks
        .peek()
        .ok_or_else(|| Error("expected Ident".into(), last.into()))?;
    if let LangItem::Ident(name) = &tk.item {
        let mut val = LVal::Scalar(name.clone());
        let _ = tks.next().unwrap();
        loop {
            if tks.peek().item() == Some(LangItem::LBra) {
                let _ = tks.next().unwrap();
                let expr = parse_expr(tks, last)?;
                expects!("expected RBra", LangItem::RBra, tks, last);
                val = LVal::Vector(Box::from(val), Box::from(expr));
            } else {
                return Ok(val);
            }
        }
    } else {
        Err(Error("expected Ident".into(), (*i).into()))
    }
}

pub fn parse(lexed: &lex::Lexed) -> Result<Parsed, Error> {
    use lex::{Command, Keyword, LangItem};

    // index 0 is reserved for placeholder
    let mut stmts = vec![Statement::Ill];

    let last = lexed.tokens.len();
    let mut tks = lexed.tokens.iter().enumerate().peekable();

    while let Some((idx, tok)) = tks.next() {
        let loc = &tok.loc;
        if let LangItem::Cmd(inst) = &tok.item {
            match inst {
                Command::Assert => parse_normal!(stmts, {
                    // "Assert" (<str> "With") <cond> ";"
                    let expr1 = parse_expr(&mut tks, last)?;

                    let (_, tk) = tks
                        .peek()
                        .ok_or_else(|| Error("expected With or Semicolon".into(), last.into()))?;

                    let mesg;
                    let cond;
                    if tk.item == LangItem::Key(Keyword::With) {
                        // with string
                        let _ = tks.next().unwrap();
                        mesg = expr1;
                        let expr2 = parse_expr(&mut tks, last)?;
                        cond = expr2;
                    } else {
                        // without string
                        mesg = Expr::from(expr1.to_string());
                        cond = expr1;
                    }
                    expects_semi!(tks, last);

                    NormalStmt::Assert { mesg, cond }
                }),

                lex::Command::Call => parse_normal!(stmts, {
                    // "Call" <name> ";"
                    let (i, tk) = tks
                        .next()
                        .ok_or_else(|| Error("expected subrountine name".into(), last.into()))?;
                    if let LangItem::Ident(name) = &tk.item {
                        expects_semi!(tks, last);
                        NormalStmt::Call { name: name.clone() }
                    } else {
                        return Err(Error("Expected subroutine name".into(), i.into()));
                    }
                }),

                lex::Command::Halt => parse_normal!(stmts, {
                    // "Halt" ";"
                    expects_semi!(tks, last);
                    NormalStmt::Halt
                }),

                lex::Command::Input => parse_normal!(stmts, {
                    // "Input" (<str>) "To" <lval> ";"

                    // TODO: accept runtime prompt string
                    let (_, tk) = tks
                        .peek()
                        .ok_or_else(|| Error("expected To".into(), last.into()))?;

                    let prompt = if let LangItem::Str(prompt) = &tk.item {
                        let _ = tks.next().unwrap();
                        Some(prompt.clone())
                    } else {
                        None
                    };

                    expects!("\"To\" expected", LangItem::Key(Keyword::To), tks, last);

                    let lval = parse_lval(&mut tks, last)?;

                    expects_semi!(tks, last);
                    NormalStmt::Input {
                        prompt,
                        target: lval,
                    }
                }),

                lex::Command::Let => parse_normal!(stmts, {
                    // "Let" <name> "Be" <expr> ("AsMut") ";"

                    let (i, tk) = tks
                        .next()
                        .ok_or_else(|| Error("expected Ident".into(), last.into()))?;
                    if let LangItem::Ident(name) = &tk.item {
                        if name.as_ref().starts_with('_') {
                            return Err(Error(
                                "Identifier starts with _ is reserved".into(),
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
                                        name: name.clone(),
                                        init,
                                        is_mut: true,
                                    }
                                }
                                LangItem::Semi => NormalStmt::Let {
                                    name: name.clone(),
                                    init,
                                    is_mut: false,
                                },
                                _ => {
                                    return Err(Error(
                                        "expected Semicolon or AsMut".into(),
                                        (i + 1).into(),
                                    ))
                                }
                            },
                            None => {
                                return Err(Error(
                                    "expected Semicolon or AsMut".into(),
                                    (i + 1).into(),
                                ));
                            }
                        }
                    } else {
                        return Err(Error("Ident expected".into(), i.into()));
                    }
                }),

                lex::Command::Modify => parse_normal!(stmts, {
                    // "Modify" <lval> "To" <expr> ";"
                    let lval = parse_lval(&mut tks, last)?;
                    expects!("To expected", LangItem::Key(Keyword::To), tks, last);

                    let expr = parse_expr(&mut tks, last)?;
                    expects_semi!(tks, last);

                    NormalStmt::Modify {
                        target: lval.clone(),
                        expr,
                    }
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
                                return Err(Error("Unexpected token".into(), (*i).into()));
                            }
                        }
                    }
                    expects_semi!(tks, last);
                    NormalStmt::Print { args }
                }),

                lex::Command::Roll => parse_normal!(stmts, {
                    // "Roll" <expr> "Dice" "With" <expr> "Faces" "To" <lval> ";"

                    let count = parse_expr(&mut tks, last)?;

                    expects!("\"Dice\" expected", LangItem::Key(Keyword::Dice), tks, last);
                    expects!("\"With\" expected", LangItem::Key(Keyword::With), tks, last);

                    let face = parse_expr(&mut tks, last)?;

                    expects!(
                        "\"Faces\" expected",
                        LangItem::Key(Keyword::Faces),
                        tks,
                        last
                    );
                    expects!("\"To\" expected", LangItem::Key(Keyword::To), tks, last);

                    let lval = parse_lval(&mut tks, last)?;

                    expects_semi!(tks, last);
                    NormalStmt::Roll {
                        count,
                        face,
                        target: lval,
                    }
                }),

                /* block statements */
                lex::Command::For => parse_block!(stmts, loc, {
                    // "For" <name> "from" <expr> "to" <expr> ";"

                    let (i, tk) = tks
                        .next()
                        .ok_or_else(|| Error("expected Ident".into(), last.into()))?;

                    if let LangItem::Ident(name) = &tk.item {
                        if name.as_ref().starts_with('_') {
                            return Err(Error(
                                "Identifier starts with _ is reserved".into(),
                                i.into(),
                            ));
                        }
                        expects!("\"From\" expected", LangItem::Key(Keyword::From), tks, last);

                        let from = parse_expr(&mut tks, last)?;

                        expects!("\"To\" expected", LangItem::Key(Keyword::To), tks, last);

                        let to = parse_expr(&mut tks, last)?;

                        expects_semi!(tks, last);

                        BlockStmt::For {
                            counter: name.clone(),
                            from,
                            to,
                        }
                    } else {
                        return Err(Error("Ident expected".into(), i.into()));
                    }
                }),

                lex::Command::While => parse_block!(stmts, loc, {
                    // "While" <cond> ";"
                    let expr = parse_expr(&mut tks, last)?;
                    expects_semi!(tks, last);
                    BlockStmt::While { cond: expr }
                }),

                lex::Command::Break => parse_block!(stmts, loc, {
                    // "Break" ";"
                    expects_semi!(tks, last);

                    BlockStmt::Break
                }),

                lex::Command::Continue => parse_block!(stmts, loc, {
                    // "Continue" ";"
                    expects_semi!(tks, last);
                    BlockStmt::Continue
                }),

                lex::Command::If => parse_block!(stmts, loc, {
                    // "If" <cond> ";"

                    let cond = parse_expr(&mut tks, last)?;
                    expects_semi!(tks, last);

                    BlockStmt::If { cond }
                }),

                lex::Command::Else => parse_block!(stmts, loc, {
                    // "Else" ("If" <cond>) ";"

                    let (_, tk) = tks
                        .peek()
                        .ok_or_else(|| Error("expected Semicolon or If".into(), last.into()))?;

                    if tk.item == LangItem::Cmd(lex::Command::If) {
                        // "Else" "If" <cond> ";"
                        let _ = tks.next().unwrap();

                        let cond = parse_expr(&mut tks, last)?;
                        expects_semi!(tks, last);

                        BlockStmt::ElIf { cond }
                    } else {
                        // "Else" ";"
                        expects_semi!(tks, last);
                        BlockStmt::Else
                    }
                }),

                lex::Command::Sub => parse_block!(stmts, loc, {
                    // "Sub" <name> ";"
                    let (i, tk) = tks
                        .next()
                        .ok_or_else(|| Error("expected subrountine name".into(), last.into()))?;
                    if let LangItem::Ident(name) = &tk.item {
                        expects_semi!(tks, last);
                        BlockStmt::Sub { name: name.clone() }
                    } else {
                        return Err(Error("Expected subroutine name".into(), i.into()));
                    }
                }),

                lex::Command::Return => parse_block!(stmts, loc, {
                    // "Return" ";"
                    expects_semi!(tks, last);
                    BlockStmt::Return
                }),

                lex::Command::End => parse_block!(stmts, loc, {
                    // "End" ";"
                    expects_semi!(tks, last);
                    BlockStmt::End
                }),
            }
        } else {
            return Err(Error("Line must begin with Command".into(), idx.into()));
        }
    }
    Ok(Parsed { stmts })
}
