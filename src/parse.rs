use crate::exprs::Expr;
use crate::lex::{self, LangItem};
use crate::span::{Span, Spannable};

mod exprs;

#[macro_use]
mod utils;

use utils::*;

pub mod stmt;
use stmt::variant::*;

pub mod types;

mod target;

mod look_item;
pub(self) use look_item::LookItem;

mod from_tokens;
use from_tokens::*;

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
    Assert(Assert),
    Call(Call),
    Halt,
    Input(Input),
    Let(Let),
    Modify(Modify),
    Print(Print),
    Roll(Roll),
}

#[derive(Debug, Clone)]
pub enum BlockStmt {
    For(For),
    While { cond: Expr },
    Break,
    Continue,

    If { cond: Expr },
    ElIf { cond: Expr },
    Else,

    Sub(Sub),

    Return(Return),

    End,
}

#[derive(Debug, Clone)]
pub struct Parsed {
    pub stmts: Vec<Statement>,
}

pub fn parse(lexed: &lex::Lexed) -> Result<Parsed> {
    use lex::Command;

    // index 0 is reserved for placeholder
    let mut stmts = vec![Statement::Ill];

    let last = lexed.tokens.len();
    let mut tks = lexed.tokens.iter().enumerate().peekable();

    while let Some((idx, tok)) = tks.next() {
        if let LangItem::Cmd(inst) = &tok.item {
            match inst {
                Command::Assert => parse_normal!(stmts, {
                    NormalStmt::Assert(Assert::try_parse(&mut tks, last)?)
                }),

                lex::Command::Call => parse_normal!(stmts, {
                    NormalStmt::Call(Call::try_parse(&mut tks, last)?)
                }),

                lex::Command::Halt => parse_normal!(stmts, {
                    // "Halt" ";"
                    expects_semi!(tks, last);
                    NormalStmt::Halt
                }),

                lex::Command::Input => parse_normal!(stmts, {
                    NormalStmt::Input(Input::try_parse(&mut tks, last)?)
                }),

                lex::Command::Let => {
                    parse_normal!(stmts, { NormalStmt::Let(Let::try_parse(&mut tks, last)?) })
                }

                lex::Command::Modify => parse_normal!(stmts, {
                    NormalStmt::Modify(Modify::try_parse(&mut tks, last)?)
                }),

                Command::Print => parse_normal!(stmts, {
                    NormalStmt::Print(Print::try_parse(&mut tks, last)?)
                }),

                lex::Command::Roll => parse_normal!(stmts, {
                    NormalStmt::Roll(Roll::try_parse(&mut tks, last)?)
                }),

                /* block statements */
                lex::Command::For => parse_block!(stmts, idx, {
                    BlockStmt::For(For::try_parse(&mut tks, last)?)
                }),

                lex::Command::While => parse_block!(stmts, idx, {
                    // "While" <cond> ";"
                    let cond = parse_expr(&mut tks, last)?;
                    expects_semi!(tks, last);
                    BlockStmt::While { cond }
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
                    BlockStmt::Sub(Sub::try_parse(&mut tks, last)?)
                }),

                lex::Command::Return => parse_block!(stmts, idx, {
                    BlockStmt::Return(Return::try_parse(&mut tks, last)?)
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
