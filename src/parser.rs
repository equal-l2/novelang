use crate::exprs::*;
use pest::Parser;

macro_rules! die {
    ($( $x:expr ),*) => {
        eprintln!($($x,)*);
        std::process::exit(1);
    }
}

#[derive(pest_derive::Parser)]
#[grammar = "prog.pest"]
struct ProgParser;

#[derive(Debug, Clone)]
pub enum PrintArgs {
    String(String),
    Expr(Expr),
}

#[derive(Debug, Clone)]
pub enum Inst {
    Print {
        args: Vec<PrintArgs>,
    },
    Sub {
        name: String,
        offset_to_end: usize,
    },
    Call {
        name: String,
    },
    While {
        cond: CompExpr,
        offset_to_end: usize,
    },
    Let {
        name: String,
        init: Expr,
        is_mut: bool,
    },
    Modify {
        name: String,
        expr: Expr,
    },
    End,
}

#[derive(Debug, Clone)]
pub struct Program {
    pub insts: Vec<Inst>,
    pub subs: std::collections::HashMap<String, usize>,
}

struct WaitsEnd {
    kind: Inst,
    index: usize,
}

pub fn parse(s: &str) -> Option<Program> {
    let stmts = ProgParser::parse(Rule::Prog, &s);
    if let Err(e) = stmts {
        eprintln!("{}", e);
        return None;
    }
    let stmts = stmts.unwrap();

    let mut insts = vec![];
    let mut waits_end_stack: Vec<WaitsEnd> = vec![]; // stmts waiting for End
    let mut subs = std::collections::HashMap::new(); // subroutines defined
    for stmt in stmts {
        match stmt.as_rule() {
            Rule::Print => insts.push(Inst::Print {
                args: stmt
                    .into_inner()
                    .map(|s| match s.as_rule() {
                        Rule::StringContent => PrintArgs::String(s.as_str().to_owned()),
                        Rule::Expr => PrintArgs::Expr(Expr::parse_stmt(s)),
                        other => {
                            die!("Semantic error: unexpected rule : {:?}", other);
                        }
                    })
                    .collect(),
            }),
            Rule::Sub => {
                // check if the Sub is nested (which is not allowed)
                if let Some(i) = waits_end_stack.last() {
                    if let Inst::Sub { .. } = i.kind {
                        die!("Semantic error: you cannot nest Sub.");
                    }
                }

                // register the sub to the name table
                let fn_name = stmt.into_inner().as_str().to_owned();
                if subs.insert(fn_name.clone(), insts.len()).is_some() {
                    die!(
                        "Semantic error: function name \"{}\" is conflicting",
                        fn_name
                    );
                }

                let inst_obj = Inst::Sub {
                    name: fn_name,
                    offset_to_end: 0,
                };
                waits_end_stack.push(WaitsEnd {
                    kind: inst_obj.clone(),
                    index: insts.len(),
                });
                insts.push(inst_obj);
            }
            Rule::Call => insts.push(Inst::Call {
                name: stmt.into_inner().as_str().to_owned(),
            }),
            Rule::While => {
                let inst_obj = Inst::While {
                    cond: CompExpr::parse_stmt(stmt.into_inner().next().unwrap()),
                    offset_to_end: 0,
                };
                waits_end_stack.push(WaitsEnd {
                    kind: inst_obj.clone(),
                    index: insts.len(),
                });
                insts.push(inst_obj);
            }
            Rule::Let => {
                let mut it = stmt.into_inner();
                let name = it.next().unwrap().as_str().to_owned();
                let init = Expr::parse_stmt(it.next().unwrap());
                let is_mut = it.next().is_some();

                insts.push(Inst::Let { name, init, is_mut });
            }
            Rule::Modify => {
                let mut it = stmt.into_inner();
                insts.push(Inst::Modify {
                    name: it.next().unwrap().as_str().to_owned(),
                    expr: Expr::parse_stmt(it.next().unwrap()),
                });
            }
            Rule::End => {
                let start = waits_end_stack.pop().unwrap_or_else(|| {
                    die!("Semantic error: a stray End detected.");
                });
                insts[start.index] = match start.kind {
                    Inst::Sub { ref name, .. } => Inst::Sub {
                        name: name.clone(),
                        offset_to_end: insts.len() - start.index,
                    },
                    Inst::While { ref cond, .. } => Inst::While {
                        cond: cond.clone(),
                        offset_to_end: insts.len() - start.index,
                    },
                    other => {
                        die!("cannot End {:?}", other);
                    }
                };
                insts.push(Inst::End);
            }
            Rule::EOI => break,
            other => {
                die!("Semantic error: unexpected rule : {:?}", other);
            }
        }
    }
    Some(Program { insts, subs })
}
