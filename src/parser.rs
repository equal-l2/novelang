use crate::exprs::*;
use pest::Parser;

#[derive(pest_derive::Parser)]
#[grammar = "prog.pest"]
struct ProgParser;

#[derive(Debug, Clone)]
pub enum Inst {
    Print {
        text: String,
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
    },
    LetMut {
        name: String,
        init: Expr,
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

pub fn parse(s: String) -> Option<Program> {
    let stmts = ProgParser::parse(Rule::Prog, &s);
    if let Err(e) = stmts {
        eprintln!("{}", e);
        return None;
    }
    let stmts = stmts.unwrap();

    let mut insts = vec![]; // statements parsed
    let mut waits_end_stack: Vec<WaitsEnd> = vec![]; // stmts waiting for End
    let mut subs = std::collections::HashMap::new(); // subroutines defined
    for stmt in stmts {
        match stmt.as_rule() {
            Rule::Print => insts.push(Inst::Print {
                text: stmt.into_inner().as_str().to_owned(),
            }),
            Rule::Sub => {
                // check if the Sub is nested (which is not allowed)
                if let Some(i) = waits_end_stack.last() {
                    if let Inst::Sub { .. } = i.kind {
                        eprintln!("Semantic error: you cannot nest Sub.");
                        std::process::exit(1);
                    }
                }

                // register the sub to the name table
                let fn_name = stmt.into_inner().as_str().to_owned();
                if subs.insert(fn_name.clone(), insts.len()).is_some() {
                    eprintln!(
                        "Semantic error: function name \"{}\" is conflicting",
                        fn_name
                    );
                    std::process::exit(1);
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
                insts.push(Inst::Let {
                    name: it.next().unwrap().as_str().to_owned(),
                    init: Expr::parse_stmt(it.next().unwrap()),
                });
            }
            Rule::LetMut => {
                let mut it = stmt.into_inner();
                insts.push(Inst::LetMut {
                    name: it.next().unwrap().as_str().to_owned(),
                    init: Expr::parse_stmt(it.next().unwrap()),
                });
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
                    eprintln!("Semantic error: a stray End detected.");
                    std::process::exit(1);
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
                        panic!("cannot End {:?}", other);
                    }
                };
                insts.push(Inst::End);
            }
            Rule::EOI => break,
            other => {
                panic!("Semantic error: unexpected rule : {:?}", other);
            }
        }
    }
    Some(Program { insts, subs })
}
