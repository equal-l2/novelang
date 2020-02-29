use pest::Parser;
use crate::exprs::*;

#[derive(pest_derive::Parser)]
#[grammar = "prog.pest"]
struct ProgParser;

#[derive(Debug, Clone)]
pub enum Inst {
    Print { text: String },
    Sub { name: String, offset_to_end: usize },
    Call { name: String },
    While { cond: CompExpr, offset_to_end: usize },
    Let { name: String, init: Expr },
    LetMut { name: String, init: Expr },
    Modify { name: String, expr: Expr },
    End,
}

#[derive(Debug, Clone)]
pub struct Program {
    pub stmts: Vec<Inst>,
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

    let mut stmt_list = vec![]; // statements parsed
    let mut waits_end_stack: Vec<WaitsEnd> = vec![]; // stmts waiting for End
    let mut subs = std::collections::HashMap::new(); // subroutines defined
    for stmt in stmts {
        match stmt.as_rule() {
            Rule::Print => stmt_list.push(Inst::Print {
                text: stmt.into_inner().as_str().to_owned(),
            }),
            Rule::Sub => {
                if let Some(i) = waits_end_stack.last() {
                    if let Inst::Sub { .. } = i.kind {
                        eprintln!("Semantic error: you cannot nest Sub.");
                        std::process::exit(1);
                    }
                }
                let fn_name = stmt.into_inner().as_str().to_owned();
                let old = subs.insert(fn_name.clone(), stmt_list.len());
                if old.is_some() {
                    eprintln!(
                        "Semantic error: function name \"{}\" is conflicting",
                        fn_name
                    );
                    std::process::exit(1);
                }
                let stmt_obj = Inst::Sub {
                    name: fn_name,
                    offset_to_end: 0,
                };
                waits_end_stack.push(WaitsEnd {
                    kind: stmt_obj.clone(),
                    index: stmt_list.len(),
                });
                stmt_list.push(stmt_obj);
            }
            Rule::Call => stmt_list.push(Inst::Call {
                name: stmt.into_inner().as_str().to_owned(),
            }),
            Rule::While => {
                let stmt_obj = Inst::While {
                    cond: CompExpr::parse_stmt(stmt.into_inner().next().unwrap()),
                    offset_to_end : 0
                };
                waits_end_stack.push(WaitsEnd {
                    kind: stmt_obj.clone(),
                    index: stmt_list.len(),
                });
                stmt_list.push(stmt_obj);
            }
            Rule::End => {
                let start = waits_end_stack.pop().unwrap_or_else(|| {
                    eprintln!("Semantic error: a stray End detected.");
                    std::process::exit(1);
                });
                stmt_list[start.index] = match start.kind {
                    Inst::Sub { ref name, .. } => Inst::Sub {
                        name: name.clone(),
                        offset_to_end: stmt_list.len() - start.index,
                    },
                    Inst::While { ref cond, .. } => Inst::While {
                        cond: cond.clone(),
                        offset_to_end: stmt_list.len() - start.index,
                    },
                    other => {
                        panic!("cannot End {:?}", other);
                    }
                };
                stmt_list.push(Inst::End);
            }
            Rule::EOI => break,
            other => {
                panic!("unexpected rule : {:?}", other);
            }
        }
    }
    Some(Program {
        stmts: stmt_list,
        subs,
    })
}
