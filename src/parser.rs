use pest::Parser;
use super::Program;
use super::StmtType;

#[derive(pest_derive::Parser)]
#[grammar = "prog.pest"]
struct ProgParser;

pub fn parse(s: String) -> Option<Program> {
    let stmts = ProgParser::parse(Rule::Prog, &s);
    if let Err(e) = stmts {
        eprintln!("{}", e);
        return None;
    }
    let stmts = stmts.unwrap();

    let mut stmt_list = vec![];
    let mut fns = std::collections::HashMap::new();
    let mut fn_start = None;
    for stmt in stmts {
        match stmt.as_rule() {
            Rule::Print => stmt_list.push(StmtType::Print {
                text: stmt.into_inner().as_str().to_owned(),
            }),
            Rule::FnBegin => {
                if fn_start.is_some() {
                    eprintln!("Semantic error: you cannot nest FnBegin.");
                    std::process::exit(1);
                }
                let fn_name = stmt.into_inner().as_str().to_owned();
                fn_start = Some(stmt_list.len());
                let old = fns.insert(fn_name.clone(), stmt_list.len());
                if old.is_some() {
                    eprintln!("Semantic error: function name \"{}\" is conflicting", fn_name);
                    std::process::exit(1);
                }
                stmt_list.push(StmtType::FnBegin {
                    name: fn_name,
                    offset_to_end: 0,
                });
            }
            Rule::FnEnd => {
                if fn_start.is_none() {
                    eprintln!("Semantic error: a stray FnEnd detected.");
                    std::process::exit(1);
                }
                let start = fn_start.take().unwrap();
                if let StmtType::FnBegin { ref name, .. } = stmt_list[start] {
                    stmt_list[start] = StmtType::FnBegin {
                        name: name.clone(),
                        offset_to_end: stmt_list.len() - start,
                    };
                } else {
                    unreachable!();
                }
                stmt_list.push(StmtType::FnEnd);
            }
            Rule::Call => stmt_list.push(StmtType::Call {
                name: stmt.into_inner().as_str().to_owned(),
            }),
            Rule::EOI => break,
            other => {
                panic!("unexpected rule : {:?}", other);
            }
        }
    }
    Some(Program {
        stmts: stmt_list,
        fns,
    })
}
