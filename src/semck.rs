use crate::lval::LVal;

mod exprs;
mod types;

#[macro_use]
mod utils;

use exprs::{Expr, TypeError};
use types::Type;
use utils::*;

#[derive(Debug)]
enum ParseError {
    TypeError(TypeError),
}

impl From<TypeError> for ParseError {
    fn from(e: TypeError) -> Self {
        Self::TypeError(e)
    }
}

#[derive(Debug, Clone)]
pub enum Statement {
    Print {
        args: Vec<Expr>,
    },
    Sub {
        name: String,
        offset_to_end: usize,
    },
    Call {
        name: String,
    },
    While {
        cond: Expr,
        offset_to_end: usize,
    },
    Let {
        name: String,
        init: Expr,
        is_mut: bool,
    },
    Modify {
        target: LVal,
        expr: Expr,
    },
    If {
        cond: Expr,
        offset_to_next: usize,
    },
    ElIf {
        cond: Expr,
        offset_to_next: usize,
    },
    Else {
        offset_to_end: usize,
    },
    End,
    Input {
        prompt: Option<String>,
        target: LVal,
        as_num: bool,
    },
    Roll {
        count: Expr,
        face: Expr,
        target: LVal,
    },
    Halt,
    Ill,
    Break,
    Assert {
        mesg: Expr,
        cond: Expr,
    },
    Continue,
    For {
        counter: String,
        from: Expr,
        to: Expr,
        offset_to_end: usize,
    },
    Return,
}

#[derive(Debug, Clone)]
pub struct AST {
    pub stmts: Vec<Statement>,
}

#[derive(Clone, Debug)]
struct TypeInfo {
    ty: Type,
    is_mut: bool,
}

type VarMap = std::collections::HashMap<String, TypeInfo>;

#[derive(Clone)]
enum ScopeKind {
    Loop,
    Sub,
    Other,
}

struct Scope {
    map: VarMap,
    kind: ScopeKind,
    ret_idx: usize,
}

impl Scope {
    fn new(kind: ScopeKind, ret_idx: usize) -> Self {
        Self {
            map: VarMap::new(),
            kind,
            ret_idx,
        }
    }

    fn add_var(&mut self, name: &str, var: TypeInfo) -> bool {
        let to_add = !self.map.contains_key(name);
        if to_add {
            self.map.insert(name.to_owned(), var);
        }

        to_add
    }

    fn get_type_info(&self, name: &str) -> Option<&TypeInfo> {
        self.map.get(name)
    }
}

struct ScopeStack {
    scopes: Vec<Scope>,
}

impl ScopeStack {
    fn new() -> Self {
        let mut internals = Scope::new(ScopeKind::Other, 0);
        internals.add_var(
            &String::from("_wait"),
            TypeInfo {
                ty: Type::Bool,
                is_mut: true,
            },
        );
        Self {
            scopes: vec![internals],
        }
    }

    fn kinds(&self) -> Vec<&ScopeKind> {
        self.scopes.iter().map(|sc| &sc.kind).rev().collect()
    }

    fn push(&mut self, kind: ScopeKind, ret_idx: usize) {
        self.scopes.push(Scope::new(kind, ret_idx));
    }

    fn pop(&mut self) -> Option<usize> {
        if self.scopes.len() > 1 {
            let sc = self.scopes.pop().unwrap();
            Some(sc.ret_idx)
        } else {
            // global scope cannot be pop
            None
        }
    }

    fn get_top_mut(&mut self) -> &mut Scope {
        self.scopes.last_mut().unwrap()
    }

    fn add_var(&mut self, name: &str, info: TypeInfo) -> bool {
        self.get_top_mut().add_var(name, info)
    }

    fn get_type_info(&self, lval: &LVal) -> Option<TypeInfo> {
        match lval {
            LVal::Scalar(s) => self
                .scopes
                .iter()
                .rev()
                .map(|m| m.get_type_info(s))
                .find(Option::is_some)
                .flatten()
                .cloned(),
            LVal::Vector(l, _) => {
                let v = self.get_type_info(l);
                match v {
                    Some(TypeInfo {
                        ty: Type::Arr(i),
                        is_mut,
                    }) => Some(TypeInfo {
                        ty: (*i).clone(),
                        is_mut,
                    }),
                    Some(_) => unreachable!(),
                    None => None,
                }
            }
        }
    }
}

pub fn check_semantics(parsed: crate::parse::Parsed) -> AST {
    use crate::parse::PreStmt;
    let mut stmts = Vec::<Statement>::new();
    let mut scope_stack = ScopeStack::new();

    for pre_s in parsed.stmts {
        let stmt = match pre_s {
            PreStmt::Print { args } => {
                for a in &args {
                    if Type::Sub == get_type(a, &scope_stack) {
                        die!("Value of type Sub cannot be printed");
                    }
                }
                Statement::Print { args: args.clone() }
            }
            PreStmt::Sub { name } => {
                // add this sub to var table
                let success = scope_stack.add_var(
                    &name,
                    TypeInfo {
                        ty: Type::Sub,
                        is_mut: false,
                    },
                );

                if !success {
                    die!("Conflicting subroutine name \"{}\"", name);
                }

                // create new scope
                scope_stack.push(ScopeKind::Sub, stmts.len());

                Statement::Sub {
                    name: name.clone(),
                    offset_to_end: 0,
                }
            }
            PreStmt::Call { name } => {
                let info = scope_stack.get_type_info(&LVal::Scalar(name.clone()));
                if info.is_none() || info.unwrap().ty != Type::Sub {
                    die!("Subroutine \"{}\" was not found", name);
                }

                Statement::Call { name: name.clone() }
            }
            PreStmt::While { cond } => {
                scope_stack.push(ScopeKind::Loop, stmts.len());
                let cond_ty = get_type(&cond, &scope_stack);
                if Type::Bool != cond_ty {
                    die!("Expected Bool, found {}", cond_ty);
                }

                Statement::While {
                    cond,
                    offset_to_end: 0,
                }
            }
            PreStmt::Let { name, init, is_mut } => {
                let init_ty = get_type(&init, &scope_stack);
                let success = scope_stack.add_var(
                    &name,
                    TypeInfo {
                        ty: init_ty,
                        is_mut,
                    },
                );

                if !success {
                    die!("Conflicting variable name \"{}\"", name);
                }

                Statement::Let {
                    name: name.clone(),
                    init,
                    is_mut,
                }
            }
            PreStmt::Modify { target, expr } => {
                let lval_tinfo = if let Some(info) = scope_stack.get_type_info(&target) {
                    if !info.is_mut {
                        die!("LVal \"{}\" is immutable", target);
                    }
                    info
                } else {
                    die!("LVal \"{}\" is invalid", target);
                };

                let expr_ty = get_type(&expr, &scope_stack);

                if lval_tinfo.ty != expr_ty {
                    die!("Type mismatch, expected {}, got {}", lval_tinfo.ty, expr_ty);
                }

                Statement::Modify { target, expr }
            }
            PreStmt::If { cond } => {
                scope_stack.push(ScopeKind::Other, stmts.len());

                Statement::If {
                    cond,
                    offset_to_next: 0,
                }
            }
            PreStmt::ElIf { cond } => {
                let prev_idx = scope_stack.pop().unwrap_or_else(|| {
                    die!("A stray Else-If detected");
                });

                let offset_to_next = stmts.len() - prev_idx;

                let prev = stmts[prev_idx].clone();
                stmts[prev_idx] = match prev {
                    Statement::If { cond, .. } => Statement::If {
                        cond: cond.clone(),
                        offset_to_next,
                    },
                    Statement::ElIf { cond, .. } => Statement::ElIf {
                        cond: cond.clone(),
                        offset_to_next,
                    },
                    _ => {
                        die!("Cannot find corresponding Element for Else-If");
                    }
                };

                scope_stack.push(ScopeKind::Other, stmts.len());
                Statement::ElIf {
                    cond,
                    offset_to_next: 0,
                }
            }
            PreStmt::Else => {
                let prev_idx = scope_stack.pop().unwrap_or_else(|| {
                    die!("A stray Else detected");
                });

                let offset_to_next = stmts.len() - prev_idx;

                let prev = stmts[prev_idx].clone();
                stmts[prev_idx] = match prev {
                    Statement::If { cond, .. } => Statement::If {
                        cond: cond.clone(),
                        offset_to_next,
                    },
                    Statement::ElIf { cond, .. } => Statement::ElIf {
                        cond: cond.clone(),
                        offset_to_next,
                    },
                    _ => {
                        die!("corresponding Element for Else is not found");
                    }
                };
                scope_stack.push(ScopeKind::Other, stmts.len());
                Statement::Else { offset_to_end: 0 }
            }
            PreStmt::End => {
                // Pop stack and assign end index
                let prev_idx = scope_stack.pop().unwrap_or_else(|| {
                    die!("A stray End detected.");
                });

                let offset_to_end = stmts.len() - prev_idx;

                let prev = stmts[prev_idx].clone();
                stmts[prev_idx] = match prev {
                    Statement::Sub { name, .. } => Statement::Sub {
                        name,
                        offset_to_end,
                    },
                    Statement::While { cond, .. } => Statement::While {
                        cond,
                        offset_to_end,
                    },
                    Statement::If { ref cond, .. } => Statement::If {
                        cond: cond.clone(),
                        offset_to_next: offset_to_end,
                    },
                    Statement::ElIf { ref cond, .. } => Statement::ElIf {
                        cond: cond.clone(),
                        offset_to_next: offset_to_end,
                    },
                    Statement::Else { .. } => Statement::Else { offset_to_end },
                    Statement::For {
                        counter, from, to, ..
                    } => Statement::For {
                        counter,
                        from,
                        to,
                        offset_to_end,
                    },
                    _ => {
                        die!("Cannot find corresponding Element for End");
                    }
                };

                Statement::End
            }
            PreStmt::Input { prompt, target } => {
                let as_num = if let Some(info) = scope_stack.get_type_info(&target) {
                    if !info.is_mut {
                        die!("LVal \"{}\" is immutable", target);
                    }
                    match info.ty {
                        Type::Num => true,
                        Type::Str => false,
                        _ => die!("Expected Num or Str, found {}", info.ty),
                    }
                } else {
                    die!("LVal \"{}\" is invalid", target);
                };

                Statement::Input {
                    prompt,
                    target,
                    as_num,
                }
            }
            PreStmt::Roll {
                count,
                face,
                target,
            } => {
                let count_ty = get_type(&count, &scope_stack);
                if count_ty != Type::Num {
                    die!("Expected Num, found {}", count_ty);
                }

                let face_ty = get_type(&face, &scope_stack);
                if face_ty != Type::Num {
                    die!("Expected Num, found {}", face_ty);
                }

                if let Some(info) = scope_stack.get_type_info(&target) {
                    if info.ty != Type::Num {
                        die!("Expected Num, found {}", info.ty);
                    }
                    if !info.is_mut {
                        die!("LVal \"{}\" is immutable", target);
                    }
                } else {
                    die!("LVal \"{}\" is invalid", target);
                };

                Statement::Roll {
                    count,
                    face,
                    target,
                }
            }
            PreStmt::Halt => Statement::Halt,
            PreStmt::Ill => Statement::Ill,
            PreStmt::Break => {
                let mut loop_found = false;
                for k in scope_stack.kinds() {
                    match k {
                        ScopeKind::Loop => {
                            loop_found = true;
                            break;
                        }
                        ScopeKind::Sub => break,
                        _ => {}
                    }
                }

                if loop_found {
                    Statement::Break
                } else {
                    die!("Break must be in a loop");
                }
            }
            PreStmt::Assert { mesg, cond } => {
                let mesg_ty = get_type(&mesg, &scope_stack);
                if mesg_ty != Type::Str {
                    die!("Expected Str");
                }

                let cond_ty = get_type(&cond, &scope_stack);
                if cond_ty != Type::Bool {
                    die!("Expected Bool");
                }

                Statement::Assert { mesg, cond }
            }
            PreStmt::Continue => {
                // "Continue" ";"
                let mut loop_found = false;
                for k in scope_stack.kinds() {
                    match k {
                        ScopeKind::Loop => {
                            loop_found = true;
                            break;
                        }
                        ScopeKind::Sub => break,
                        _ => {}
                    }
                }

                if loop_found {
                    Statement::Continue
                } else {
                    die!("Break must be in a loop");
                }
            }
            PreStmt::For { counter, from, to } => {
                // "For" <name> "from" <expr> "to" <expr> ";"
                let from_ty = get_type(&from, &scope_stack);
                if from_ty != Type::Num {
                    die!("Expected Num, found {}", from_ty);
                }

                let to_ty = get_type(&to, &scope_stack);
                if to_ty != Type::Num {
                    die!("Expected Num, found {}", to_ty);
                }

                scope_stack.push(ScopeKind::Loop, stmts.len());

                // always successful because we are pushing it to a new scope
                let _ = scope_stack.add_var(
                    &counter,
                    TypeInfo {
                        ty: Type::Num,
                        is_mut: false,
                    },
                );

                Statement::For {
                    counter: counter.clone(),
                    from,
                    to,
                    offset_to_end: 0,
                }
            }
            PreStmt::Return => {
                // "Return" ";"
                let mut sub_found = false;
                for k in scope_stack.kinds() {
                    if let ScopeKind::Sub = k {
                        sub_found = true;
                        break;
                    }
                }

                if sub_found {
                    Statement::Return
                } else {
                    die!("Return must be in a sub");
                }
            }
        };
        stmts.push(stmt);
    }

    AST { stmts }
}