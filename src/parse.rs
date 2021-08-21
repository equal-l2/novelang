use crate::exprs::Expr;
use crate::lex;

mod expr_type;
mod exprs;
mod types;

#[macro_use]
mod utils;

use expr_type::{TypeCheck, TypeError};
use types::Type;
use utils::*;

enum ParseError {
    UnexpectedToken(lex::Token),
    EmptyExpr,
    NoPairParen { lparen: lex::Token },
    TokenExhausted,
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
        name: String,
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
        name: String,
        as_num: bool,
    },
    Roll {
        count: Expr,
        face: Expr,
        name: String,
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

#[derive(Debug)]
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

    fn add_var(&mut self, name: String, var: TypeInfo) -> bool {
        let exists = self.map.contains_key(&name);
        if !exists {
            // variable with the same name already exists
            self.map.insert(name, var);
        }

        !exists
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
            String::from("_wait"),
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
        self.scopes.push(Scope::new(kind, ret_idx))
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

    fn add_var(&mut self, name: String, info: TypeInfo) -> bool {
        self.get_top_mut().add_var(name, info)
    }

    fn get_type_info(&self, name: &str) -> Option<&TypeInfo> {
        self.scopes
            .iter()
            .rev()
            .map(|m| m.get_type_info(name))
            .find(Option::is_some)
            .flatten()
    }
}

#[derive(Clone, Debug)]
enum LVal {
    Scalar(String),
    Vector(Box<Self>, Expr),
}

pub fn parse(lexed: crate::lex::Lexed) -> AST {
    use lex::{Items, Keyword};

    let mut stmts = vec![Statement::Ill];
    let mut scope_stack = ScopeStack::new();

    let tks = &lexed.tokens;

    let mut i = 0;
    while i < tks.len() {
        if let Items::Cmd(inst) = &tks[i].item {
            match inst {
                lex::Command::Print => parse_stmt!(i, stmts, {
                    // "Print" (<expr> {"," <expr>}) ";"
                    let mut args = Vec::new();

                    {
                        let expr = parse_expr(&mut i, &lexed, &mut scope_stack);

                        match expr.check_type(&scope_stack) {
                            Ok(ty) => {
                                if ty == Type::Sub {
                                    die_cont!("Value of type Sub cannot be printed", i, lexed)
                                }
                                args.push(expr);
                            }
                            Err(e) => die_by_expr_parse_error(e.into(), i, &lexed),
                        }
                    }

                    while i < tks.len() {
                        match &tks[i].item {
                            Items::Semi => break,
                            Items::Comma => {
                                i += 1;
                                if matches!(tks[i].item, Items::Semi) {
                                    break;
                                } else {
                                    let expr = parse_expr(&mut i, &lexed, &mut scope_stack);

                                    match expr.check_type(&scope_stack) {
                                        Ok(ty) => {
                                            if ty == Type::Sub {
                                                die_cont!(
                                                    "Value of type Sub cannot be printed",
                                                    i,
                                                    lexed
                                                )
                                            }
                                            args.push(expr);
                                        }
                                        Err(e) => die_by_expr_parse_error(e.into(), i, &lexed),
                                    }
                                }
                            }
                            _ => {
                                die_cont!("Unexpected token", i, lexed)
                            }
                        }
                    }
                    expects_semi!(i, lexed);
                    Statement::Print { args }
                }),

                lex::Command::Sub => parse_stmt!(i, stmts, {
                    // "Sub" <name> ";"

                    if let Items::Ident(name) = &tks[i].item {
                        i += 1;
                        expects_semi!(i, lexed);

                        // add this sub to var table
                        let success = scope_stack.add_var(
                            name.clone(),
                            TypeInfo {
                                ty: Type::Sub,
                                is_mut: false,
                            },
                        );

                        if !success {
                            die_cont!("Conflicting subroutine name", i, lexed);
                        }

                        // create new scope
                        scope_stack.push(ScopeKind::Sub, stmts.len());

                        Statement::Sub {
                            name: name.clone(),
                            offset_to_end: 0,
                        }
                    } else {
                        die_cont!("Expected subroutine name", i, lexed)
                    }
                }),

                lex::Command::Call => parse_stmt!(i, stmts, {
                    // "Call" <name> ";"
                    if let Items::Ident(name) = &tks[i].item {
                        i += 1;
                        expects_semi!(i, lexed);

                        let info = scope_stack.get_type_info(name);
                        if info.is_none() || info.unwrap().ty != Type::Sub {
                            die_cont!(format!("Subroutine \"{}\" was not found", name), i, lexed);
                        }

                        Statement::Call { name: name.clone() }
                    } else {
                        die_cont!("Expected subroutine name", i, lexed)
                    }
                }),

                lex::Command::While => parse_stmt!(i, stmts, {
                    // "While" <cond> ";"

                    scope_stack.push(ScopeKind::Loop, stmts.len());

                    let expr = parse_expr(&mut i, &lexed, &mut scope_stack);
                    expects_type!(expr, Type::Bool, scope_stack, i, lexed);

                    expects_semi!(i, lexed);

                    Statement::While {
                        cond: expr,
                        offset_to_end: 0,
                    }
                }),

                lex::Command::Let => parse_stmt!(i, stmts, {
                    // "Let" <name> "Be" <expr> ("AsMut") ";"

                    if let Items::Ident(name) = &tks[i].item {
                        i += 1;
                        if name.starts_with('_') {
                            die_cont!("Identifier starts with _ is reserved", i, lexed);
                        }
                        expects!("\"Be\" expected", Items::Key(Keyword::Be), i, lexed);

                        let init = parse_expr(&mut i, &lexed, &mut scope_stack);

                        let init_ty = match init.check_type(&scope_stack) {
                            Ok(t) => t,
                            Err(e) => die_by_expr_parse_error(e.into(), i, &lexed),
                        };

                        expects!(
                            "\"AsMut\" or semicolon expected",
                            Items::Semi | Items::Key(Keyword::AsMut),
                            i,
                            lexed
                        );

                        let is_mut = {
                            if tks[i - 1].item == Items::Key(Keyword::AsMut) {
                                expects_semi!(i, lexed);
                                true
                            } else {
                                false
                            }
                        };

                        let success = scope_stack.add_var(
                            name.clone(),
                            TypeInfo {
                                ty: init_ty,
                                is_mut,
                            },
                        );

                        if !success {
                            die_cont!("Conflicting variable name", i, lexed);
                        }

                        Statement::Let {
                            name: name.clone(),
                            init,
                            is_mut,
                        }
                    } else {
                        die_cont!("Ident expected", i, lexed)
                    }
                }),

                lex::Command::Modify => parse_stmt!(i, stmts, {
                    // "Modify" <lval> "To" <expr> ";"

                    if let Items::Ident(name) = &tks[i].item {
                        i += 1;

                        expects!("To expected", Items::Key(Keyword::To), i, lexed);

                        let expr = parse_expr(&mut i, &lexed, &mut scope_stack);
                        expects_semi!(i, lexed);

                        let var_tinfo = scope_stack.get_type_info(name);
                        if let Some(info) = var_tinfo {
                            // TODO: better error message (maybe)
                            let expr_ty = match expr.check_type(&scope_stack) {
                                Ok(t) => t,
                                Err(e) => die_by_expr_parse_error(e.into(), i, &lexed),
                            };

                            if info.ty != expr_ty {
                                die_cont!("Type mismatch", i, lexed);
                            }

                            if !info.is_mut {
                                die_cont!("Variable is immutable", i, lexed);
                            }
                        } else {
                            die_cont!(format!("Variable \"{}\" was not found", name), i, lexed);
                        }

                        Statement::Modify {
                            name: name.clone(),
                            expr,
                        }
                    } else {
                        die_cont!("Ident expected", i, lexed);
                    }
                }),

                lex::Command::If => parse_stmt!(i, stmts, {
                    // "If" <cond> ";"

                    let cond = parse_expr(&mut i, &lexed, &mut scope_stack);
                    expects_semi!(i, lexed);

                    scope_stack.push(ScopeKind::Other, stmts.len());

                    Statement::If {
                        cond,
                        offset_to_next: 0,
                    }
                }),

                lex::Command::Else => parse_stmt!(i, stmts, {
                    // "Else" ("If" <cond>) ";"

                    let inst_obj = if let Items::Cmd(lex::Command::If) = &tks[i].item {
                        // "Else" "If" <cond> ";"
                        i += 1;

                        let prev_idx = scope_stack.pop().unwrap_or_else(|| {
                            die_cont!("A stray Else-If detected.", i, lexed);
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
                                die_cont!(
                                    "Cannot find corresponding Element for Else-If",
                                    i,
                                    lexed
                                );
                            }
                        };

                        let cond = parse_expr(&mut i, &lexed, &mut scope_stack);
                        expects_semi!(i, lexed);

                        Statement::ElIf {
                            cond,
                            offset_to_next: 0,
                        }
                    } else {
                        // "Else" ";"
                        expects_semi!(i, lexed);

                        let prev_idx = scope_stack.pop().unwrap_or_else(|| {
                            die_cont!("A stray Else detected.", i, lexed);
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
                                die_cont!("Cannot find corresponding Element for Else", i, lexed);
                            }
                        };
                        Statement::Else { offset_to_end: 0 }
                    };

                    scope_stack.push(ScopeKind::Other, stmts.len());

                    inst_obj
                }),

                lex::Command::End => parse_stmt!(i, stmts, {
                    // "End" ";"
                    expects_semi!(i, lexed);

                    // Pop stack and assign end index
                    let prev_idx = scope_stack.pop().unwrap_or_else(|| {
                        die_cont!("A stray End detected.", i, lexed);
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
                            die_cont!("Cannot find corresponding Element for End", i, lexed);
                        }
                    };

                    Statement::End
                }),

                lex::Command::Input => parse_stmt!(i, stmts, {
                    // "Input" (<str>) "To" <lval> ";"

                    let prompt = if let Items::Str(prompt) = &tks[i].item {
                        i += 1;
                        Some(prompt.clone())
                    } else {
                        None
                    };

                    expects!("\"To\" expected", Items::Key(Keyword::To), i, lexed);

                    let name = if let Items::Ident(n) = &tks[i].item {
                        i += 1;
                        n.clone()
                    } else {
                        die_cont!("Ident expected", i, lexed)
                    };

                    let as_num = if let Some(info) = scope_stack.get_type_info(&name) {
                        if !info.is_mut {
                            die_cont!("Variable is immutable", i, lexed);
                        }
                        match info.ty {
                            Type::Num => true,
                            Type::Str => false,
                            _ => die_cont!("Expected Num or Str", i, lexed),
                        }
                    } else {
                        die_cont!(format!("Variable \"{}\" was not found", name), i, lexed)
                    };

                    expects_semi!(i, lexed);
                    Statement::Input {
                        prompt,
                        name,
                        as_num,
                    }
                }),

                lex::Command::Roll => parse_stmt!(i, stmts, {
                    // "Roll" <expr> "Dice" "With" <expr> "Face" "To" <lval> ";"

                    let count = parse_expr(&mut i, &lexed, &mut scope_stack);

                    let count_ty = match count.check_type(&scope_stack) {
                        Ok(t) => t,
                        Err(e) => die_by_expr_parse_error(e.into(), i, &lexed),
                    };

                    if count_ty != Type::Num {
                        die_cont!("Expected Num Expr", i, lexed);
                    }

                    expects!("\"Dice\" expected", Items::Key(Keyword::Dice), i, lexed);

                    expects!("\"With\" expected", Items::Key(Keyword::With), i, lexed);

                    let face = parse_expr(&mut i, &lexed, &mut scope_stack);

                    let face_ty = match count.check_type(&scope_stack) {
                        Ok(t) => t,
                        Err(e) => die_by_expr_parse_error(e.into(), i, &lexed),
                    };

                    if face_ty != Type::Num {
                        die_cont!("Expected Num Expr", i, lexed);
                    }

                    expects!("\"Face\" expected", Items::Key(Keyword::Face), i, lexed);

                    expects!("\"To\" expected", Items::Key(Keyword::To), i, lexed);

                    let name = if let Items::Ident(n) = &tks[i].item {
                        i += 1;
                        n.clone()
                    } else {
                        die_cont!("Ident expected", i, lexed)
                    };

                    if let Some(info) = scope_stack.get_type_info(&name) {
                        if !matches!(info.ty, Type::Num) {
                            die_cont!("Expected Num", i, lexed)
                        }
                        if !info.is_mut {
                            die_cont!("Variable is immutable", i, lexed);
                        }
                    } else {
                        die_cont!(format!("Variable \"{}\" was not found", name), i, lexed)
                    };

                    expects_semi!(i, lexed);
                    Statement::Roll { count, face, name }
                }),

                lex::Command::Halt => parse_stmt!(i, stmts, {
                    // "Halt" ";"
                    expects_semi!(i, lexed);
                    Statement::Halt
                }),

                lex::Command::Break => parse_stmt!(i, stmts, {
                    // "Break" ";"
                    expects_semi!(i, lexed);

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
                        die_cont!("Break must be in a loop", i, lexed);
                    }
                }),

                lex::Command::Assert => parse_stmt!(i, stmts, {
                    // "Assert" (<str> "With") <cond> ";"
                    let expr1 = parse_expr(&mut i, &lexed, &mut scope_stack);

                    let expr1_ty = match expr1.check_type(&scope_stack) {
                        Ok(t) => t,
                        Err(e) => die_by_expr_parse_error(e.into(), i, &lexed),
                    };

                    let mesg;
                    let cond;
                    match expr1_ty {
                        Type::Str => {
                            // "Assert" <str> "With" <cond> ";"
                            mesg = expr1;
                            expects!(
                                "\"With\" expected, as the first expression was Str",
                                Items::Key(Keyword::With),
                                i,
                                lexed
                            );

                            let expr2 = parse_expr(&mut i, &lexed, &mut scope_stack);
                            let expr2_ty = match expr2.check_type(&scope_stack) {
                                Ok(t) => t,
                                Err(e) => die_by_expr_parse_error(e.into(), i, &lexed),
                            };

                            if expr2_ty != Type::Bool {
                                die_cont!("Expected Bool", i, lexed);
                            }

                            cond = expr2;
                        }
                        Type::Bool => {
                            mesg = Expr::new_str(expr1.to_string());
                            cond = expr1;
                        }
                        _ => die_cont!("Expected Str or Bool", i, lexed),
                    }

                    expects_semi!(i, lexed);

                    Statement::Assert { mesg, cond }
                }),

                lex::Command::Continue => parse_stmt!(i, stmts, {
                    // "Continue" ";"
                    expects_semi!(i, lexed);
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
                        die_cont!("Break must be in a loop", i, lexed);
                    }
                }),

                lex::Command::For => parse_stmt!(i, stmts, {
                    // "For" <name> "from" <expr> "to" <expr> ";"

                    if let Items::Ident(name) = &tks[i].item {
                        i += 1;
                        if name.starts_with('_') {
                            die_cont!("Identifier starts with _ is reserved", i, lexed);
                        }
                        expects!("\"From\" expected", Items::Key(Keyword::From), i, lexed);

                        let from = parse_expr(&mut i, &lexed, &mut scope_stack);

                        {
                            let from_ty = match from.check_type(&scope_stack) {
                                Ok(t) => t,
                                Err(e) => die_by_expr_parse_error(e.into(), i, &lexed),
                            };

                            if from_ty != Type::Num {
                                die_cont!("Expected Str or Num", i, lexed);
                            }
                        }

                        expects!("\"To\" expected", Items::Key(Keyword::To), i, lexed);

                        let to = parse_expr(&mut i, &lexed, &mut scope_stack);

                        {
                            let to_ty = match to.check_type(&scope_stack) {
                                Ok(t) => t,
                                Err(e) => die_by_expr_parse_error(e.into(), i, &lexed),
                            };

                            if to_ty != Type::Num {
                                die_cont!("Expected Str or Num", i, lexed);
                            }
                        }

                        scope_stack.push(ScopeKind::Loop, stmts.len());

                        // always successful because we are pushing it to a new scope
                        let _ = scope_stack.add_var(
                            name.clone(),
                            TypeInfo {
                                ty: Type::Num,
                                is_mut: false,
                            },
                        );

                        expects_semi!(i, lexed);

                        Statement::For {
                            counter: name.clone(),
                            from,
                            to,
                            offset_to_end: 0,
                        }
                    } else {
                        die_cont!("Ident expected", i, lexed)
                    }
                }),
                lex::Command::Return => parse_stmt!(i, stmts, {
                    // "Return" ";"
                    expects_semi!(i, lexed);
                    let mut sub_found = false;
                    for k in scope_stack.kinds() {
                        match k {
                            ScopeKind::Sub => {
                                sub_found = true;
                                break;
                            }
                            _ => {}
                        }
                    }

                    if sub_found {
                        Statement::Return
                    } else {
                        die_cont!("Return must be in a sub", i, lexed);
                    }
                }),
            }
        } else {
            die_cont!("Line must begin with Command", i, lexed);
        }
    }
    AST { stmts }
}
