use crate::die;
use crate::exprs::Expr;
use crate::lex;

mod exprs;
mod type_check;

use exprs::TryFromTokens;
use type_check::{TypeCheck, TypeError};

enum ParseError {
    InvalidToken(lex::Token),
    EmptyExpr,
    NoPairParen { lparen: lex::Token },
    TrailingToken { from: lex::Token },
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
}

#[derive(Debug, Clone)]
pub struct AST {
    pub stmts: Vec<Statement>,
}

macro_rules! die_cont {
    ($msg: expr, $i: expr, $lexed: ident) => {
        die!(
            "Error: {}\n{}",
            $msg,
            $lexed.generate_loc_info(&$lexed.tokens[$i].loc)
        )
    };
}

// expects!("message here", SomeItem | AnotherItem, i, lexed);
macro_rules! expects {
    ($msg: expr, $($pat: pat)|+, $i: ident, $lexed: ident) => {
        {
            if $lexed.tokens.len() <= $i {
                // tokens has been exhausted
                let last_token = &$lexed.tokens.last().unwrap();
                die!(
                    "Error: {}\n{}",
                    $msg,
                    $lexed.generate_loc_info(&last_token.next_col_loc())
                );
            } else if !matches!(&$lexed.tokens[$i].item, $($pat)|+) {
                die_cont!($msg, $i, $lexed);
            }
            $i += 1;
        }
    }
}

// expects_semi!(i, lexed);
macro_rules! expects_semi {
    ($i: ident, $lexed: ident) => {
        expects!("Semicolon expected", Items::Semi, $i, $lexed);
    };
}

fn parse_expr_from_tokens(tks: &[lex::Token], stack: &ScopeStack) -> Result<Expr, ParseError> {
    if tks.is_empty() {
        return Err(ParseError::EmptyExpr);
    }

    let expr = Expr::try_from_tokens(&mut tks.iter().peekable())?;

    let _ = expr.check_type(stack)?;

    Ok(expr)
}

fn die_by_expr_parse_error(e: ParseError, i: usize, lexed: &lex::Lexed) -> ! {
    match e {
        ParseError::EmptyExpr => {
            die_cont!("Expr is empty", i, lexed);
        }
        ParseError::InvalidToken(tk) => {
            die!(
                "Error: {}\n{}",
                "Failed to parse expr because of this token",
                lexed.generate_loc_info(&tk.loc)
            );
        }
        ParseError::NoPairParen { lparen: tk } => {
            die!(
                "Error: {}\n{}",
                "Paren doesn't have its pair",
                lexed.generate_loc_info(&tk.loc)
            );
        }
        ParseError::TrailingToken { from: tk } => {
            die!(
                "Error: {}\n{}",
                "Trailing token from here",
                lexed.generate_loc_info(&tk.loc)
            );
        }
        ParseError::TokenExhausted => {
            die_cont!("Expression abruptly ended", i, lexed);
        }
        ParseError::TypeError(te) => match te {
            TypeError::VarNotFound(name) => {
                die_cont!(format!("Variable {} was not found", name), i, lexed);
            }
            TypeError::UnaryUndefined(ty) => {
                //TODO: show operator (such as '<=')
                die_cont!(
                    format!("Unary operator is not defined for {}", ty),
                    i,
                    lexed
                );
            }
            TypeError::BinaryUndefined(l, r) => {
                //TODO: show operator (such as '-' or '+')
                die_cont!(
                    format!("Unary operator is not defined for {} and {}", l, r),
                    i,
                    lexed
                );
            }
        },
    }
}

// parse tokens into expression
// parse_expr!(EndItemOfExpr | AnotherEndOfExpr, i, tks, lexed)
macro_rules! parse_expr {
    ($($end_pat: pat)|+, $i: ident, $tks: ident, $lexed: ident, $stack: ident) => {
        {
            let mut j = $i;
            while j < $tks.len()
                && !matches!(
                    $tks[j].item,
                    $($end_pat)|+
                )
            {
                j += 1;
            }
            let expr = parse_expr_from_tokens(&$tks[$i..j], &$stack).unwrap_or_else(
                |e| die_by_expr_parse_error(e, $i, &$lexed)
            );
            $i = j;
            expr
        }
    }
}

macro_rules! parse_stmt {
    ($i: ident, $stmts: ident, $proc: block) => {{
        $i += 1;
        let inst_obj = $proc;
        $stmts.push(inst_obj);
    }};
}

macro_rules! expects_type {
    ($expr: ident, $ty: path, $stack: ident, $i: ident, $lexed: ident) => {
        match $expr.check_type(&$stack) {
            Ok(t) => {
                if t != $ty {
                    die_cont!(format!("Expected {}, found {}", $ty, t), $i, $lexed)
                }
            }
            Err(e) => {
                die_by_expr_parse_error(e.into(), $i, &$lexed);
            }
        }
    };
}

#[derive(Clone, Debug, PartialEq, Eq)]
enum Type {
    Bool,
    Num,
    Str,
    Sub,
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "{}", self.typename())
    }
}

impl Type {
    const fn typename(&self) -> &str {
        match self {
            Self::Bool => "Bool",
            Self::Num => "Num",
            Self::Str => "Str",
            Self::Sub => "Sub",
        }
    }
}

#[derive(Debug)]
struct TypeInfo {
    ty: Type,
    is_mut: bool,
}

type VarMap = std::collections::HashMap<String, TypeInfo>;

struct Scope {
    map: VarMap,
    ret_idx: usize,
}

impl Scope {
    fn new(ret_idx: usize) -> Self {
        Self {
            map: VarMap::new(),
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
        let mut internals = Scope::new(0);
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

    fn push(&mut self, ret_idx: usize) {
        self.scopes.push(Scope::new(ret_idx))
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

pub fn parse(lexed: crate::lex::Lexed) -> AST {
    use lex::{Items, Keywords};

    let mut stmts = vec![Statement::Ill];
    let mut scope_stack = ScopeStack::new();

    let tks = &lexed.tokens;

    let mut i = 0;
    while i < tks.len() {
        if let Items::Cmd(inst) = &tks[i].item {
            match inst {
                lex::Command::Print => parse_stmt!(i, stmts, {
                    // "Print" (expr {"," expr}) ";"
                    let mut args = Vec::new();
                    while i < tks.len() {
                        match &tks[i].item {
                            Items::Semi => break,
                            Items::Comma => {
                                i += 1;
                            }
                            _ => {
                                let expr = parse_expr!(
                                    Items::Comma | Items::Semi,
                                    i,
                                    tks,
                                    lexed,
                                    scope_stack
                                );

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
                    }
                    expects_semi!(i, lexed);
                    Statement::Print { args }
                }),

                lex::Command::Sub => parse_stmt!(i, stmts, {
                    // "Sub" name ";"

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
                        scope_stack.push(stmts.len());

                        Statement::Sub {
                            name: name.clone(),
                            offset_to_end: 0,
                        }
                    } else {
                        die_cont!("Expected subroutine name", i, lexed)
                    }
                }),

                lex::Command::Call => parse_stmt!(i, stmts, {
                    // "Call" name ";"
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
                    // "While" cond ";"

                    scope_stack.push(stmts.len());

                    let expr = parse_expr!(Items::Semi, i, tks, lexed, scope_stack);
                    expects_type!(expr, Type::Bool, scope_stack, i, lexed);

                    expects_semi!(i, lexed);

                    Statement::While {
                        cond: expr,
                        offset_to_end: 0,
                    }
                }),

                lex::Command::Let => parse_stmt!(i, stmts, {
                    // "Let" name "Be" expr ("AsMut") ";"

                    if let Items::Ident(name) = &tks[i].item {
                        i += 1;
                        if name.starts_with('_') {
                            die_cont!("Identifier starts with _ is reserved", i, lexed);
                        }
                        expects!("\"Be\" expected", Items::Key(Keywords::Be), i, lexed);

                        let init = parse_expr!(
                            Items::Semi | Items::Key(Keywords::AsMut),
                            i,
                            tks,
                            lexed,
                            scope_stack
                        );

                        let init_ty = match init.check_type(&scope_stack) {
                            Ok(t) => t,
                            Err(e) => die_by_expr_parse_error(e.into(), i, &lexed),
                        };

                        expects!(
                            "\"AsMut\" or semicolon expected",
                            Items::Semi | Items::Key(Keywords::AsMut),
                            i,
                            lexed
                        );

                        let is_mut = {
                            if tks[i - 1].item == Items::Key(Keywords::AsMut) {
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
                    // "Modify" name "To" expr ";"

                    if let Items::Ident(name) = &tks[i].item {
                        i += 1;

                        expects!("To expected", Items::Key(Keywords::To), i, lexed);

                        let expr = parse_expr!(Items::Semi, i, tks, lexed, scope_stack);
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
                    // "If" cond ";"

                    let cond = parse_expr!(Items::Semi, i, tks, lexed, scope_stack);
                    expects_semi!(i, lexed);

                    scope_stack.push(stmts.len());

                    Statement::If {
                        cond,
                        offset_to_next: 0,
                    }
                }),

                lex::Command::Else => parse_stmt!(i, stmts, {
                    // "Else" ("If" cond) ";"

                    let inst_obj = if let Items::Cmd(lex::Command::If) = &tks[i].item {
                        // "Else" "If" cond ";"
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

                        let cond = parse_expr!(Items::Semi, i, tks, lexed, scope_stack);
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

                    scope_stack.push(stmts.len());

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
                        _ => {
                            die_cont!("Cannot find corresponding Element for End", i, lexed);
                        }
                    };

                    Statement::End
                }),

                lex::Command::Input => parse_stmt!(i, stmts, {
                    // "Input" (prompt) "To" name ";"

                    let prompt = if let Items::Str(prompt) = &tks[i].item {
                        i += 1;
                        Some(prompt.clone())
                    } else {
                        None
                    };

                    expects!("\"To\" expected", Items::Key(Keywords::To), i, lexed);

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
                    // "Roll" n "Dice" "With" k "Face" "To" name ";"

                    let count = parse_expr!(Items::Key(Keywords::Dice), i, tks, lexed, scope_stack);

                    let count_ty = match count.check_type(&scope_stack) {
                        Ok(t) => t,
                        Err(e) => die_by_expr_parse_error(e.into(), i, &lexed),
                    };

                    if count_ty != Type::Num {
                        die_cont!("Expected Num Expr", i, lexed);
                    }

                    expects!("\"Dice\" expected", Items::Key(Keywords::Dice), i, lexed);

                    expects!("\"With\" expected", Items::Key(Keywords::With), i, lexed);

                    let face = parse_expr!(Items::Key(Keywords::Face), i, tks, lexed, scope_stack);

                    let face_ty = match count.check_type(&scope_stack) {
                        Ok(t) => t,
                        Err(e) => die_by_expr_parse_error(e.into(), i, &lexed),
                    };

                    if face_ty != Type::Num {
                        die_cont!("Expected Num Expr", i, lexed);
                    }

                    expects!("\"Face\" expected", Items::Key(Keywords::Face), i, lexed);

                    expects!("\"To\" expected", Items::Key(Keywords::To), i, lexed);

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
                    Statement::Break
                }),

                lex::Command::Assert => parse_stmt!(i, stmts, {
                    let idx_start = i;
                    // "Assert" (<str> "With") <cond> ";"
                    let expr1 = parse_expr!(
                        Items::Semi | Items::Key(Keywords::With),
                        i,
                        tks,
                        lexed,
                        scope_stack
                    );

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
                                Items::Key(Keywords::With),
                                i,
                                lexed
                            );

                            let expr2 = parse_expr!(
                                Items::Semi | Items::Key(Keywords::With),
                                i,
                                tks,
                                lexed,
                                scope_stack
                            );
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
                            let s = ((idx_start+1)..i).map(|i| tks[i].item.to_string()).fold(
                                tks[idx_start].item.to_string(),
                                |mut acc, x| {
                                    acc += " ";
                                    acc += &x;
                                    acc
                                },
                            );
                            mesg = Expr::new_str(s);
                            cond = expr1;
                        }
                        _ => die_cont!("Expected Str or Num", i, lexed),
                    }

                    expects_semi!(i, lexed);

                    Statement::Assert { mesg, cond }
                }),
            }
        } else {
            die_cont!("Line must begin with Command", i, lexed);
        }
    }
    AST { stmts }
}
