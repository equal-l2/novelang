use crate::die;
use crate::exprs::Expr;
use crate::lex;

mod exprs;
mod type_check;

use exprs::TryFromTokens;
use type_check::TypeCheck;

pub enum ParseError {
    InvalidToken(lex::Token),
    EmptyExpr,
    NoPairParen { lparen: lex::Token },
    TrailingToken { from: lex::Token },
    TokenExhausted,
    SubInExpr,
    TypeConflict,
    VarNotFound,
}

#[derive(Debug, Clone)]
pub enum Insts {
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
}

#[derive(Debug, Clone)]
pub struct Program {
    pub insts: Vec<Insts>,
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
    if tks.len() == 0 {
        return Err(ParseError::EmptyExpr);
    }

    let ret = Expr::try_from_tokens(&mut tks.iter().peekable())?;

    let ty = ret.check_type(stack);
    match ty {
        Type::Sub => Err(ParseError::SubInExpr),
        Type::Conflict => Err(ParseError::TypeConflict),
        Type::NotFound => Err(ParseError::VarNotFound),
        _ => Ok(ret),
    }
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
        ParseError::TypeConflict => {
            die_cont!("Expression has conflicting types", i, lexed);
        }
        ParseError::SubInExpr => {
            die_cont!("Type Sub cannot be in Expr", i, lexed);
        }
        ParseError::VarNotFound => {
            die_cont!("Variable was not found", i, lexed);
        }
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

macro_rules! parse_inst {
    ($i: ident, $insts: ident, $proc: block) => {{
        $i += 1;
        let inst_obj = $proc;
        $insts.push(inst_obj);
    }};
}

macro_rules! expects_type {
    ($expr: ident, $ty: path, $stack: ident, $i: ident, $lexed: ident) => {
        let ty = $expr.check_type(&$stack);
        if ty != $ty {
            die_cont!(
                format!("Expected {}, found {}", $ty.typename(), ty.typename()),
                $i,
                $lexed
            )
        }
    };
}

#[derive(Clone, Debug, PartialEq, Eq)]
enum Type {
    Bool,
    Num,
    Str,
    Sub,
    Conflict,
    NotFound,
}

impl Type {
    fn typename(&self) -> &str {
        match self {
            Self::Bool => "Bool",
            Self::Num => "Num",
            Self::Str => "Str",
            Self::Sub => "Sub",
            Self::Conflict => "Conflict",
            Self::NotFound => "NotFound",
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

    fn get_type_info(&self, name: &String) -> Option<&TypeInfo> {
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

    fn get_type_info(&self, name: &String) -> Option<&TypeInfo> {
        self.scopes
            .iter()
            .rev()
            .map(|m| m.get_type_info(name))
            .find(Option::is_some)
            .flatten()
    }
}

pub fn parse(lexed: crate::lex::Lexed) -> Program {
    use lex::{Items, Keywords};

    let mut insts = vec![Insts::Ill];
    let mut scope_stack = ScopeStack::new();

    let tks = &lexed.tokens;

    let mut i = 0;
    while i < tks.len() {
        if let Items::Inst(inst) = &tks[i].item {
            match inst {
                lex::Insts::Print => parse_inst!(i, insts, {
                    // "Print" (expr {"," expr}) ";"
                    let mut args = Vec::new();
                    while i < tks.len() {
                        match &tks[i].item {
                            Items::Semi => break,
                            Items::Comma => {
                                i += 1;
                            }
                            _ => args.push(parse_expr!(
                                Items::Comma | Items::Semi,
                                i,
                                tks,
                                lexed,
                                scope_stack
                            )),
                        }
                    }
                    expects_semi!(i, lexed);
                    Insts::Print { args }
                }),

                lex::Insts::Sub => parse_inst!(i, insts, {
                    // "Sub" name ";"

                    // check if the Sub is nested (which is not allowed yet)
                    /*if let Some(top) = waits_end_stack.last() {
                        if let Insts::Sub { .. } = top.kind {
                            die_cont!("Nested Sub is not allowed yet", i, lexed);
                        }
                    }*/

                    // register the sub to the name table
                    if let Items::Ident(name) = &tks[i].item {
                        i += 1;
                        expects_semi!(i, lexed);

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

                        scope_stack.push(insts.len());

                        Insts::Sub {
                            name: name.clone(),
                            offset_to_end: 0,
                        }
                    } else {
                        die_cont!("Expected subroutine name", i, lexed)
                    }
                }),

                lex::Insts::Call => parse_inst!(i, insts, {
                    // "Call" name ";"
                    if let Items::Ident(name) = &tks[i].item {
                        i += 1;
                        expects_semi!(i, lexed);

                        let info = scope_stack.get_type_info(name);
                        if info.is_none() || info.unwrap().ty != Type::Sub {
                            die_cont!(format!("Subroutine \"{}\" was not found", name), i, lexed);
                        }

                        Insts::Call { name: name.clone() }
                    } else {
                        die_cont!("Expected subroutine name", i, lexed)
                    }
                }),

                lex::Insts::While => parse_inst!(i, insts, {
                    // "While" cond ";"

                    scope_stack.push(insts.len());

                    let expr = parse_expr!(Items::Semi, i, tks, lexed, scope_stack);
                    expects_type!(expr, Type::Bool, scope_stack, i, lexed);

                    expects_semi!(i, lexed);

                    Insts::While {
                        cond: expr,
                        offset_to_end: 0,
                    }
                }),

                lex::Insts::Let => parse_inst!(i, insts, {
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
                                ty: init.check_type(&scope_stack),
                                is_mut,
                            },
                        );

                        if !success {
                            die_cont!("Conflicting variable name", i, lexed);
                        }

                        Insts::Let {
                            name: name.clone(),
                            init,
                            is_mut,
                        }
                    } else {
                        die_cont!("Ident expected", i, lexed)
                    }
                }),

                lex::Insts::Modify => parse_inst!(i, insts, {
                    // "Modify" name "To" expr ";"

                    if let Items::Ident(name) = &tks[i].item {
                        i += 1;

                        expects!("To expected", Items::Key(Keywords::To), i, lexed);

                        let expr = parse_expr!(Items::Semi, i, tks, lexed, scope_stack);
                        expects_semi!(i, lexed);

                        let var_tinfo = scope_stack.get_type_info(name);
                        if let Some(info) = var_tinfo {
                            // TODO: better error message (maybe)
                            let expr_ty = expr.check_type(&scope_stack);

                            if info.ty != expr_ty {
                                die_cont!("Type mismatch", i, lexed);
                            }

                            if !info.is_mut {
                                die_cont!("Variable is immutable", i, lexed);
                            }
                        } else {
                            die_cont!(format!("Variable \"{}\" was not found", name), i, lexed);
                        }

                        Insts::Modify {
                            name: name.clone(),
                            expr,
                        }
                    } else {
                        die_cont!("Ident expected", i, lexed);
                    }
                }),

                lex::Insts::If => parse_inst!(i, insts, {
                    // "If" cond ";"

                    let cond = parse_expr!(Items::Semi, i, tks, lexed, scope_stack);
                    expects_semi!(i, lexed);

                    scope_stack.push(insts.len());

                    Insts::If {
                        cond,
                        offset_to_next: 0,
                    }
                }),

                lex::Insts::Else => parse_inst!(i, insts, {
                    // "Else" ("If" cond) ";"

                    let inst_obj = if let Items::Inst(lex::Insts::If) = &tks[i].item {
                        // "Else" "If" cond ";"
                        i += 1;

                        let prev_idx = scope_stack.pop().unwrap_or_else(|| {
                            die_cont!("A stray Else-If detected.", i, lexed);
                        });

                        let offset_to_next = insts.len() - prev_idx;

                        let prev = insts[prev_idx].clone();
                        insts[prev_idx] = match prev {
                            Insts::If { cond, .. } => Insts::If {
                                cond: cond.clone(),
                                offset_to_next,
                            },
                            Insts::ElIf { cond, .. } => Insts::ElIf {
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

                        Insts::ElIf {
                            cond,
                            offset_to_next: 0,
                        }
                    } else {
                        // "Else" ";"
                        expects_semi!(i, lexed);

                        let prev_idx = scope_stack.pop().unwrap_or_else(|| {
                            die_cont!("A stray Else detected.", i, lexed);
                        });

                        let offset_to_next = insts.len() - prev_idx;

                        let prev = insts[prev_idx].clone();
                        insts[prev_idx] = match prev {
                            Insts::If { cond, .. } => Insts::If {
                                cond: cond.clone(),
                                offset_to_next,
                            },
                            Insts::ElIf { cond, .. } => Insts::ElIf {
                                cond: cond.clone(),
                                offset_to_next,
                            },
                            _ => {
                                die_cont!("Cannot find corresponding Element for Else", i, lexed);
                            }
                        };
                        Insts::Else { offset_to_end: 0 }
                    };

                    scope_stack.push(insts.len());

                    inst_obj
                }),

                lex::Insts::End => parse_inst!(i, insts, {
                    // "End" ";"
                    expects_semi!(i, lexed);

                    // Pop stack and assign end index
                    let prev_idx = scope_stack.pop().unwrap_or_else(|| {
                        die_cont!("A stray End detected.", i, lexed);
                    });

                    let offset_to_end = insts.len() - prev_idx;

                    let prev = insts[prev_idx].clone();
                    insts[prev_idx] = match prev {
                        Insts::Sub { name, .. } => Insts::Sub {
                            name,
                            offset_to_end,
                        },
                        Insts::While { cond, .. } => Insts::While {
                            cond,
                            offset_to_end,
                        },
                        Insts::If { ref cond, .. } => Insts::If {
                            cond: cond.clone(),
                            offset_to_next: offset_to_end,
                        },
                        Insts::ElIf { ref cond, .. } => Insts::ElIf {
                            cond: cond.clone(),
                            offset_to_next: offset_to_end,
                        },
                        Insts::Else { .. } => Insts::Else { offset_to_end },
                        _ => {
                            die_cont!("Cannot find corresponding Element for End", i, lexed);
                        }
                    };

                    Insts::End
                }),

                lex::Insts::Input => parse_inst!(i, insts, {
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

                    let as_num = match scope_stack.get_type_info(&name) {
                        Some(info) => {
                            if !info.is_mut {
                                die_cont!("Variable is immutable", i, lexed);
                            }
                            match info.ty {
                                Type::Num => true,
                                Type::Str => false,
                                _ => die_cont!("Expected Num or Str", i, lexed),
                            }
                        }
                        None => die_cont!(format!("Variable \"{}\" was not found", name), i, lexed),
                    };

                    expects_semi!(i, lexed);
                    Insts::Input {
                        prompt,
                        name,
                        as_num,
                    }
                }),

                lex::Insts::Roll => parse_inst!(i, insts, {
                    // "Roll" n "Dice" "With" k "Face" "To" name ";"

                    let count = parse_expr!(Items::Key(Keywords::Dice), i, tks, lexed, scope_stack);
                    if count.check_type(&scope_stack) != Type::Num {
                        die_cont!("Expected Num Expr", i, lexed);
                    }

                    expects!("\"Dice\" expected", Items::Key(Keywords::Dice), i, lexed);

                    expects!("\"With\" expected", Items::Key(Keywords::With), i, lexed);

                    let face = parse_expr!(Items::Key(Keywords::Face), i, tks, lexed, scope_stack);
                    if face.check_type(&scope_stack) != Type::Num {
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

                    match scope_stack.get_type_info(&name) {
                        Some(info) => {
                            if !matches!(info.ty, Type::Num) {
                                die_cont!("Expected Num", i, lexed)
                            }
                            if !info.is_mut {
                                die_cont!("Variable is immutable", i, lexed);
                            }
                        }
                        None => die_cont!(format!("Variable \"{}\" was not found", name), i, lexed),
                    };

                    expects_semi!(i, lexed);
                    Insts::Roll { count, face, name }
                }),

                lex::Insts::Halt => parse_inst!(i, insts, {
                    // "Halt" ";"
                    expects_semi!(i, lexed);
                    Insts::Halt
                }),

                lex::Insts::Break => parse_inst!(i, insts, {
                    // "Break" ";"
                    expects_semi!(i, lexed);
                    Insts::Break
                }),
            }
        } else {
            die_cont!("Line must begin with Inst", i, lexed);
        }
    }
    Program { insts }
}
