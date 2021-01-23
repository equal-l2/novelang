mod typed;
mod variable;

use crate::lex;
use crate::parse::Insts;
use crate::parse::PrintArgs;
use crate::parse::Program;
use crate::types::IntType;

use lex::Item;
use typed::Typed;
use variable::{ModifyError, Variable};

/// prints expr and exit
macro_rules! die {
    ($( $x:expr ),*) => {
        eprintln!($($x,)*);
        std::process::exit(1);
    }
}

type VarTable = std::collections::HashMap<String, Variable>;

/// Represents a scope
struct Scope {
    kind: ScopeKind,
    ret_idx: usize,
    vars: VarTable,
}

impl Scope {
    fn new(kind: ScopeKind, ret_idx: usize) -> Self {
        Self {
            kind,
            ret_idx,
            vars: VarTable::new(),
        }
    }
}

enum ScopeKind {
    Branch,
    Loop,
    Sub,
}

/// Represents the store for runtime state
pub struct Runtime {
    stack: Vec<Scope>,
    globals: VarTable,
    internals: VarTable,
}

impl Runtime {
    fn new() -> Self {
        // internal variables
        // - "_result": stores result of Input and Roll
        // - "_wait": flag if wait is enabled

        let internals = {
            let mut vt = VarTable::new();
            vt.insert("_result".to_owned(), Variable::new_mut(Typed::Num(0)));
            vt.insert("_wait".to_owned(), Variable::new_mut(Typed::Bool(false)));
            vt
        };

        Self {
            stack: vec![],
            globals: VarTable::new(),
            internals,
        }
    }

    /// Declare a variable
    /// Aborts when the variable is already declared in the scope
    fn decl_var(&mut self, name: &str, val: Variable) {
        let var_table = if self.stack.is_empty() {
            &mut self.globals
        } else {
            &mut self.stack.last_mut().unwrap().vars
        };
        if var_table.insert(name.to_owned(), val).is_some() {
            die!("Runtime error: variable {} is already declared", name);
        }
    }

    /// Modify a variable
    /// Aborts on error (the variable doesn't exists, differ in type, or is immutable)
    fn modify_var(&mut self, name: &str, val: Typed) {
        // no check for internals as already done in the parse phase.

        let var = self.get_var_mut(name).unwrap_or_else(|| {
            die!("Runtime error: variable was not found");
        });

        match var.modify(val) {
            Ok(_) => {}
            Err(ModifyError::TypeDiffers) => {
                die!("Runtime error: Type differs");
            }
            Err(ModifyError::Immutable) => {
                die!("Runtime error: variable {} is immutable", name);
            }
        }
    }

    /// Pop the current scope
    fn pop(&mut self) -> Option<Scope> {
        self.stack.pop()
    }

    /// Push a new scope
    fn push(&mut self, kind: ScopeKind, ret_idx: usize) {
        self.stack.push(Scope::new(kind, ret_idx))
    }

    fn vars_iter(&self) -> impl Iterator<Item = &VarTable> {
        use std::iter::once;
        once(&self.internals)
            .chain(self.stack.iter().map(|f| &f.vars).rev())
            .chain(once(&self.globals))
    }

    fn vars_iter_mut(&mut self) -> impl Iterator<Item = &mut VarTable> {
        use std::iter::once;
        once(&mut self.internals)
            .chain(self.stack.iter_mut().map(|f| &mut f.vars).rev())
            .chain(once(&mut self.globals))
    }

    // get the highest variable in the stack with the specified name
    pub fn get_var(&self, name: &str) -> Option<&Variable> {
        self.vars_iter() // Iterator<Item = &mut VarTable>
            .map(|t| t.get(name)) // Iterator<Item = Option<&Variable>>
            .find(Option::is_some) // Option<Option<&Variable>>
            .flatten() // Option<&Variable>
    }

    fn get_var_mut(&mut self, name: &str) -> Option<&mut Variable> {
        self.vars_iter_mut()
            .map(|t| t.get_mut(name))
            .find(Option::is_some)
            .flatten()
    }

    fn eval(&self, expr: &crate::exprs::Expr) -> Result<Typed, EvalError> {
        use crate::exprs::RPNode;
        use crate::lex::{AriOps, Ops, RelOps};
        let resolve_ident = |name: &str| {
            if let Some(v) = self.get_var(name) {
                Ok(v.get().clone())
            } else {
                Err(EvalError::VariableNotFound(name.to_owned()))
            }
        };
        let list = &expr.content;
        match list.len() {
            0 => return Err(EvalError::InvalidExpr(expr.clone())),
            1 => {
                return match list.last().unwrap() {
                    RPNode::Num(num) => Ok(Typed::Num(*num)),
                    RPNode::Bool(b) => Ok(Typed::Bool(*b)),
                    RPNode::Ident(name) => resolve_ident(name),
                    _ => Err(EvalError::InvalidExpr(expr.clone())),
                }
            }
            _ => {}
        };

        let mut stack = vec![];
        let wrap = |v| {
            if let RPNode::Ident(name) = v {
                resolve_ident(&name).map(|v| match v {
                    Typed::Num(n) => RPNode::Num(n),
                    Typed::Bool(b) => RPNode::Bool(b),
                })
            } else {
                Ok(v)
            }
        };
        for n in list {
            if let RPNode::Ops(op) = n {
                let rhs = stack.pop().map(wrap).transpose()?;
                let lhs = stack.pop().map(wrap).transpose()?;
                match (lhs, rhs) {
                    (Some(lhs), Some(rhs)) => {
                        let lhs_type = lhs.typename();
                        let rhs_type = rhs.typename();
                        let typeerror = |op: &str| {
                            Err(EvalError::TypeError(format!(
                                "Operator \"{}\" cannot be applied to {}-{}",
                                op, lhs_type, rhs_type,
                            )))
                        };
                        match (&lhs, &rhs) {
                            (RPNode::Num(lhs), RPNode::Num(rhs)) => stack.push(match op {
                                Ops::Ari(op) => RPNode::Num(match op {
                                    AriOps::Add => {
                                        lhs.checked_add(*rhs).ok_or(EvalError::OverFlow)?
                                    }
                                    AriOps::Sub => {
                                        lhs.checked_sub(*rhs).ok_or(EvalError::OverFlow)?
                                    }
                                    AriOps::Mul => {
                                        lhs.checked_mul(*rhs).ok_or(EvalError::OverFlow)?
                                    }
                                    AriOps::Div => {
                                        lhs.checked_div(*rhs).ok_or(EvalError::ZeroDivision)?
                                    }
                                    AriOps::Mod => {
                                        lhs.checked_rem(*rhs).ok_or(EvalError::OverFlow)?
                                    }
                                }),
                                Ops::Rel(op) => RPNode::Bool(match op {
                                    RelOps::LessThan => lhs < rhs,
                                    RelOps::GreaterThan => lhs > rhs,
                                    RelOps::Equal => lhs == rhs,
                                    RelOps::NotEqual => lhs != rhs,
                                    RelOps::LessEqual => lhs <= rhs,
                                    RelOps::GreaterEqual => lhs >= rhs,
                                }),
                            }),
                            (RPNode::Bool(lhs), RPNode::Bool(rhs)) => stack.push(match op {
                                Ops::Rel(op) => RPNode::Bool(match op {
                                    RelOps::Equal => lhs == rhs,
                                    RelOps::NotEqual => lhs != rhs,
                                    _ => return typeerror(lex::Item::as_str(op)),
                                }),
                                _ => return typeerror(op.as_str()),
                            }),
                            _ => return typeerror(op.as_str()),
                        }
                    }
                    _ => {
                        return Err(EvalError::InvalidExpr(expr.clone()));
                    }
                }
            } else {
                stack.push(n.clone());
            }
        }

        // check if expression is sound
        if stack.len() == 1 {
            match stack.last().unwrap() {
                RPNode::Num(num) => Ok(Typed::Num(*num)),
                RPNode::Bool(b) => Ok(Typed::Bool(*b)),
                _ => Err(EvalError::InvalidExpr(expr.clone())),
            }
        } else {
            Err(EvalError::InvalidExpr(expr.clone()))
        }
    }
}

#[derive(Debug)]
enum EvalError {
    VariableNotFound(String),
    InvalidExpr(crate::exprs::Expr),
    OverFlow,
    ZeroDivision,
    TypeError(String),
}

impl std::fmt::Display for EvalError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Failed to eval because ")?;
        match self {
            Self::VariableNotFound(s) => write!(f, "variable {} was not found", s),
            Self::InvalidExpr(e) => write!(f, "expr {:?} is invalid", e),
            Self::OverFlow => write!(f, "of overflow"),
            Self::ZeroDivision => write!(f, "of zero division"),
            Self::TypeError(s) => write!(f, "of type error : {}", s),
        }?;
        Ok(())
    }
}

fn exec_print(idx: usize, runtime: &Runtime, wait: bool, args: &[PrintArgs]) {
    use std::io::Write;
    let stdout = std::io::stdout();
    let mut lock = stdout.lock();

    write!(lock, "{:04} :", idx).unwrap();
    for arg in args {
        match arg {
            PrintArgs::Str(i) => write!(lock, " {}", i),
            PrintArgs::Expr(i) => {
                match runtime.eval(i).unwrap_or_else(|e| {
                    // FIXME
                    die!("Runtime error: Failed to eval arg of Print: {:?}", e);
                }) {
                    Typed::Num(n) => write!(lock, " {}", n),
                    Typed::Bool(b) => write!(lock, " {}", b),
                }
            }
        }
        .unwrap();
    }
    writeln!(lock).unwrap();
    let _ = lock.flush();

    if wait {
        write!(lock, "[Proceed with Enter⏎ ]").unwrap();
        let _ = lock.flush();
        let _ = read_line_from_stdin();
        {
            use crossterm::cursor;
            use crossterm::execute;
            use crossterm::terminal;
            execute!(
                lock,
                cursor::MoveToPreviousLine(1),
                terminal::Clear(terminal::ClearType::CurrentLine)
            )
            .unwrap();
        }
    }
}

fn get_int_input(prompt: Option<&str>) -> IntType {
    use std::io::Write;
    let stdout = std::io::stdout();
    let mut lock = stdout.lock();
    loop {
        write!(lock, "{} > ", prompt.unwrap_or("Provide an integer")).unwrap();
        let _ = lock.flush();
        if let Ok(i) = read_line_from_stdin().parse() {
            return i;
        }
        writeln!(lock, "!! Provided input is invalid").unwrap();
        let _ = lock.flush();
    }
}

fn unwrap_bool(val: &Typed) -> bool {
    if let Typed::Bool(b) = val {
        *b
    } else {
        die!("Runtime error: Bool expected, got Num");
    }
}

fn unwrap_num(val: &Typed) -> IntType {
    if let Typed::Num(n) = val {
        *n
    } else {
        die!("Runtime error: Num expected, got Bool");
    }
}

pub fn run(prog: Program) {
    let mut runtime = Runtime::new();

    let mut i = 1; // index 0 is reserved (unreachable)
    let mut if_eval = false;
    let mut breaking = false;

    while i < prog.insts.len() {
        match &prog.insts[i] {
            Insts::Print { args } => {
                exec_print(
                    i,
                    &runtime,
                    unwrap_bool(runtime.get_var("_wait").unwrap().get()),
                    args,
                );
            }
            Insts::Sub { offset_to_end, .. } => {
                i += offset_to_end;
            }
            Insts::Call { name } => {
                if let Some(idx) = prog.subs.get(name) {
                    runtime.push(ScopeKind::Sub, i + 1);
                    i = *idx;
                } else {
                    die!("Runtime error: function \"{}\" was not found", name);
                }
            }
            Insts::While {
                cond,
                offset_to_end,
            } => {
                if breaking {
                    breaking = false;
                    i += offset_to_end;
                } else {
                    let val = runtime.eval(cond).unwrap_or_else(|e| {
                        // FIXME
                        die!("Runtime error: failed to eval condition of While : {}", e);
                    });

                    if unwrap_bool(&val) {
                        runtime.push(ScopeKind::Loop, i);
                    } else {
                        i += offset_to_end;
                    }
                }
            }
            Insts::Let { name, init, is_mut } => {
                // there is no check for internals, as the check is done in the parse phase.
                let init_val = runtime.eval(init).unwrap_or_else(|e| {
                    die!("Runtime error: Failed to eval init value of Let: {}", e);
                });
                runtime.decl_var(
                    name,
                    if *is_mut {
                        Variable::new_mut(init_val)
                    } else {
                        Variable::new(init_val)
                    },
                );
            }
            Insts::Modify { name, expr } => {
                // there is no check for internals, as the check is done in the parse phase.
                let to_value = runtime.eval(expr).unwrap_or_else(|e| {
                    // FIXME
                    die!("Runtime error: Failed to eval value of Modify: {}", e);
                });
                runtime.modify_var(name, to_value);
            }
            Insts::If {
                cond,
                offset_to_next,
            } => {
                // use a scope, but don't use a return address
                // push a frame always to unify End behavior
                runtime.push(ScopeKind::Branch, 0);
                let val = runtime.eval(cond).unwrap_or_else(|e| {
                    // FIXME
                    die!("Runtime error: Failed to eval condition of If: {}", e);
                });
                if unwrap_bool(&val) {
                    // go to body
                    // no-op
                } else {
                    i += offset_to_next;
                    if_eval = true;
                    continue;
                }
            }
            Insts::ElIf {
                cond,
                offset_to_next,
                ..
            } => {
                if if_eval {
                    let val = runtime.eval(cond).unwrap_or_else(|e| {
                        // FIXME
                        die!("Runtime error: Failed to eval condition of Elif: {}", e);
                    });
                    if unwrap_bool(&val) {
                        // don't push a frame
                        // since If pushed the one already
                        if_eval = false;
                    } else {
                        i += offset_to_next;
                        continue;
                    }
                } else {
                    i += offset_to_next;
                    continue;
                }
            }
            Insts::Else { offset_to_end, .. } => {
                if if_eval {
                    // don't push a frame
                    // since If pushed the one already
                    if_eval = false;
                } else {
                    i += offset_to_end;
                    continue;
                }
            }
            Insts::End => {
                if_eval = false;
                let top = runtime.pop().map(|s| s.ret_idx);
                match top {
                    Some(0) => {
                        // return address unspecified
                        // no-op
                    }
                    Some(ret_idx) => {
                        // return to the specified address
                        i = ret_idx;
                        continue;
                    }
                    _ => {
                        die!("Runtime error: scope stack is empty");
                    }
                }
            }
            Insts::Input { prompt } => {
                runtime.modify_var("_result", Typed::Num(get_int_input(prompt.as_deref())));
            }
            Insts::Roll { count, face } => {
                let count = unwrap_num(&runtime.eval(count).unwrap_or_else(|e| {
                    die!("Runtime error: Failed to eval count of Roll: {}", e);
                }));
                let face = unwrap_num(&runtime.eval(face).unwrap_or_else(|e| {
                    die!("Runtime error: Failed to eval face of Roll: {}", e);
                }));

                if count <= 0 {
                    die!("Runtime error: Count for Roll must be a positive integer");
                }

                if face <= 0 {
                    die!("Runtime error: Face for Roll must be a positive integer");
                }
                runtime.modify_var("_result", Typed::Num(roll_dice(count, face)));
            }
            Insts::Halt => {
                return;
            }
            Insts::Break => {
                i = loop {
                    if let Some(scope) = runtime.pop() {
                        match scope.kind {
                            ScopeKind::Loop => {
                                breaking = true;
                                break scope.ret_idx;
                            }
                            ScopeKind::Sub => {
                                break scope.ret_idx;
                            }
                            ScopeKind::Branch => {
                                // break outer scope
                            }
                        }
                    } else {
                        die!("Runtime error: scope stack is empty");
                    }
                };
                continue;
            }
            #[allow(unreachable_patterns)]
            other => {
                die!("Runtime error: unknown instruction: {:?}", other);
            }
        }
        i += 1;
    }
}

fn read_line_from_stdin() -> String {
    use std::io::BufRead;
    let stdin = std::io::stdin();
    let mut it = stdin.lock().lines();
    it.next().unwrap_or_else(|| Ok("".to_owned())).unwrap()
}

fn roll_dice(count: IntType, face: IntType) -> IntType {
    use rand::Rng;
    let mut rng = rand::thread_rng();
    let mut sum = 0;

    for _ in 0..count {
        sum += rng.gen_range(1..=face);
    }

    sum
}
