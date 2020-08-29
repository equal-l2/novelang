use std::io::Write;

use crate::exprs::Eval;
use crate::parse::Inst;
use crate::parse::PrintArgs;
use crate::parse::Program;

macro_rules! die {
    ($( $x:expr ),*) => {
        eprintln!($($x,)*);
        std::process::exit(1);
    }
}

pub type VarTable = std::collections::HashMap<String, Variable>;
pub type VarIntType = i64;

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

pub struct ScopeStack {
    stack: Vec<Scope>,
    globals: VarTable,
    internals: VarTable,
}

impl ScopeStack {
    fn new() -> Self {
        let internals = {
            let mut vt = VarTable::new();
            vt.insert(
                "_result".to_owned(),
                Variable {
                    is_mutable: true,
                    value: 0,
                },
            );
            vt
        };
        Self {
            stack: vec![],
            globals: VarTable::new(),
            internals,
        }
    }

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

    fn modify_var(&mut self, name: &str, val: VarIntType) {
        let var = self.get_var_mut(name).unwrap_or_else(|| {
            die!("Runtime error: variable was not found");
        });
        if var.is_mutable {
            var.value = val;
        } else {
            die!("Runtime error: variable {} is immutable", name);
        }
    }

    fn pop(&mut self) -> Option<Scope> {
        self.stack.pop()
    }

    fn push(&mut self, kind: ScopeKind, ret_idx: usize) {
        self.stack.push(Scope::new(kind, ret_idx))
    }

    fn vars_iter<'a>(&'a self) -> impl Iterator<Item = &'a VarTable> {
        use std::iter::once;
        once(&self.internals)
            .chain(self.stack.iter().map(|f| &f.vars).rev())
            .chain(once(&self.globals))
    }

    fn vars_iter_mut<'a>(&'a mut self) -> impl Iterator<Item = &'a mut VarTable> {
        use std::iter::once;
        once(&mut self.internals)
            .chain(self.stack.iter_mut().map(|f| &mut f.vars).rev())
            .chain(once(&mut self.globals))
    }

    // get the highest variable in the stack with the specified name
    pub fn get_var(&self, name: &str) -> Option<&Variable> {
        self.vars_iter()
            .map(|t| t.get(name))
            .find(|v| v.is_some())
            .flatten()
    }

    fn get_var_mut(&mut self, name: &str) -> Option<&mut Variable> {
        self.vars_iter_mut()
            .map(|t| t.get_mut(name))
            .find(|v| v.is_some())
            .flatten()
    }
}

#[derive(Debug, Clone)]
pub struct Variable {
    is_mutable: bool,
    pub value: VarIntType,
}

fn exec_print(idx: usize, call_stack: &ScopeStack, wait: bool, args: &Vec<PrintArgs>) {
    let stdout = std::io::stdout();
    let mut lock = stdout.lock();

    write!(lock, "{:04} :", idx).unwrap();
    for arg in args {
        match arg {
            PrintArgs::String(i) => write!(lock, " {}", i),
            PrintArgs::Expr(i) => write!(
                lock,
                " {}",
                i.eval(&call_stack).unwrap_or_else(|e| {
                    die!("Runtime error: cannot eval expr: {}", e);
                })
            ),
        }
        .unwrap();
    }
    write!(lock, "\n").unwrap();
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

fn get_int_input(prompt: Option<&str>) -> VarIntType {
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

pub fn run(prog: Program) {
    let mut wait = false;
    let mut call_stack = ScopeStack::new();

    let mut i = 1; // index 0 is reserved (unreachable)
    let mut if_eval = false;
    let mut breaking = false;

    while i < prog.insts.len() {
        match &prog.insts[i] {
            Inst::Print { args } => {
                exec_print(i, &call_stack, wait, args);
            }
            Inst::Sub { offset_to_end, .. } => {
                i += offset_to_end;
            }
            Inst::Call { name } => {
                if let Some(idx) = prog.subs.get(name) {
                    call_stack.push(ScopeKind::Sub, i + 1);
                    i = *idx;
                } else {
                    die!("Runtime error: function \"{}\" was not found", name);
                }
            }
            Inst::While {
                cond,
                offset_to_end,
            } => {
                if breaking {
                    breaking = false;
                    i += offset_to_end;
                } else {
                    match cond.eval(&call_stack) {
                        Ok(true) => call_stack.push(ScopeKind::Loop, i),
                        Ok(false) => i += offset_to_end,
                        Err(e) => {
                            die!("Runtime error: CondExpr cannot be evaled: {}", e);
                        }
                    }
                }
            }
            Inst::Let { name, init, is_mut } => {
                let init_var = Variable {
                    is_mutable: *is_mut,
                    value: init.eval(&call_stack).unwrap_or_else(|e| {
                        die!("Runtime error: cannot eval the init value: {}", e);
                    }),
                };
                call_stack.decl_var(name, init_var);
            }
            Inst::Modify { name, expr } => {
                let to_value = expr.eval(&call_stack).unwrap_or_else(|e| {
                    die!("Runtime error: cannot eval the value: {}", e);
                });
                call_stack.modify_var(name, to_value);
            }
            Inst::If {
                cond,
                offset_to_next,
            } => {
                // use a scope, but don't use a return address
                // push a frame always to unify End behavior
                call_stack.push(ScopeKind::Branch, 0);
                match cond.eval(&call_stack) {
                    Ok(true) => {
                        // go to body
                        // no-op
                    }
                    Ok(false) => {
                        i += offset_to_next;
                        if_eval = true;
                        continue;
                    }
                    Err(e) => {
                        die!("Runtime error: CondExpr cannot be evaled: {}", e);
                    }
                }
            }
            Inst::ElIf {
                cond,
                offset_to_next,
                ..
            } => {
                if if_eval {
                    match cond.eval(&call_stack) {
                        Ok(true) => {
                            // don't push a frame
                            // since If pushed one already
                            if_eval = false;
                        }
                        Ok(false) => {
                            i += offset_to_next;
                            continue;
                        }
                        Err(e) => {
                            die!("Runtime error: CondExpr cannot be evaled: {}", e);
                        }
                    }
                } else {
                    i += offset_to_next;
                    continue;
                }
            }
            Inst::Else { offset_to_end, .. } => {
                if if_eval {
                    // don't push a frame
                    // since If pushed one already
                    if_eval = false;
                } else {
                    i += offset_to_end;
                    continue;
                }
            }
            Inst::End => {
                if_eval = false;
                let top = call_stack.pop().map(|s| s.ret_idx);
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
                        die!("Runtime error: index to return was not set");
                    }
                }
            }
            Inst::Input { prompt } => {
                call_stack.modify_var("_result", get_int_input(prompt.as_deref()));
            }
            Inst::Roll { count, face } => {
                let count = count.eval(&call_stack).unwrap_or_else(|e| {
                    die!("Runtime error: cannot eval expr: {}", e);
                });
                let face = face.eval(&call_stack).unwrap_or_else(|e| {
                    die!("Runtime error: cannot eval expr: {}", e);
                });

                if count <= 0 {
                    die!("Runtime error: Count for Roll must be a positive integer");
                }

                if face <= 0 {
                    die!("Runtime error: Face for Roll must be a positive integer");
                }
                call_stack.modify_var("_result", roll_dice(count, face));
            }
            Inst::Halt => {
                return;
            }
            Inst::Break => {
                i = loop {
                    if let Some(scope) = call_stack.pop() {
                        match scope.kind {
                            ScopeKind::Loop => {
                                breaking = true;
                                break scope.ret_idx;
                            }
                            ScopeKind::Sub => {
                                break scope.ret_idx;
                            }
                            _ => {}
                        }
                    } else {
                        die!("Scope stack is empty");
                    }
                };
                continue;
            }
            Inst::EnableWait => {
                wait = true;
            }
            Inst::DisableWait => {
                wait = false;
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
    it.next().unwrap_or(Ok("".to_owned())).unwrap()
}

fn roll_dice(count: VarIntType, face: VarIntType) -> VarIntType {
    use rand::Rng;
    let mut rng = rand::thread_rng();
    let mut sum = 0;

    for _ in 0..count {
        sum += rng.gen_range(1, face + 1);
    }

    sum
}
