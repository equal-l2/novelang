use crate::parser::Inst;
use crate::parser::PrintArgs;
use crate::parser::Program;

pub type VarTable = std::collections::HashMap<String, Variable>;

pub struct CallStack {
    ret_stack: Vec<usize>,
    vars_stack: Vec<VarTable>,
}

impl CallStack {
    fn new() -> Self {
        // init with one vartable for global vars
        Self {
            ret_stack: vec![],
            vars_stack: vec![VarTable::new()],
        }
    }

    fn pop(&mut self) -> Option<usize> {
        self.vars_stack.pop().unwrap();
        self.ret_stack.pop()
    }

    fn push(&mut self, ret_index: usize) {
        self.vars_stack.push(Default::default());
        self.ret_stack.push(ret_index)
    }

    // get the highest variable in the stack with the specified name
    pub fn get_var(&self, name: &str) -> Option<&Variable> {
        self.vars_stack
            .iter()
            .rev()
            .map(|t| t.get(name))
            .find(|v| v.is_some())
            .flatten()
    }

    fn get_var_mut(&mut self, name: &str) -> Option<&mut Variable> {
        self.vars_stack
            .iter_mut()
            .rev()
            .map(|t| t.get_mut(name))
            .find(|v| v.is_some())
            .flatten()
    }
}

#[derive(Debug, Clone)]
pub struct Variable {
    is_mutable: bool,
    pub value: isize,
}

macro_rules! die {
    ($( $x:expr ),*) => {
        let _ = crossterm::terminal::disable_raw_mode();
        eprintln!($($x,)*);
        std::process::exit(1);
    }
}

fn exec_print(idx: usize, call_stack: &CallStack, wait: bool, args: &Vec<PrintArgs>) {
    use crate::exprs::Eval;
    use crossterm::*;
    use std::io::Write;
    let mut stdout = std::io::stdout();

    queue!(
        stdout,
        terminal::Clear(terminal::ClearType::CurrentLine),
        style::Print(format!("{:04} :", idx)),
    )
    .unwrap();
    for arg in args {
        stdout
            .queue(style::Print(match arg {
                PrintArgs::String(i) => format!(" {}", i),
                PrintArgs::Expr(i) => format!(
                    " {}",
                    i.eval(&call_stack).unwrap_or_else(|e| {
                        die!("Runtime error: cannot eval expr: {}", e);
                    })
                ),
            }))
            .unwrap();
    }
    stdout.queue(style::Print("\r\n")).unwrap();
    let _ = stdout.flush();

    if wait {
        stdout
            .execute(style::Print("[Proceed with any key]\r"))
            .unwrap();
        wait_keypress();
    }
}

fn run_insts(prog: Program, wait: bool) {
    let mut call_stack = CallStack::new();

    let mut i = 1; // index 0 is reserved (unreachable)
    let mut if_eval = false;

    while i < prog.insts.len() {
        use crate::exprs::Eval;
        match &prog.insts[i] {
            Inst::Print { args } => {
                exec_print(i, &call_stack, wait, args);
            }
            Inst::Sub { offset_to_end, .. } => {
                i += offset_to_end;
            }
            Inst::Call { name } => {
                if let Some(idx) = prog.subs.get(name) {
                    call_stack.push(i + 1);
                    i = *idx;
                } else {
                    die!("Runtime error: function \"{}\" was not found", name);
                }
            }
            Inst::While {
                cond,
                offset_to_end,
            } => match cond.eval(&call_stack) {
                Ok(true) => call_stack.push(i),
                Ok(false) => i += offset_to_end,
                Err(e) => {
                    die!("Runtime error: CondExpr cannot be evaled: {}", e);
                }
            },
            Inst::Let { name, init, is_mut } => {
                let init_var = Variable {
                    is_mutable: *is_mut,
                    value: init.eval(&call_stack).unwrap_or_else(|e| {
                        die!("Runtime error: cannot eval the init value: {}", e);
                    }),
                };
                if call_stack
                    .vars_stack
                    .last_mut()
                    .unwrap()
                    .insert(name.clone(), init_var)
                    .is_some()
                {
                    die!("Runtime error: variable {} is already declared", name);
                }
            }
            Inst::Modify { name, expr } => {
                let to_value = expr.eval(&call_stack).unwrap_or_else(|e| {
                    die!("Runtime error: cannot eval the value: {}", e);
                });
                let var = call_stack.get_var_mut(name).unwrap_or_else(|| {
                    die!("Runtime error: variable was not found");
                });
                if var.is_mutable {
                    var.value = to_value;
                } else {
                    die!("Runtime error: variable {} is immutable", name);
                }
            }
            Inst::If {
                cond,
                offset_to_next,
            } => {
                // use a scope, but don't use a return address
                // push a frame always to unify End behavior
                call_stack.push(0);
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
                let top = call_stack.pop();
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
            #[allow(unreachable_patterns)]
            other => {
                die!("Runtime error: unknown instruction: {:?}", other);
            }
        }
        i += 1;
    }
}

pub fn run(prog: Program, wait: bool) {
    ctrlc::set_handler(|| {
        let _ = crossterm::terminal::disable_raw_mode();
        std::process::exit(-1);
    })
    .unwrap();
    if wait {
        let _ = crossterm::terminal::enable_raw_mode();
    }
    run_insts(prog, wait);
    let _ = crossterm::terminal::disable_raw_mode();
}

fn wait_keypress() {
    use crossterm::event::*;
    loop {
        if let Event::Key(KeyEvent { .. }) = read().unwrap() {
            return;
        }
    }
}
