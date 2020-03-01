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
    pub fn get_var(&self, name: &String) -> Option<&Variable> {
        for table in self.vars_stack.iter().rev() {
            if let Some(v) = table.get(name) {
                return Some(v);
            }
        }
        None
    }

    fn get_var_mut(&mut self, name: &String) -> Option<&mut Variable> {
        for table in self.vars_stack.iter_mut().rev() {
            if let Some(v) = table.get_mut(name) {
                return Some(v);
            }
        }
        None
    }
}

#[derive(Debug, Clone)]
pub struct Variable {
    is_mutable: bool,
    pub value: usize,
}

macro_rules! die {
    ($( $x:expr ),*) => {
        let _ = crossterm::terminal::disable_raw_mode();
        eprintln!($($x,)*);
        std::process::exit(1);
    }
}

fn run_insts(prog: Program, wait: bool) {
    use std::io::Write;

    let mut call_stack = CallStack::new();
    let mut i = 0;
    while i < prog.insts.len() {
        use crate::exprs::Eval;
        match &prog.insts[i] {
            Inst::Print { args } => {
                use crossterm::*;
                let mut stdout = std::io::stdout();
                queue!(
                    stdout,
                    terminal::Clear(terminal::ClearType::CurrentLine),
                    style::Print(format!("{:04} :", i)),
                );
                for arg in args {
                    stdout.queue(style::Print(match arg {
                        PrintArgs::String(i) => format!(" {}", i),
                        PrintArgs::Expr(i) => format!(" {}", i.eval(&call_stack).unwrap()),
                    }));
                }

                stdout.queue(style::Print("\r\n[Proceed with any key]\r"));

                let _ = stdout.flush();

                if wait {
                    let _ = wait_keypress();
                }
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
            } => {
                if let Some(b) = cond.eval(&call_stack) {
                    if b {
                        call_stack.push(i);
                    } else {
                        i += offset_to_end;
                    }
                } else {
                    die!("Runtime error: condition expression is corrupted");
                }
            }
            Inst::Let { name, init } => {
                let init_var = Variable {
                    is_mutable: false,
                    value: init.eval(&call_stack).unwrap_or_else(|| {
                        die!("Runtime error: init value of Let is None");
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
            Inst::LetMut { name, init } => {
                let init_var = Variable {
                    is_mutable: true,
                    value: init.eval(&call_stack).unwrap_or_else(|| {
                        die!("Runtime error: init value of Let is None");
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
                let to_value = expr.eval(&call_stack).unwrap_or_else(|| {
                    die!("Runtime error: expr value of Modify is None");
                });
                let var = call_stack.get_var_mut(name).unwrap_or_else(|| {
                    die!("Runtime error: target variable of Modify was not found");
                });
                if var.is_mutable {
                    var.value = to_value;
                } else {
                    die!("Runtime error: variable {} is immutable", name);
                }
            }
            Inst::End => {
                if let Some(ret_idx) = call_stack.pop() {
                    i = ret_idx;
                } else {
                    die!("Runtime error: index to return was not set");
                }
                continue;
            }
            other => {
                die!("Runtime error: unknown instruction type : {:?}", other);
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
