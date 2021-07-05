use crate::parse::{Statement, AST};
use crate::types::IntType;

mod expr;

use expr::Codegen;

#[derive(Clone, Debug)]
pub enum Val {
    Bool(bool),
    Num(IntType),
    Str(String),
}

impl From<&String> for Val {
    fn from(s: &String) -> Self {
        Self::Str(s.clone())
    }
}

impl From<IntType> for Val {
    fn from(i: IntType) -> Self {
        Self::Num(i)
    }
}

impl From<bool> for Val {
    fn from(b: bool) -> Self {
        Self::Bool(b)
    }
}

#[derive(Clone, Debug)]
pub enum Inst {
    Push(Val), // push val
    Store,     // store val [-1] to var with name [-2]
    Load,      // load var with name [-1] into stack
    Jump,      // jump to [-1]
    TJump,      // jump to [-2] if [-1] is true
    Call,      // call sub named [-1]
    Ret,       // return to original address (implementation is TBD)
    Print,     // print [-1]
    InputNum,  // take input as num into stack
    InputStr,  // take input as str into stack
    Roll,      // roll [-2] dice with [-1] faces
    Halt,
    Ill,
    Break,

    Neg,
    Eq,
    LT,
    GT,
    Mul,
    Div,
    Mod,
    Add,
    Sub,
}

#[derive(Clone, Debug)]
pub struct Asm {
    pub insts: Vec<Inst>,
}

pub fn codegen(ast: AST) -> Asm {
    Asm {
        insts: codegen_impl(0, ast.stmts.as_slice()).into(),
    }
}

fn codegen_impl(start_idx: usize, stmts: &[Statement]) -> Vec<Inst> {
    let mut insts = vec![];
    let mut i = start_idx;
    while i < stmts.len() {
        match &stmts[i] {
            Statement::Print { args } => {
                for a in args {
                    insts.extend(a.codegen());
                    insts.push(Inst::Print);
                }
                i += 1;
            }
            Statement::Sub {
                name,
                offset_to_end,
            } => {
                // store sub
                insts.push(Inst::Push((insts.len() as IntType).into()));
                insts.push(Inst::Push(name.into()));
                insts.push(Inst::Store);

                // codegen for content
                // TODO: nested function?
                let sub = codegen_impl(i + 1, &stmts[0..(i + offset_to_end)]);
                insts.extend(sub);

                i += offset_to_end;
            }
            Statement::Call { name } => {
                insts.push(Inst::Push(name.into()));
                insts.push(Inst::Call);
                i += 1;
            }
            Statement::While {
                cond,
                offset_to_end,
            } => {
                todo!();
                i += 1;
            }
            Statement::Let {
                name, init: expr, ..
            }
            | Statement::Modify { name, expr } => {
                // mutability is handled in parsing parsing phase
                insts.extend(expr.codegen());
                insts.push(Inst::Push(name.into()));
                insts.push(Inst::Store);
                i += 1;
            }
            Statement::If {
                cond,
                offset_to_next,
            } => {
                todo!();
                i += 1;
            }
            Statement::ElIf {
                cond,
                offset_to_next,
            } => {
                todo!();
                i += 1;
            }
            Statement::Else { offset_to_end } => {
                todo!();
                i += 1;
            }
            Statement::End => {
                todo!();
                i += 1;
            }
            Statement::Input {
                prompt,
                name,
                as_num,
            } => {
                if let Some(s) = prompt {
                    insts.push(Inst::Push(s.into()));
                    insts.push(Inst::Print);
                }

                insts.push(if *as_num {
                    Inst::InputNum
                } else {
                    Inst::InputStr
                });

                insts.push(Inst::Push(name.into()));
                insts.push(Inst::Store);
                i += 1;
            }
            Statement::Roll { count, face, name } => {
                insts.extend(count.codegen());
                insts.extend(face.codegen());
                insts.push(Inst::Roll);

                insts.push(Inst::Push(name.into()));
                insts.push(Inst::Store);
                i += 1;
            }
            Statement::Halt => {
                insts.push(Inst::Halt);
                i += 1;
            }
            Statement::Ill => {
                insts.push(Inst::Ill);
                i += 1;
            }
            Statement::Break => {
                todo!();
                i += 1;
            }
        }
    }
    insts
}
