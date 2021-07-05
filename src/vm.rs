use crate::codegen::{Asm, Inst, Val};
use crate::types::IntType;
use std::collections::HashMap;

struct VM {
    val_stack: Vec<Val>,
    var_map: HashMap<String, Val>,
}

impl VM {
    fn new() -> Self {
        Self {
            val_stack: Vec::new(),
            var_map: HashMap::new(),
        }
    }

    fn pop(&mut self) -> Val {
        self.val_stack.pop().expect("stack is empty")
    }

    fn pop_str(&mut self) -> String {
        let v = self.pop();
        if let Val::Str(s) = v {
            s
        } else {
            panic!("str expected");
        }
    }

    fn push(&mut self, v: Val) {
        self.val_stack.push(v)
    }

    fn store(&mut self) {
        let name = self.pop_str();
        let val = self.pop();
        self.var_map.insert(name, val);
    }

    fn load(&mut self) {
        let name = self.pop_str();
        self.push(
            self.var_map
                .get(&name)
                .expect("variable is not found")
                .clone(),
        );
    }

    fn unary<F>(&mut self, op: F)
    where
        F: Fn(Val) -> Val,
    {
        let v = self.pop();
        self.push(op(v))
    }

    fn binary<F>(&mut self, op: F)
    where
        F: Fn(Val, Val) -> Val,
    {
        let v2 = self.pop();
        let v1 = self.pop();
        self.push(op(v1, v2))
    }
}

pub fn run(asm: Asm) {
    let insts = asm.insts;
    let mut vm = VM::new();
    let mut i = 1;
    while i < insts.len() {
        match insts[i] {
            Inst::Push(ref v) => vm.push(v.clone()),
            Inst::Store => vm.store(),
            Inst::Load => vm.load(),
            Inst::Add => vm.binary(|v1, v2| match (v1, v2) {
                (Val::Num(this), Val::Num(that)) => match this.checked_add(that) {
                    Some(n) => Val::Num(n),
                    None => panic!("overflow in add"),
                },
                (Val::Str(this), Val::Str(that)) => Val::Str(this.clone() + &that),
                _ => panic!("type error in add"),
            }),
            Inst::Sub => vm.binary(|v1, v2| match (v1, v2) {
                (Val::Num(this), Val::Num(that)) => match this.checked_sub(that) {
                    Some(n) => Val::Num(n),
                    None => panic!("overflow in sub"),
                },
                _ => panic!("type error in sub"),
            }),
            Inst::Mul => vm.binary(|v1, v2| match (v1, v2) {
                (Val::Num(this), Val::Num(that)) => match this.checked_add(that) {
                    Some(n) => Val::Num(n),
                    None => panic!("overflow in sub"),
                },
                (Val::Num(n), Val::Str(s)) | (Val::Str(s), Val::Num(n)) => {
                    Val::Str(s.repeat(n as usize))
                }
                _ => panic!("type error in mul"),
            }),
            Inst::Div => vm.binary(|v1, v2| match (v1, v2) {
                (Val::Num(this), Val::Num(that)) => match this.checked_div(that) {
                    Some(n) => Val::Num(n),
                    None => panic!("overflow in div"),
                },
                _ => panic!("type error in div"),
            }),
            Inst::Mod => vm.binary(|v1, v2| match (v1, v2) {
                (Val::Num(this), Val::Num(that)) => match this.checked_rem(that) {
                    Some(n) => Val::Num(n),
                    None => panic!("overflow in mod"),
                },
                _ => panic!("type error in mod"),
            }),
            Inst::Neg => vm.unary(|v| match v {
                Val::Bool(b) => Val::Bool(!b),
                Val::Num(n) => Val::Num(-n),
                Val::Str(s) => Val::Str(s.chars().rev().collect()),
            }),
            Inst::Print => {
                // FIXME
                let v = vm.pop();
                eprintln!("{:?}", v);
            }
            _ => todo!(),
        }
        i += 1;
    }
}
