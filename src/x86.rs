/**
Data structures for x86 code generation.
**/

use std::collections::HashMap;
use std::fmt;


#[allow(dead_code)]
#[derive(Debug, Clone)]
pub enum X86Instr {
    Mov(String, String),
    Add(String, String),
    Sub(String, String),
    Call(String),
    Label(String),
    Push(String),
    Pop(String),
    Mul(String),
    // TODO: complete
    Comment(String)
}


// TODO: complete
impl fmt::Display for X86Instr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            X86Instr::Mov(dst, src) => write!(f, "    movq {}, {}", dst, src),
            X86Instr::Add(dst, src) => write!(f, "    addq {}, {}", dst, src),
            X86Instr::Sub(dst, src) => write!(f, "    subq {}, {}", dst, src),
            X86Instr::Call(label)   => write!(f, "    call {}", label),
            X86Instr::Label(name)   => write!(f, "{}:", name),
            X86Instr::Comment(message) => write!(f, "# {}", message),
            X86Instr::Push(name) => write!(f, "    push {}", name),
            X86Instr::Pop(name) => write!(f, "    pop {}", name),
            X86Instr::Mul(dst, src) => write!(f, "    push {}", name),
        }
    }
}



/// Data structure to keep track of stack
/// offsets for variables.
#[allow(dead_code)]
#[derive(Debug, Clone)]
pub struct Stack {
    sp: i32,
    offsets: HashMap<String, i32>,
}