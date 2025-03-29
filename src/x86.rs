/**
Data structures for x86 code generation.
**/

use std::fmt;

#[derive(Debug, Clone)]
pub enum X86Insn {
    Mov(X86Operand, X86Operand),
    Add(X86Operand, X86Operand),
    Sub(X86Operand, X86Operand),
    Call(String),
    Label(String),
    // TODO: complete
}

#[derive(Debug, Clone)]
pub enum X86Operand {
    Reg(Register), // no offset
    RegInt(Register, i32), // integer offset
    RegLabel(Register, String), // label offset
    Constant(i64),
    Global(String), // name of global constant
    // GlobalOffset(String, i32) // integer offset
}

impl fmt::Display for X86Operand {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            X86Operand::Reg(reg) => write!(f, "{}", reg),
            X86Operand::Constant(val) => write!(f, "${}", val),
            X86Operand::Global(name) => write!(f, "{}", name),
            X86Operand::RegInt(reg, offset) => write!(f, "{}({})", offset, reg),
            X86Operand::RegLabel(reg, label) => write!(f, "{}({})", label, reg),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Register {
    Rdi,
    Rsi,
    Rdx,
    Rcx,
    R8,
    R9,
    R10,
    Rbp,
    Rsp,
    Rax
}

impl fmt::Display for Register {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Register::Rdi => write!(f, "%rdi"),
            Register::Rsi => write!(f, "%rsi"),
            Register::Rdx => write!(f, "%rdx"),
            Register::Rcx => write!(f, "%rcx"),
            Register::R8 => write!(f, "%r8"),
            Register::R9 => write!(f, "%r9"),
            Register::R10 => write!(f, "%r10"),
            Register::Rbp => write!(f, "%rbp"),
            Register::Rsp => write!(f, "%rsp"),
            Register::Rax => write!(f, "%rax")
        }
    }
}

// TODO: complete
impl fmt::Display for X86Insn {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            X86Insn::Mov(dst, src) => write!(f, "    movq {}, {}", dst, src),
            X86Insn::Add(dst, src) => write!(f, "    addq {}, {}", dst, src),
            X86Insn::Sub(dst, src) => write!(f, "    subq {}, {}", dst, src),
            X86Insn::Call(label)   => write!(f, "    call {}", label),
            X86Insn::Label(name)   => write!(f, "{}:", name),
        }
    }
}