/**
Data structures for x86 code generation.
**/

use std::fmt;

#[derive(Debug, Clone)]
pub enum X86Insn {
    Mov(X86Operand, X86Operand),
    Movzbq(X86Operand, X86Operand),
    Add(X86Operand, X86Operand),
    Sub(X86Operand, X86Operand),
    Mul(X86Operand, X86Operand),
    Div(X86Operand),
    Xor(X86Operand, X86Operand),
    Call(String),
    Label(String),
    Jmp(String),
    Push(X86Operand),
    Pop(X86Operand),
    Ret,
    Lea(X86Operand, X86Operand),
    Cmp(X86Operand, X86Operand),
    Jne(String),
    Sete(X86Operand),
    Setg(X86Operand),
    Setge(X86Operand),
    Setl(X86Operand),
    Setle(X86Operand),
    Setne(X86Operand),
    Comm(String, i64, i64), // name, size, alignment
    String(String),
    Global(String)
}

#[derive(Debug, Clone)]
pub enum X86Operand {
    Reg(Register),              // no offset
    RegInt(Register, i64),      // integer offset
    RegLabel(Register, String), // label offset
    Constant(i64),
    Global(String), // name of global constant
    Address(Option<String>, Option<Register>, Register, i64), // reg1 + reg2 * scale, offset by label
}

impl fmt::Display for X86Operand {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            X86Operand::Reg(reg) => write!(f, "{}", reg),
            X86Operand::Constant(val) => write!(f, "${}", val),
            X86Operand::Global(name) => write!(f, "{}", name),
            X86Operand::RegInt(reg, offset) => write!(f, "{}({})", offset, reg),
            X86Operand::RegLabel(reg, label) => write!(f, "{}({})", label, reg),
            X86Operand::Address(label, reg1, reg2, scale) => {
                write!(
                    f,
                    "{}({}, {}, {})",
                    if label.is_some() {
                        label.clone().unwrap()
                    } else {
                        "".to_string()
                    },
                    if reg1.is_some() {
                        reg1.as_ref().unwrap().to_string()
                    } else {
                        "".to_string()
                    },
                    reg2,
                    scale
                )
            }
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
    Rax,
    Al,
    Rip,
    R11
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
            Register::Rax => write!(f, "%rax"),
            Register::Al => write!(f, "%al"),
            Register::Rip => write!(f, "%rip"),
            Register::R11 => write!(f, "%r11"),
        }
    }
}

impl fmt::Display for X86Insn {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            X86Insn::Mov(src, dst) => write!(f, "    movq {}, {}", src, dst),
            X86Insn::Movzbq(src, dst) => write!(f, "    movzbq {}, {}", src, dst),
            X86Insn::Add(src, dst) => write!(f, "    addq {}, {}", src, dst),
            X86Insn::Sub(src, dst) => write!(f, "    subq {}, {}", src, dst),
            X86Insn::Mul(src, dst) => write!(f, "    imul {}, {}", src, dst),
            X86Insn::Div(divisor) => write!(f, "    idiv {}", divisor),
            X86Insn::Xor(src, dst) => write!(f, "    xor {}, {}", src, dst),
            X86Insn::Call(label) => write!(f, "    call {}", label),
            X86Insn::Label(name) => write!(f, "{}:", name),
            X86Insn::Jmp(label) => write!(f, "    jmp {}", label),
            X86Insn::Push(op) => write!(f, "    pushq {}", op),
            X86Insn::Pop(op) => write!(f, "    popq {}", op),
            X86Insn::Ret => write!(f, "    ret"),
            X86Insn::Lea(src, dst) => write!(f, "    leaq {}, {}", src, dst),
            X86Insn::Cmp(left, right) => write!(f, "    cmpq {}, {}", left, right),
            X86Insn::Jne(label) => writeln!(f, "    jne {}", label),
            X86Insn::Sete(dst) => write!(f, "    sete {}", dst),
            X86Insn::Setg(dst) => write!(f, "    setg {}", dst),
            X86Insn::Setge(dst) => write!(f, "    setge {}", dst),
            X86Insn::Setl(dst) => write!(f, "    setl {}", dst),
            X86Insn::Setle(dst) => write!(f, "    setle {}", dst),
            X86Insn::Setne(dst) => write!(f, "    setne {}", dst),
            X86Insn::Comm(name, size, alignment) => write!(f, ".comm {name}, {size}, {alignment}"),
            X86Insn::String(val) => write!(f, "    .string \"{val}\""),
            X86Insn::Global(val) => write!(f, ".globl {val}"),
        }
    }
}