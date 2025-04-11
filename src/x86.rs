/**
Data structures for x86 code generation.
**/

use std::fmt;
use crate::ast::Type;

#[derive(Debug, Clone)]
pub enum X86Insn {
    // Type differentiates between 32 and 64-bit instructions
    Mov(X86Operand, X86Operand, Type),
    Movzbq(X86Operand, X86Operand),
    Add(X86Operand, X86Operand, Type),
    Sub(X86Operand, X86Operand, Type),
    Mul(X86Operand, X86Operand),
    Div(X86Operand),
    Cdq,
    Cqto,
    Xor(X86Operand, X86Operand),
    Or(X86Operand, X86Operand),
    Shl(X86Operand, X86Operand),
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
    Global(String),
    Exit
}

#[derive(Debug, Clone)]
pub enum X86Operand {
    // memory operands need type to determine how much to read/write
    Reg(Register),              // no offset
    RegInt(Register, i64, Type),      // integer offset
    RegLabel(Register, String), // label offset
    Constant(i64),
    Global(String), // name of global constant
    Address(Option<String>, Option<Register>, Register, i64, Type), // reg1 + reg2 * scale, offset by label
}

impl X86Operand {
    pub fn get_type(&self) -> Type {
        match self {
            X86Operand::RegInt(_, _, typ) => typ.clone(),
            X86Operand::Address(_, _, _, _, typ) => typ.clone(),
            _ => Type::Long, // default fallback
        }
    }
}

impl fmt::Display for X86Operand {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            X86Operand::Reg(reg) => write!(f, "{}", reg),
            X86Operand::Constant(val) => write!(f, "${}", val),
            X86Operand::Global(name) => write!(f, "{}", name),
            X86Operand::RegInt(reg, offset, typ) => write!(f, "{}({})", offset, reg),
            X86Operand::RegLabel(reg, label) => write!(f, "{}({})", label, reg),
            X86Operand::Address(label, reg1, reg2, scale, typ) => {
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
    R11,
    Rbp,
    Rsp,
    Rax,
    Al,
    Rip,

    Ebx,
    Edi,
    Esi,
    Edx,
    Ecx,
    R8d,
    R9d,
    R10d,
    R11d,
    Ebp,
    Esp,
    Eax,
    Rbx
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
            Register::R11 => write!(f, "%r11"),
            Register::Rbp => write!(f, "%rbp"),
            Register::Rsp => write!(f, "%rsp"),
            Register::Rax => write!(f, "%rax"),
            Register::Al => write!(f, "%al"),
            Register::Rip => write!(f, "%rip"),
            Register::Rbx => write!(f, "%rbx"),

            Register::Ebx => write!(f, "%ebx"),
            Register::Edi => write!(f, "%edi"),
            Register::Esi => write!(f, "%esi"),
            Register::Edx => write!(f, "%edx"),
            Register::Ecx => write!(f, "%ecx"),
            Register::R8d => write!(f, "%r8d"),
            Register::R9d => write!(f, "%r9d"),
            Register::R10d => write!(f, "%r10d"),
            Register::R11d => write!(f, "%r11d"),
            Register::Ebp => write!(f, "%ebp"),
            Register::Esp => write!(f, "%esp"),
            Register::Eax => write!(f, "%eax"),
        }
    }
}

impl fmt::Display for X86Insn {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let suffix = |typ: &Type| match typ {
            Type::Int => "l",
            Type::Long => "q",
            Type::Bool => "l",
            _ => "q", // default to 64-bit
        };

        match self {
            X86Insn::Mov(src, dst, typ) => {write!(f, "    mov{} {}, {}", suffix(typ), src, dst)}
            X86Insn::Movzbq(src, dst) => {write!(f, "    movzbq {}, {}", src, dst) }
            X86Insn::Add(src, dst, typ) => {write!(f, "    add{} {}, {}", suffix(typ), src, dst) }
            X86Insn::Sub(src, dst, typ) => {write!(f, "    sub{} {}, {}", suffix(typ), src, dst) }
            X86Insn::Mul(src, dst, ..) => {write!(f, "    imul {}, {}", src, dst) } // `imul` has same mnemonic for int/long 
            X86Insn::Div(divisor, ..) => {write!(f, "    idiv {}", divisor)}
            X86Insn::Cdq => write!(f, "    cdq"),
            X86Insn::Cqto => write!(f, "    cqto"),
            X86Insn::Xor(src, dst) => write!(f, "    xor {}, {}", src, dst),
            X86Insn::Call(label) => write!(f, "    call {}", label),
            X86Insn::Label(name) => write!(f, "{}:", name),
            X86Insn::Jmp(label) => write!(f, "    jmp {}", label),
            X86Insn::Push(op) => write!(f, "    pushq {}", op),
            X86Insn::Pop(op) => write!(f, "    popq {}", op),
            X86Insn::Ret => write!(f, "    ret"),
            X86Insn::Lea(src, dst) => write!(f, "    leaq {}, {}", src, dst),
            X86Insn::Cmp(left, right) => write!(f, "    cmpl {}, {}", left, right),
            X86Insn::Jne(label) => writeln!(f, "    jne {}", label),
            X86Insn::Sete(dst) => write!(f, "    sete {}", dst),
            X86Insn::Setg(dst) => write!(f, "    setg {}", dst),
            X86Insn::Setge(dst) => write!(f, "    setge {}", dst),
            X86Insn::Setl(dst) => write!(f, "    setl {}", dst),
            X86Insn::Setle(dst) => write!(f, "    setle {}", dst),
            X86Insn::Setne(dst) => write!(f, "    setne {}", dst),
            X86Insn::Exit => write!(f, "    call exit"),
            X86Insn::Comm(name, size, alignment) => write!(f, ".comm {name}, {size}, {alignment}"),
            X86Insn::String(val) => write!(f, "    .string \"{val}\""),
            X86Insn::Global(val) => write!(f, ".globl {val}"),
            X86Insn::Or(src, dst) => write!(f, "    orq {src}, {dst}"),
            X86Insn::Shl(src, dst) => write!(f, "    shlq {src}, {dst}"),
        }
    }
}