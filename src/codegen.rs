/**
Generate x86 code from the Control flow graph.
**/

use std::collections::HashMap;
use crate::cfg::ELEMENT_SIZE;
use crate::{buildcfg::build_cfg, cfg::CFG};
use crate::utils::print::{self, print_cfg};
use crate::x86::*;
use crate::tac::*;


// TODO: 
// Use the CFGS to linearize all blocks starting from main 
// Convert linear blocks into x86 Assembly

// Remember to follow x86 calling convention:
//  - For now, everything on stack (no registers!)
//  - Args in %rdi, ...
//  - Save all caller saved registers
//  - call
// 
//  - callee function prologue (%bp, new stack frame)
//  - save callee saved registers
//  - function body
//  - callee function epilogue
//  - ret

/// Returns the x86 operand corresponding to operand
fn map_operand(method_cfg: &CFG, operand: &Operand, x86_instructions: &mut Vec<X86Insn>) -> X86Operand {    
    match operand {
        Operand::Const(val) => X86Operand::Constant(*val),
        Operand::LocalVar(temp) => X86Operand::RegInt(Register::Rbp, method_cfg.get_stack_offset(temp)),
        Operand::GlobalVar(val) => X86Operand::Global(val.to_string()),
        Operand::LocalArrElement(arr, idx) => {
            let idx_op = map_operand(method_cfg, idx, x86_instructions);
            x86_instructions.push(X86Insn::Lea(X86Operand::RegInt(Register::Rbp, method_cfg.get_stack_offset(arr)), X86Operand::Reg(Register::Rax))); // store base address of array in rax
            x86_instructions.push(X86Insn::Mov(idx_op, X86Operand::Reg(Register::R10))); // store index in r10
            X86Operand::Address(None, Some(Register::Rax), Register::R10, ELEMENT_SIZE)
        }
        Operand::GlobalArrElement(arr, idx) => {
            let idx_op = map_operand(method_cfg, idx, x86_instructions);
            x86_instructions.push(X86Insn::Mov(idx_op, X86Operand::Reg(Register::R10))); // store index in r10
            X86Operand::Address(Some(arr.to_string()), None, Register::R10, ELEMENT_SIZE)
        }
        Operand::Argument(pos) => {
            match pos {
                0 => X86Operand::Reg(Register::Rdi),
                1 => X86Operand::Reg(Register::Rsi),
                2 => X86Operand::Reg(Register::Rdx),
                3 => X86Operand::Reg(Register::Rcx),
                4 => X86Operand::Reg(Register::R8),
                5 => X86Operand::Reg(Register::R9),
                _ => todo!()
            }
        }
        Operand::String(idx) => X86Operand::RegLabel(Register::Rip, format!("str{idx}"))
    }
}

/// Adds the x86 instructions corresponding to insn to x86_instructions
fn add_instruction(method_cfg: &CFG, insn: &Instruction, x86_instructions: &mut Vec<X86Insn>) {
    match insn {
        Instruction::Add { left, right, dest } => {
            let left_op = map_operand(method_cfg, left, x86_instructions);
            let right_op = map_operand(method_cfg, right, x86_instructions);
            let dest_op = map_operand(method_cfg, dest, x86_instructions);

            // rax as working register
            x86_instructions.push(X86Insn::Mov(left_op, X86Operand::Reg(Register::Rax)));
            x86_instructions.push(X86Insn::Add(right_op, X86Operand::Reg(Register::Rax)));
            x86_instructions.push(X86Insn::Mov(X86Operand::Reg(Register::Rax), dest_op));
        }
        Instruction::Assign { src, dest } => {
            let dest_op = map_operand(method_cfg, dest, x86_instructions);
            let src_op = map_operand(method_cfg, src, x86_instructions);
            x86_instructions.push(X86Insn::Mov(src_op, dest_op));
        }
        Instruction::LoadString { src, dest } => {
            let dest_op = map_operand(method_cfg, dest, x86_instructions);
            let src_op = map_operand(method_cfg, src, x86_instructions);
            x86_instructions.push(X86Insn::Lea(src_op, dest_op));
        }
        _ => todo!()
    }
}

/// Emit x86 code corresponding to the given CFG
/// Returns a vector of strings of x86 instructions.
fn generate_method_x86(method_name: &String, method_cfg: &mut CFG) -> Vec<X86Insn>{
    let mut x86_instructions: Vec<X86Insn> = Vec::new();

    x86_instructions.push(X86Insn::Label(method_name.to_string()));

    // method prologue
    x86_instructions.push(X86Insn::Push(X86Operand::Reg(Register::Rbp))); // push base pointer onto stack
    x86_instructions.push(X86Insn::Mov(X86Operand::Reg(Register::Rsp), X86Operand::Reg(Register::Rbp))); // copy stack pointer to base pointer
    x86_instructions.push(X86Insn::Sub(X86Operand::Constant(method_cfg.stack_size), X86Operand::Reg(Register::Rsp))); // decrease stack pointer to allocate space on the stack

    for (id, block) in method_cfg.get_blocks() {

        x86_instructions.push(X86Insn::Label(method_name.to_string() + &id.to_string()));

        for insn in block.get_instructions() {
            add_instruction(method_cfg, &insn, &mut x86_instructions);
        }
    }

    // method epilogue
    x86_instructions.push(X86Insn::Mov(X86Operand::Reg(Register::Rbp), X86Operand::Reg(Register::Rsp))); // move base pointer to stack pointer
    x86_instructions.push(X86Insn::Pop(X86Operand::Reg(Register::Rbp))); // pop base pointer off stack
    x86_instructions.push(X86Insn::Ret); // return to where function was called
    
    x86_instructions
}


/// Generate x86 assembly code from the CFG/
pub fn generate_assembly(
    file: &str,
    filename: &str,
    writer: &mut dyn std::io::Write,
    debug: bool
) {

    // Generate the method CFGS
    let (method_cfgs, globals, strings) = build_cfg(file, filename, writer, debug);
    if debug {
        print_cfg(&method_cfgs);
    }

    // Generate a vector of x86 for each method
    let mut code: HashMap<String, Vec<X86Insn>> = HashMap::new();
    for (method_name, mut method_cfg) in method_cfgs {
        let method_code = generate_method_x86(&method_name, &mut method_cfg); 
        code.insert(method_name.clone(), method_code);
    }

    // Emit the final code
    writeln!(writer, "{}", "\n========== X86 Code ==========\n").expect("Failed to write instruction!");
    for (_, method_code) in &code {    
        for instr in method_code {
            writeln!(writer, "{}", instr).expect("Failed to write instruction!");
        }
    
        writeln!(writer).expect("Failed to write newline between methods!");
    }
    
}