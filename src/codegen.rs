/**
Generate x86 code from the Control flow graph.
**/

use std::collections::HashMap;
use crate::{buildcfg::build_cfg, cfg::CFG};
use crate::utils::print::print_cfg;
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


/// Emit x86 code corresponding to the given CFG
/// Returns a vector of strings of x86 instructions.
fn generate_method_x86(method_name: &String, method_cfg: &mut CFG) -> Vec<X86Insn>{
    let mut x86_instructions: Vec<X86Insn> = Vec::new();

    x86_instructions.push(X86Insn::Label(method_name.to_string()));

    // TODO: method prolog

    for (id, block) in method_cfg.get_blocks() {

        x86_instructions.push(X86Insn::Label(method_name.to_string() + &id.to_string()));

        for insn in block.get_instructions() {
            match insn {
                Instruction::Add { left, right, dest } => {
                    let left_op = match left {
                        Operand::Const(val) => X86Operand::Constant(*val),
                        Operand::LocalVar(temp) => X86Operand::RegInt(Register::Rbp, method_cfg.get_stack_offset(temp)),
                        Operand::GlobalVar(val) => X86Operand::Global(val.to_string()),
                        _ => unreachable!()
                    };
                    let right_op = match right {
                        Operand::Const(val) => X86Operand::Constant(*val),
                        Operand::LocalVar(temp) => X86Operand::RegInt(Register::Rbp, method_cfg.get_stack_offset(temp)),
                        Operand::GlobalVar(val) => X86Operand::Global(val.to_string()),
                        _ => unreachable!()
                    };
                    let dest_op = match dest {
                        Operand::LocalVar(temp) => X86Operand::RegInt(Register::Rbp, method_cfg.get_stack_offset(temp)),
                        Operand::GlobalVar(val) => X86Operand::Global(val.to_string()),
                        _ => unreachable!()
                    };

                    // rax as working register
                    x86_instructions.push(X86Insn::Mov(left_op, X86Operand::Reg(Register::Rax)));
                    x86_instructions.push(X86Insn::Add(right_op, X86Operand::Reg(Register::Rax)));
                    x86_instructions.push(X86Insn::Add(X86Operand::Reg(Register::Rax), dest_op));
                }
                _ => todo!()
            }
        }
    }

    // TODO: method epilog
    
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
    for (method_name, method_code) in &code {
        writeln!(writer, "{}:", method_name).expect("Failed to write label!");
    
        for instr in method_code {
            writeln!(writer, "{}", instr).expect("Failed to write instruction!");
        }
    
        writeln!(writer).expect("Failed to write newline between methods!");
    }
    
}