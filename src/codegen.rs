/**
Generate x86 code from the Control flow graph.
**/

use std::collections::HashMap;
use crate::cfg::{BasicBlock};
use crate::token::Literal;
use crate::{buildcfg::build_cfg, cfg::CFG};
use crate::utils::print::print_cfg;
use crate::x86::X86Instr;
use crate::tac::*;


fn literal_to_x86(lit: &Literal) -> String {
    match lit {
        Literal::Char(c) => format!("${}", *c as u8),
        Literal::String(s) => format!("${}", s),
        Literal::Int(s) => format!("${}", s),
        Literal::Long(s) => format!("${}", s),
        Literal::HexInt(s) => format!("${}", s),
        Literal::HexLong(s) => format!("${}", s),
        Literal::Bool(true) => "$1".to_string(),
        Literal::Bool(false) => "$0".to_string(),
    }
}


fn operand_to_x86(op: &Operand) -> String {
    match op {
        Operand::GlobalVar(name) => format!("{}", name),
        Operand::GlobalArrElement(name, index) => {
            let index_str = operand_to_x86(index);
            format!("{}({})", name, index_str)
        }
        Operand::String(id) => format!(".S{}", id),
        Operand::LocalVar(name) => format!("-{}(%rbp)", name),
        Operand::LocalArrElement(name, index) => {
            let index_str = operand_to_x86(index);
            format!("-{}({})", name, index_str)
        }
        Operand::Const(lit) => literal_to_x86(lit),
        Operand::Argument(pos) => {
            // System V x86_64 calling convention: first 6 arguments are in registers
            let arg_regs = ["%rdi", "%rsi", "%rdx", "%rcx", "%r8", "%r9"];
            if *pos < arg_regs.len() {
                arg_regs[*pos].to_string()
            } else {
                // After the 6th argument, they are pushed on stack (simplified)
                let offset = 16 + 8 * (pos - arg_regs.len()); // after return addr and old RBP
                format!("{}(%rbp)", offset)     // TODO is this right way to handle arguments
            }
        }
    }
}


fn instruction_to_x86(instruction: &Instruction) -> Vec<X86Instr> {
    match instruction {
        Instruction::Add { left, right, dest } => {
                        let left_str = operand_to_x86(left);
                        let right_str = operand_to_x86(right);
                        let dest_str = operand_to_x86(dest);
                        vec![
                            X86Instr::Comment(format!("Begin Add: {} = {} + {}", dest, left, right)),
                            X86Instr::Mov("%rax".into(), left_str),
                            X86Instr::Add("%rax".into(), right_str),
                            X86Instr::Mov(dest_str, "%rax".into()),
                            X86Instr::Comment("End Add".into()),
                        ]
            }
        Instruction::Subtract { left, right, dest } => {
                let left_str = operand_to_x86(left);
                let right_str = operand_to_x86(right);
                let dest_str = operand_to_x86(dest);
                vec![
                    X86Instr::Comment(format!("Begin Sub: {} = {} - {}", dest, left, right)),
                    X86Instr::Mov("%rax".into(), left_str),
                    X86Instr::Sub("%rax".into(), right_str),
                    X86Instr::Mov(dest_str, "%rax".into()),
                    X86Instr::Comment("End Sub".into()),
                ]
            }
        Instruction::Multiply { left, right, dest } => {
                let left_str = operand_to_x86(left);
                let right_str = operand_to_x86(right);
                let dest_str = operand_to_x86(dest);
                vec![
                    X86Instr::Comment(format!("Begin Mul: {} = {} * {}", dest, left, right)),
                    X86Instr::Mov("%rax".into(), left_str),
                    X86Instr::Mul("%rax".into(), right_str),
                    X86Instr::Mov(dest_str, "%rax".into()),
                    X86Instr::Comment("End Mul".into()),
                ]
            }
        Instruction::Divide { left, right, dest } => todo!(),
        Instruction::Modulo { left, right, dest } => todo!(),
        Instruction::Not { expr, dest } => todo!(),
        Instruction::Cast { expr, dest, target_type } => todo!(),
        Instruction::Len { expr, dest } => todo!(),
        Instruction::Greater { left, right, dest } => todo!(),
        Instruction::Less { left, right, dest } => todo!(),
        Instruction::LessEqual { left, right, dest } => todo!(),
        Instruction::GreaterEqual { left, right, dest } => todo!(),
        Instruction::Equal { left, right, dest } => todo!(),
        Instruction::NotEqual { left, right, dest } => todo!(),
        Instruction::MethodCall { name, args, dest } => todo!(),
        Instruction::UJmp { id } => todo!(),
        Instruction::Cmp { arg1, arg2 } => todo!(),
        Instruction::CJmp { condition, target_id } => todo!(),
        Instruction::Ret { value } => todo!(),
        Instruction::Assign { src, dest } => {
            let src_str = operand_to_x86(src);
            let dest_str = operand_to_x86(dest);
            vec![X86Instr::Mov(dest_str, src_str)]
        },
        Instruction::LoadString { src, dest } => todo!(),
    }
}


fn generate_block_x86(bblock: BasicBlock) -> Vec<X86Instr> {
    let mut instructions: Vec<X86Instr> = Vec::new();
    for instruction in bblock.get_instructions() {
        instructions.append(&mut instruction_to_x86(&instruction));
    }
    instructions
}

fn generate_method_x86(cfg: &CFG) -> Vec<X86Instr> {
    let mut instructions: Vec<X86Instr> = Vec::new();
    // TODO set up method call

    let blocks: std::collections::BTreeMap<i32, BasicBlock> = cfg.clone().get_blocks();
    for (_number, bblock) in blocks {
        instructions.append(&mut generate_block_x86(bblock));
    }

    // TODO end method call cleanup
    instructions
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
    let mut code: HashMap<String, Vec<X86Instr>> = HashMap::new();
    for (method_name, method_cfg) in &method_cfgs {
        let method_code = generate_method_x86(method_cfg); 
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