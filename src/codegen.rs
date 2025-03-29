/**
Generate x86 code from the Control flow graph.
**/

use std::collections::HashMap;
use crate::cfg::BasicBlock;
use crate::tac::Instruction;
use crate::{buildcfg::build_cfg, cfg::CFG};
use crate::utils::print::print_cfg;
use crate::x86::X86Instr;
// use crate::tac::*;


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


// /// Linerize the CFG into an instrruction list
// /// suitable for code generation.
// fn linearize_cfg(cfg: &CFG) -> Vec<Instruction>{

//     // TODO

//     // Iterate over CFG to flatten it
//     for (id, block) in cfg.blocks.clone() {
//     }

//     todo!()

// }

// Convienient helper for storing top of stack into a dest operand
fn store_top_to(dest: &Operand) -> Vec<X86Instr> {
    let dest_str = operand_to_x86(dest);
    vec![
        X86Instr::Pop("%rax".into()),                 // Pop result into %rax
        X86Instr::Mov(dest_str, "%rax".into()),      // Store into destination
    ]
}

fn instruction_to_x86(instruction: Instruction) -> Vec<X86Instr> {
    match instruction {
        Instruction::Add { left, right, dest } => {
            vec![
                X86Instr::Comment(format!("Begin Add: {} = {} + {}", dest, left, right)),
                X86Instr::Mov("%rax".into(), left),     
                X86Instr::Add("%rax".into(), right), // rax = left + right
                X86Instr::Mov(dest_str, "%rax".into()), 
                X86Instr::Comment(format!("End Add")),
            ]
        },
        Instruction::Subtract { left, right, dest } => {
            vec![
                X86Instr::Comment(format!("Begin Sub: {} = {} - {}", dest, left, right)),
                X86Instr::Mov("%rax".into(), left),     
                X86Instr::Sub("%rax".into(), right), // rax = left - right
                X86Instr::Mov(dest_str, "%rax".into()), 
                X86Instr::Comment(format!("End Sub")),
            ]
        },
        Instruction::Multiply { left, right, dest } => {
            vec![
                X86Instr::Comment(format!("Begin Mul: {} = {} * {}", dest, left, right)),
                X86Instr::Mov("%rax".into(), left),
                X86Instr::Mul("%rax".into(), right), // rax = rax * right
                X86Instr::Mov(dest_str, "%rax".into()), 
                X86Instr::Comment(format!("End Mul")),
            ]
        },
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
        Instruction::Branch { condition, true_target, false_target } => todo!(),
        Instruction::Ret { value } => todo!(),
        Instruction::Assign { src, dest } => todo!(),
        Instruction::Load { src, dest } => todo!(),
        Instruction::Store { src, dest } => todo!(),
    }
}


fn generate_block_x86(bblock: BasicBlock) -> Vec<X86Instr> {    // TODO make this into a simple map
    let mut instructions: Vec<X86Instr> =  Vec::new();
    for instruction in bblock.get_instructions() {
        instructions.append(&mut instruction_to_x86(instruction));
    }
    instructions
}

fn generate_method_x86(cfg: &CFG) -> Vec<X86Instr> {
    let mut instructions: Vec<X86Instr> = Vec::new();
    // TODO set up method call

    let blocks: std::collections::BTreeMap<i32, BasicBlock> = cfg.clone().get_blocks();
    for (number, bblock) in blocks {
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
    let method_cfgs: HashMap<String, CFG> = build_cfg(file, filename, writer, debug);
    if debug {
        print_cfg(&method_cfgs);
    }

    // Generate a vector of x86 for each method
    let mut code: HashMap<String, Vec<X86Instr>> = HashMap::new();
    for (method_name, method_cfg) in &method_cfgs {
        let method_code = generate_method_x86(method_cfg); 
        code.insert(method_name.clone(), method_code);
    }

    // // Emit the final code
    // for (method_name, method_code) in &code {
    //     writeln!(writer, "{}:", method_name).expect("Failed to write label!");
    
    //     for instr in method_code {
    //         writeln!(writer, "{}", instr).expect("Failed to write instruction!");
    //     }
    
    //     writeln!(writer).expect("Failed to write newline between methods!");
    // }
    
}