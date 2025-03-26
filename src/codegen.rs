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


/// Linerize the CFG into an instrruction list
/// suitable for code generation.
fn linearize_cfg(cfg: &CFG) -> Vec<Instruction>{

    // TODO

    // Iterate over CFG to flatten it
    for (id, block) in cfg.blocks.clone() {
    }

    todo!()

}


/// Emit x86 code corresponding to the given CFG
/// Returns a vector of strings of x86 instructions.
fn generate_method_x86(linear_method: &Vec<Instruction>) -> Vec<X86Instr>{
    let mut x86_instructions: Vec<X86Instr> = Vec::new();


    // TODO: initial pass over blocks to precompute
    // stack offsets

    
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

    // Generate the linearized IR for each method
    let mut linearized_methods: HashMap<String, Vec<Instruction>> = HashMap::new();
    for (method_name, cfg) in &method_cfgs {
        let instructions = linearize_cfg(cfg); 
        linearized_methods.insert(method_name.clone(), instructions);
    }

    // Generate a vector of x86 for each method
    let mut code: HashMap<String, Vec<X86Instr>> = HashMap::new();
    for (method_name, linear_method) in &linearized_methods {
        let method_code = generate_method_x86(linear_method); 
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