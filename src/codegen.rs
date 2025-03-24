use std::collections::HashMap;

/**
Generate x86 code from the Control flow graph.
**/

use crate::{buildcfg::build_cfg, cfg::CFG, linear_ir::Instruction};

fn print_cfg(method_cfgs: HashMap<String, CFG>) {
    for (method_name, cfg) in method_cfgs {
        println!("\n{method_name}:");

        for (id, block) in cfg.get_blocks() {
            println!("    {id}:");

            for insn in block.get_instructions() {
                match insn {
                    Instruction::Add { left, right, dest } => { println!("        {dest} <- {left} + {right}"); }
                    Instruction::And { left, right, dest } => { println!("        {dest} <- {left} && {right}"); }
                    Instruction::ArrAccess { array, index, dest } => { println!("        {dest} <- {array}[{index}]"); }
                    Instruction::Assign { src, dest } => { println!("        {dest} <- {src}"); }
                    Instruction::Branch { condition, true_target, false_target } => { println!("        branch {condition}, {true_target}, {false_target}"); }
                    Instruction::Cast { expr, dest, target_type } => { println!("        {dest} <- {target_type}({expr})"); }
                    Instruction::Divide { left, right, dest } => { println!("        {dest} <- {left} / {right}"); }
                    Instruction::Equal { left, right, dest } => { println!("        {dest} <- {left} == {right}"); }
                    Instruction::Greater { left, right, dest } => { println!("        {dest} <- {left} > {right}"); }
                    Instruction::GreaterEqual { left, right, dest } => { println!("        {dest} <- {left} >= {right}"); }
                    Instruction::Len { expr, dest } => { println!("        {dest} <- len({expr})"); }
                    Instruction::Less { left, right, dest } => { println!("        {dest} <- {left} < {right}"); }
                    Instruction::LessEqual { left, right, dest } => { println!("        {dest} <- {left} <= {right}"); }
                    Instruction::MethodCall { name, args, dest } => { 
                        let args_string = args.iter().map(|op| op.to_string()).collect::<Vec<_>>().join(", ");
                        if dest.is_some() { 
                            let dest_string = dest.unwrap();
                            println!("        {dest_string} <- {name}({args_string})"); 
                        }
                        else { println!("        {name}({args_string})"); }
                    }
                    Instruction::Modulo { left, right, dest } => { println!("        {dest} <- {left} % {right}"); }
                    Instruction::Multiply { left, right, dest } => { println!("        {dest} <- {left} * {right}"); }
                    Instruction::Neg { expr, dest } => { println!("        {dest} <- -{expr}"); }
                    Instruction::Nop => { println!("        nop"); }
                    Instruction::Not { expr, dest } => { println!("        {dest} <- !{expr}"); }
                    Instruction::NotEqual { left, right, dest } => { println!("        {dest} <- {left} != {right}"); }
                    Instruction::Or { left, right, dest } => { println!("        {dest} <- {left} || {right}"); }
                    Instruction::Ret { value } => { println!("        ret"); }
                    Instruction::Subtract { left, right, dest } => { println!("        {dest} <- {left} - {right}"); }
                    Instruction::UJmp { id } => { println!("        jmp {id}"); }
                }
            }
        }
    }
}

/// Generate x86 assembly code from the CFG/
pub fn generate_assembly(
    file: &str,
    filename: &str,
    writer: &mut dyn std::io::Write,
    debug: bool
) {
    let method_cfgs: HashMap<String, CFG> = build_cfg(file, filename, writer, debug);

    if debug {
        print_cfg(method_cfgs);
    }

    // writeln!(writer, "{}", "Here is your code").expect("Failed to write output!");

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
}