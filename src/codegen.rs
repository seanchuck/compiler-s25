use std::collections::HashMap;

/**
Generate x86 code from the Control flow graph.
**/

use crate::{buildcfg::build_cfg, cfg::CFG};




/// Generate x86 assembly code from the CFG/
pub fn generate_assembly(
    file: &str,
    filename: &str,
    writer: &mut dyn std::io::Write,
    debug: bool
) {

    let method_cfgs: HashMap<String, CFG> = build_cfg(file, filename, writer, debug);

    writeln!(writer, "{}", "Here is your code").expect("Failed to write output!");

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