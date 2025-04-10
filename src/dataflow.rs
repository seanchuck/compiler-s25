/**
Dataflow code generation optimizations.
*/
use std::collections::{HashMap, HashSet};
use crate::{
    cfg::CFG,
    utils::cli::Optimization,
    tac::*,
};


/// Invalidate the table entries for a variable whose
/// value has been updated
fn invalidate(
    dest_name: &str,
    copy_to_src: &mut HashMap<String, String>,
    src_to_copies: &mut HashMap<String, HashSet<String>>,
) {
    
    // Remove dest from its source's copy set
    if let Some(src) = copy_to_src.remove(dest_name) {
        if let Some(set) = src_to_copies.get_mut(&src) {
            set.remove(dest_name);
        }
    }

    // If dest was a source, remove all dependent copies
    if let Some(dependents) = src_to_copies.remove(dest_name) {
        for dependent in dependents {
            copy_to_src.remove(&dependent);
        }
    }

    println!("Invalidated mutated variable: {}", dest_name);
}

/// Recursively get the root source that a copy refers to
fn get_root_source(
    var_name: &str,
    copy_to_src: &HashMap<String, String>,
) -> String {
    let mut current = var_name;
    while let Some(next) = copy_to_src.get(current) {
        current = next;
    }
    current.to_string()
}


fn substitute_operand(op: &mut Operand, copy_to_src: &HashMap<String, String>) {
    // TODO: make sure this is properly mutating the CFG
    match op {
        Operand::LocalVar(name) => {
            let root = get_root_source(name, copy_to_src);
            if *name != root {
                *op = Operand::LocalVar(root);
            }
        }

        Operand::GlobalVar(name) => {
            let root = get_root_source(name, copy_to_src);
            if *name != root {
                *op = Operand::GlobalVar(root);
            }
        }

        Operand::LocalArrElement(_, index) | Operand::GlobalArrElement(_, index) => {
            substitute_operand(index, copy_to_src);
        }

        _ => {
            // Const, String, Argument – do nothing
        }
    }
}



fn copy_propagation(method_cfg: &mut CFG) -> bool {
    // If a variable is assigned directly from another variable, 
    // and the value hasn't changed, 
    // we can replace the copied variable with the original one.

    // identify any copy assignments a = b, a = c and make a table
    // mapping each var to its "source" {a: [b, c]}

    // Maps a copy to its source
    // b = a; --> copy_to_src = {b : a} = {dest : src}
    let mut copy_to_src: HashMap<String, String> = HashMap::new();

    // Maps src to set
    // b = a; c = a; --> src_to_copies = {b}
    let mut src_to_copies: HashMap<String, HashSet<String>> = HashMap::new();
    // As soon as src is changed, wipe values of set
    // a = 3; b = a; c = a;
    // a = 4; --> wipe the set!

    // As soon as copy value is changed, update its value
    // a = 3; b = a; c = a; --> src_to_copies = {a: [b, c]}
    // b = 4; --> remove b from src_to_copies, update copy_to_src

    // Single pass (or need 2?)

    // iterate through code looking for copy instructions
    for (_, block) in method_cfg.get_blocks_mut() {
        for insn in block.get_instructions_mut() {
            match insn {
                // Matches a direct copy assignment
                Instruction::Assign { src, dest } => {
                    // println!("instruction is: {:#?}", insn);
                    let src_name = src.get_name().unwrap().to_owned();
                    let dest_name = dest.get_name().unwrap().to_owned();

                    // Recursively find the src and mark that dest is a copy of src
                    let root_src = get_root_source(&src_name, &copy_to_src);

                    // Update hash tables
                    copy_to_src.insert(dest_name.clone(), root_src.clone());
                    src_to_copies
                    .entry(src_name.clone()) 
                    .or_insert_with(HashSet::new) 
                    .insert(dest_name.clone());
                
                    println!("Direct copy: {} <- {}", dest_name, src_name);
                    println!("Can copy-propagate: src {} to copy {}", root_src, dest_name);
                },

                Instruction::Add { left, right, dest }
                | Instruction::Subtract { left, right, dest }
                | Instruction::Multiply { left, right, dest }
                | Instruction::Divide { left, right, dest }
                | Instruction::Modulo { left, right, dest }
                | Instruction::Greater { left, right, dest }
                | Instruction::Less { left, right, dest }
                | Instruction::LessEqual { left, right, dest }
                | Instruction::GreaterEqual { left, right, dest }
                | Instruction::Equal { left, right, dest }
                | Instruction::NotEqual { left, right, dest } => {
                    substitute_operand(left, &copy_to_src);
                    substitute_operand(right, &copy_to_src);
            
                    let dest_name = dest.get_name().unwrap().to_owned();
                    invalidate(&dest_name, &mut copy_to_src, &mut src_to_copies);
                }
                
                Instruction::Not { expr, dest }
                | Instruction::Len { expr, dest }
                | Instruction::Cast { expr, dest, .. } => {
                    substitute_operand(expr, &copy_to_src);
            
                    let dest_name = dest.get_name().unwrap().to_owned();
                    invalidate(&dest_name, &mut copy_to_src, &mut src_to_copies);
                }
            
                Instruction::LoadConst { dest, .. }
                | Instruction::LoadString { dest, .. } => {
                    let dest_name = dest.get_name().unwrap().to_owned();
                    invalidate(&dest_name, &mut copy_to_src, &mut src_to_copies);
                }

                Instruction::MethodCall { args, dest, .. } => {
                    // Always try to propagate copies in the arguments
                    for arg in args {
                        substitute_operand(arg, &copy_to_src);
                    }
                
                    // Now handle the destination if it exists
                    match dest {
                        Some(dest) => {
                            let dest_name = dest.get_name().unwrap().to_owned();
                            invalidate(&dest_name, &mut copy_to_src, &mut src_to_copies);
                        }
                        None => {
                            // Nothing to invalidate, just propagated args
                        }
                    }
                }                
                _ => {}
            }
        }
    }


    // // standard (src is not redefined)
    // a = 3;
    // b = a // {b : a}
    // c = b // {b : a, c : a}
    // d = c // {b : a, c : a, d : a }
    // e = b + c;

    // e = a + a;

    // // advanced (src is redefined)
    // a = 3;
    // b = a // {b : a}
    // c = b // {b : a, c : a}
    // a = 4;
    // d = c // {b : a, c : a, d : a }
    // e = b + c;
    // e = a + a;


    // make a table mapping var_to_temp
    false
}

fn dead_code_elimination(cfg: &mut CFG) -> bool {
    false
}

fn common_subexpression_elimination(cfg: &mut CFG) -> bool {
    false
}


/// Perform multiple passes over the CFG to apply the given optimizations
/// Returns the optimized CFG
pub fn optimize_dataflow(method_cfgs: &mut HashMap<String, CFG>, optimizations: &HashSet<Optimization>, debug: bool
) -> HashMap<String, CFG> {
    if debug {
        println!("============= Optimizing dataflow =============");
        for opt in optimizations {
            println!("{:#?}", opt);
        }
    }

    // Run the optimizations until the CFG stops changing
    let mut fixed_point = false;

    while !fixed_point {
        fixed_point = true;

        // TODO: ordering?
        if optimizations.contains(&Optimization::Cp) {
            for (method, cfg) in method_cfgs.iter_mut() {
                if copy_propagation(cfg) {
                    fixed_point = false;
                    if debug {
                        println!("Constant propagation changed {}", method);
                    }
                }
            }
        }

        if optimizations.contains(&Optimization::Dce) {
            for (method, cfg) in method_cfgs.iter_mut() {
                if dead_code_elimination(cfg) {
                    fixed_point = false;
                    if debug {
                        println!("Dead code elimination changed {}", method);
                    }
                }
            }
        }

        if optimizations.contains(&Optimization::Cse) {
            for (method, cfg) in method_cfgs.iter_mut() {
                if common_subexpression_elimination(cfg) {
                    fixed_point = false;
                    if debug {
                        println!("Common subexpression elimination elimination changed {}", method);
                    }
                }
            }
        }
    }

    method_cfgs.clone()
}
