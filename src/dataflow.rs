/**
Dataflow code generation optimizations.
*/
use std::collections::{HashMap, HashSet, VecDeque};
use crate::{
    cfg::CFG,
    utils::cli::Optimization,
    tac::*,
    state::*,
};


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


        // As soon as src is changed, wipe values of set
    // a = 3; b = a; c = a;
    // a = 4; --> wipe the set!

    // As soon as copy value is changed, update its value
    // a = 3; b = a; c = a; --> src_to_copies = {a: [b, c]}
    // b = 4; --> remove b from src_to_copies, update copy_to_src

    // Single pass (or need 2?)



/// Invalidate the hash table entries for a variable whose
/// value has been updated, so that we don't attempt to
/// do copy propagation of stale values.
fn invalidate(
    dest_name: &str,
    copy_to_src: &mut HashMap<String, String>,
    src_to_copies: &mut HashMap<String, HashSet<String>>,
    debug: bool
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

    // if debug {
    //     println!("Invalidated mutated variable: {}", dest_name);
    // }

}

/// Recursively get the root source that a copy refers to.
/// Returns the input variable if it is not a copy of anything.
fn get_root_source(
    var_name: &str,
    copy_to_src: &HashMap<String, String>,
) -> String {
    let mut current: &str = var_name;
    while let Some(next) = copy_to_src.get(current) {
        current = next;
    }
    current.to_string()
}


/// If the given operand is a direct copy of another operand,
/// replace it with the source operand. Otherwise, this has no effect.
/// Returns true iff a mutation occurred.
fn substitute_operand(op: &mut Operand, copy_to_src: &HashMap<String, String>, update_occurred: &mut bool, debug: bool) {
    // TODO: make sure this is properly mutating the CFG
    match op {
        Operand::LocalVar(name) => {
            let root_src = get_root_source(name, copy_to_src);
            // Check whether an udpate occurred
            if *name != root_src {
                // if debug {
                    println!("Replacing {} with {}", name, root_src);
                // }

                *op = Operand::LocalVar(root_src.clone());
                *update_occurred = true;
            }
        }

        Operand::GlobalVar(name) => {
            let root_src = get_root_source(name, copy_to_src);
            if *name != root_src {
                // if debug {
                    println!("Replacing {} with {}", name, root_src);
                // }

                *op = Operand::GlobalVar(root_src);
                *update_occurred = true;
            }
        }

        Operand::LocalArrElement(_, index) | Operand::GlobalArrElement(_, index) => {
            substitute_operand(index, copy_to_src, update_occurred, debug);
        }

        _ => {
            // Const, String, Argument – do nothing
        }
    }
}


/// Map intersection for IN
fn intersect_maps(
    mapa: &HashMap<String, String>,
    mapb: &HashMap<String, String>
) -> HashMap<String, String> {
    let mut result = HashMap::new();
    for (k, val1) in mapa {
        if let Some(val2) = mapb.get(k) {
            if val1 == val2 {
                result.insert(k.clone(), val1.clone());
            }
        }
    }
    result
}
    
/// Reverse (key, map) pairs for easier invalidation
fn reverse_map(map: &HashMap<String, String>) -> HashMap<String, HashSet<String>> {
    let mut rev: HashMap<String, HashSet<String>> = HashMap::new();
    for (dst, src) in map {
        rev.entry(src.clone()).or_default().insert(dst.clone());
    }
    rev
}

/// Get the destination that an instruction writes to
fn get_dest(instr: &Instruction) -> Option<String> {
    match instr {
        Instruction::Assign { dest, .. }
            | Instruction::Add { dest, .. }
            | Instruction::Subtract { dest, .. }
            | Instruction::Multiply { dest, .. }
            | Instruction::Divide { dest, .. }
            | Instruction::Modulo { dest, .. }
            | Instruction::Cast { dest, .. }
            | Instruction::Not { dest, .. }
            | Instruction::Len { dest, .. }
            | Instruction::Equal { dest, .. }
            | Instruction::Less { dest, .. }
            | Instruction::Greater { dest, .. }
            | Instruction::LessEqual { dest, .. }
            | Instruction::GreaterEqual { dest, .. }
            | Instruction::NotEqual {dest, .. }
            | Instruction::LoadString {dest, .. }
            | Instruction::LoadConst {dest, .. } => {
                Some(dest.to_string())
            }
            Instruction::MethodCall { dest, .. } =>{
                if let Some(dest) = dest {
                    Some(dest.to_string())
                } else {
                    None
                }
            }
            _=> None
    }
}



// Worklist equations for copy propagation:
//      IN[B]  = ⋂ OUT[P] for all predecessors P of B
//      OUT[B] = GEN[B] ∪ (IN[B] - KILL[B])
fn copy_propagation(method_cfg: &mut CFG, debug: bool) -> bool {
    // Compute predecessor and successor graphs
    let cfg_preds = compute_predecessors(&method_cfg);
    let cfg_succs = compute_successors(&method_cfg);

    // if debug {
    //     println!("preds are: {:#?}", cfg_preds);
    //     println!("succs are {:#?}", cfg_succs);
    // }

    // Copies that are valid going in to this block; hashmap keyed by block_id
    let mut in_map: HashMap<i32, CopyMap> = HashMap::new();
    // Copies that are valid going out of this block; hashmap keyed by block_id
    let mut out_map: HashMap<i32, CopyMap> = HashMap::new();

    // Worklist of basic block ids
    let mut worklist: VecDeque<i32> = method_cfg.blocks.keys().copied().collect::<VecDeque<i32>>();
    let mut update_occurred = false;


    // Iterate until a fixed point
    while let Some(block_id) = worklist.pop_front() {
        // let block = method_cfg.blocks.get(&block_id).unwrap();
    
        // IN[B] = ⋂ OUT[P] for all predecessors P of B, where IN is the hash table containing valid copy mappigns
        let in_copies = if let Some(preds) = cfg_preds.get(&block_id) {
            preds.iter()
                .filter_map(|p| out_map.get(p).cloned())
                .reduce(|a, b| intersect_maps(&a, &b))
                .unwrap_or_default()
        } else {
            // If there are no predecessors, IN is empty
            HashMap::new()
        };
    
        // Copy state we'll update during this block
        let mut copy_to_src = in_copies.clone();
        let mut src_to_copies: HashMap<String, HashSet<String>> = reverse_map(&copy_to_src);
    
        for instr in method_cfg.blocks.get_mut(&block_id).unwrap().instructions.iter_mut() {
            match instr {
                Instruction::Assign { src, dest } => {
                    // Apply the copy prop if operands are copies of another block
                    substitute_operand(src, &copy_to_src, &mut update_occurred, debug);
                    substitute_operand(dest, &copy_to_src, &mut update_occurred, debug);
    
                    // Kill copies that use dest, since this assignment updates its values
                    if let Operand::LocalVar(dest_name) = dest {
                        invalidate(dest_name, &mut copy_to_src, &mut src_to_copies, debug);

                        // Gen[B] : if this is a direct assignment, add to tables
                        if let Operand::LocalVar(src_name) = src {
                            copy_to_src.insert(dest_name.clone(), src_name.clone());
                            src_to_copies.entry(src_name.clone()).or_default().insert(dest_name.clone());
                        }
                    }
                }
    
                // TODO: handle other mutating instructions
                _ => {
                    // Conservatively invalidate any destinations that are written to (e.g., in arithmetic)
                    let dest = get_dest(instr);
                    if let Some(dest) = dest {
                        invalidate(&dest, &mut copy_to_src, &mut src_to_copies, debug);
                    }
                }
            }
        }
    
        // OUT[B] = GEN[B] ∪ (IN[B] - KILL[B])
        let out_copies = copy_to_src.clone();
    
        // Update maps if either IN or OUT changed
        if in_map.get(&block_id) != Some(&in_copies) || out_map.get(&block_id) != Some(&out_copies) {
            in_map.insert(block_id, in_copies);
            out_map.insert(block_id, out_copies);
    
            // Queue all successors of block, since IN or OUT changed
            if let Some(succs) = cfg_succs.get(&block_id) {
                for succ in succs {
                    worklist.push_back(*succ);
                }
            }
        }
    }

    update_occurred
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
                if copy_propagation(cfg, debug) {
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
