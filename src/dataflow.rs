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

// #################################################
// HELPERS
// #################################################


/// Invalidate the hash table entries for a variable whose
/// value has been updated, so that we don't attempt to
/// do copy propagation of stale values.
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

    // If dest was a source,b remove all dependent copies
    if let Some(dependents) = src_to_copies.remove(dest_name) {
        for dependent in dependents {
            copy_to_src.remove(&dependent);
        }
    }
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
    match op {
        Operand::LocalVar(name) => {
            // Substitute with the original source
            let root_src = get_root_source(name, copy_to_src);
            // Check whether an udpate occurred
            if *name != root_src {
                if debug {
                    println!("Replacing {} with {}", name, root_src);
                }

                *op = Operand::LocalVar(root_src.clone());
                *update_occurred = true;
            }
        }

        Operand::GlobalVar(name) => {
            let root_src = get_root_source(name, copy_to_src);
            if *name != root_src {
                if debug {
                    println!("Replacing {} with {}", name, root_src);
                }

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
fn get_dest_var(instr: &Instruction) -> Option<String> {
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


/// Returns a vector of source vars involved in an instruction.
fn get_source_vars(instr: &Instruction) -> Vec<String> {
    match instr {
        // t <- X
        Instruction::Assign { src, .. } => match src {
            Operand::LocalVar(v) => vec![v.clone()],
            _ => vec![],
        },

        // t <- X op Y
        Instruction::Add { left, right, .. }
        | Instruction::Subtract { left, right, .. }
        | Instruction::Multiply { left, right, .. }
        | Instruction::Divide { left, right, .. }
        | Instruction::Modulo { left, right, .. }
        | Instruction::Greater { left, right, .. }
        | Instruction::Less { left, right, .. }
        | Instruction::LessEqual { left, right, .. }
        | Instruction::GreaterEqual { left, right, .. }
        | Instruction::Equal { left, right, .. }
        | Instruction::NotEqual { left, right, .. } => {
            let mut vars = Vec::new();
            if let Operand::LocalVar(v) = left {
                vars.push(v.clone());
            }
            if let Operand::LocalVar(v) = right {
                vars.push(v.clone());
            }
            vars
        }

        // t <- !X / cast(X) / len(X)
        Instruction::Not { expr, .. }
        | Instruction::Cast { expr, ..}
        | Instruction::Len { expr, .. } => match expr {
            Operand::LocalVar(v) => vec![v.clone()],
            _ => vec![],
        },

        // Method call arguments
        Instruction::MethodCall { args, .. } => args
            .iter()
            .filter_map(|op| match op {
                Operand::LocalVar(v) => Some(v.clone()),
                _ => None,
            })
            .collect(),

        // Conditional jump depends on condition
        Instruction::CJmp { condition, .. } => match condition {
            Operand::LocalVar(v) => vec![v.clone()],
            _ => vec![],
        },

        // Return value
        Instruction::Ret { value } => match value {
            Some(Operand::LocalVar(v)) => vec![v.clone()],
            _ => vec![],
        },

        // LoadString reads from a source variable
        Instruction::LoadString { src, .. } => match src {
            Operand::LocalVar(v) => vec![v.clone()],
            _ => vec![],
        },

        // LoadConst and Exit do not use variables
        Instruction::LoadConst { .. }
        | Instruction::UJmp { .. }
        | Instruction::Exit { .. } => vec![],
    }
}


// #################################################
// DEAD CODE ELIMINATION
// #################################################

/// Dataflow equations for DCE:
///     - OUT[B] = ∪ IN[S] for successors S (live if may be used in any subsequent blocks)
///     - IN[B] = USE[B] ∪ (OUT[B] - DEF[B])
/// 
/// USE[B] := vars used in B before any local assignment
/// DEF[B] := vars defined in B before any use (any prev. def is not live)
fn compute_liveness(method_cfg: &mut CFG, debug: bool) -> (HashMap<i32, HashSet<String>>, HashMap<i32, HashSet<String>>) {
    // Compute predecessor and successor graphs
    let predecessors = compute_predecessors(&method_cfg);
    let successors = compute_successors(&method_cfg);

    // Variables that are live going into this block; hashmap keyed by block_id
    let mut in_map: HashMap<i32, HashSet<String>> = HashMap::new();
    // Variables that are live going out of this block; hashmap keyed by block_id
    let mut out_map: HashMap<i32, HashSet<String>> = HashMap::new();

    // Worklist of basic block ids (order doesn't matter since iterates until fixed point)
    let mut worklist: VecDeque<i32> = method_cfg.blocks.keys().copied().collect::<VecDeque<i32>>();


    while let Some(block_id) = worklist.pop_front() {
        let block = method_cfg.blocks.get(&block_id).unwrap();

        // OUT[B] = ∪ IN[S] for successors S
        let out_set: HashSet<String> = successors
            .get(&block_id)
            .map(|succs| {
                succs.iter()
                    .filter_map(|s| in_map.get(s))
                    .fold(HashSet::new(), |mut acc, s| {
                        acc.extend(s.clone());
                        acc
                    })
            })
            .unwrap_or_default();

        let mut use_set = HashSet::new();
        let mut def_set = HashSet::new();

        // iterate in reverse within the basic block
        for instr in block.instructions.iter().rev() {
            let used_vars = get_source_vars(instr);
            let dest = get_dest_var(instr);

            for var in used_vars {
                if !def_set.contains(&var) {
                    use_set.insert(var);
                }
            }

            if let Some(var) = dest {
                def_set.insert(var);
            }
        }

        // IN[B] = USE[B] ∪ (OUT[B] - DEF[B])
        let mut in_set = use_set.clone();
        in_set.extend(out_set.difference(&def_set).cloned());

        // Check if IN or OUT changed → if so, propagate
        if in_map.get(&block_id) != Some(&in_set) || out_map.get(&block_id) != Some(&out_set) {
            in_map.insert(block_id, in_set);
            out_map.insert(block_id, out_set);

            if let Some(preds) = predecessors.get(&block_id) {
                for pred in preds {
                    worklist.push_back(*pred); // append predecessors to the queue
                }
            }
        }
    }

    (in_map, out_map)
}


/// Dataflow equations for DCE (backwards analysis):
///     - OUT[B] = ⋃ IN[S] for all successors S of B
///     - IN[B] = USE[B] ∪ (OUT[B] - DEF[B])
fn dead_code_elimination(method_cfg: &mut CFG, debug: bool) -> bool {
    // basic-block level maps; we generate the instruction-level maps during iteration below
    let (_, out_maps) = compute_liveness(method_cfg, debug);
    let mut update_occurred = false;

    for (block_id, block) in method_cfg.blocks.iter_mut() {
        let out_set = out_maps.get(&block_id).unwrap_or(&HashSet::new()).clone();

        // We will add any non-dead code to this vector
        let mut new_instrs = Vec::new();
        let mut live = out_set.clone();

        for instr in block.instructions.iter().rev() {
            let dest = get_dest_var(instr);
            let used = get_source_vars(instr);

            // avoid moving any instructions with a side-effect
            let has_side_effect = matches!(
                instr,
                Instruction::MethodCall { .. }
                    | Instruction::Ret { .. }
                    | Instruction::CJmp { .. }
                    | Instruction::UJmp { .. }
                    | Instruction::Exit { .. }
            );

            let keep = match &dest {
                Some(var) => live.contains(var) || has_side_effect,
                None => true,
            };

            if keep {
                // Update live set
                for var in &used {
                    live.insert(var.clone());
                }
                if let Some(var) = &dest {
                    live.remove(var);
                }
                new_instrs.push(instr.clone());
            } else {
                if debug {
                    println!("Removed dead instruction: {:?}", instr);
                }
                update_occurred = true;
            }
        }

        new_instrs.reverse();
        block.instructions = new_instrs;
    }

    update_occurred
}



// #################################################
// COPY PROPAGATION
// #################################################

// Worklist equations for copy propagation:
//      IN[B]  = ⋂ OUT[P] for all predecessors P of B
//      OUT[B] = GEN[B] ∪ (IN[B] - KILL[B])
// Returns a tuple (in_map, out_map)
fn compute_maps(method_cfg: &mut CFG, debug: bool) -> (HashMap<i32, CopyMap>, HashMap<i32, CopyMap> ) {
    // Compute predecessor and successor graphs
    let cfg_preds = compute_predecessors(&method_cfg);
    let cfg_succs = compute_successors(&method_cfg);

    // Copies that are valid going in to this block; hashmap keyed by block_id
    let mut in_map: HashMap<i32, CopyMap> = HashMap::new();
    // Copies that are valid going out of this block; hashmap keyed by block_id
    let mut out_map: HashMap<i32, CopyMap> = HashMap::new();

    // Worklist of basic block ids
    let mut worklist: VecDeque<i32> = method_cfg.blocks.keys().copied().collect::<VecDeque<i32>>();


    // Iterate until a fixed point
    while let Some(block_id) = worklist.pop_front() {

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
                    // Kill copies that use dest, since this assignment updates its values
                    if let Operand::LocalVar(dest_name) = dest {
                        invalidate(dest_name, &mut copy_to_src, &mut src_to_copies);

                        // Gen[B]: this is a direct assignment (a = b), add to tables
                        if let Operand::LocalVar(src_name) = src {
                            copy_to_src.insert(dest_name.clone(), src_name.clone());
                            src_to_copies.entry(src_name.clone()).or_default().insert(dest_name.clone());
                        }
                    }
                }

                // Binary operations: arithmetic and relational 
                Instruction::Add {dest, .. } 
                | Instruction::Subtract {dest, .. }
                | Instruction::Multiply {dest, .. }
                | Instruction::Divide {dest, .. }
                | Instruction::Modulo {dest, .. } 
                | Instruction::Greater {dest, .. }
                | Instruction::Less {dest, .. }
                | Instruction::LessEqual {dest, .. }
                | Instruction::GreaterEqual {dest, .. }
                | Instruction::Equal {dest, .. }
                | Instruction::NotEqual {dest, .. }=> {
                    // Invalidate the destination since its value is updated
                    invalidate(&dest.to_string(), &mut copy_to_src, &mut src_to_copies);
                }

                // Unary operations
                Instruction::Not {dest, .. } => {
                    invalidate(&dest.to_string(), &mut copy_to_src, &mut src_to_copies);
                }
                Instruction::Cast {dest, ..} => {
                    invalidate(&dest.to_string(), &mut copy_to_src, &mut src_to_copies);
                }
                Instruction::Len {dest, .. } => {
                    invalidate(&dest.to_string(), &mut copy_to_src, &mut src_to_copies);
                }

                Instruction::MethodCall {args, dest , ..} => {
                    // Invalidate the destination since its value is updated
                    if dest.is_some() {
                        invalidate(&dest.clone().unwrap().to_string(), &mut copy_to_src, &mut src_to_copies);
                    }
                }

                Instruction::LoadString { dest, .. } => {
                    // Invalidate the destination since its value is updated
                    invalidate(&dest.to_string(), &mut copy_to_src, &mut src_to_copies);
                }
                Instruction::LoadConst {dest, .. } => {
                    // Invalidate the destination since its value is updated
                    invalidate(&dest.to_string(), &mut copy_to_src, &mut src_to_copies);
                }
                _ => { 
                    // UJmp, CJmp, Ret, and Exit have no effect
                    // Conservatively attempt to invalidate any destinations that are written, but shouldn't
                    // be any for any of these
                    let dest = get_dest_var(instr);
                    if let Some(dest) = dest {
                        invalidate(&dest, &mut copy_to_src, &mut src_to_copies);
                    }
                }
            }
        }
    
        // OUT[B] = GEN[B] ∪ (IN[B] - KILL[B])
        let out_copies: HashMap<String, String> = copy_to_src.clone();
    
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

    (in_map, out_map)
}


fn copy_propagation(method_cfg: &mut CFG, debug: bool) -> bool {
    let (in_maps, _) = compute_maps(method_cfg, debug);
    let mut update_occurred = false;

    for (block_id, _) in method_cfg.blocks.clone() {
        let copy_to_src = in_maps
            .get(&block_id)
            .cloned()
            .expect("couldn't find block id");
        let mut copy_to_src = copy_to_src.clone();
        let mut src_to_copies = reverse_map(&copy_to_src);

        for instr in method_cfg
            .blocks
            .get_mut(&block_id)
            .unwrap()
            .instructions
            .iter_mut()
        {
            match instr {
                Instruction::Assign { src, dest } => {
                    substitute_operand(src, &copy_to_src, &mut update_occurred, debug);

                    if let Operand::LocalVar(dest_name) = dest {
                        invalidate(dest_name, &mut copy_to_src, &mut src_to_copies);

                        if let Operand::LocalVar(src_name) = src {
                            copy_to_src.insert(dest_name.clone(), src_name.clone());
                            src_to_copies
                                .entry(src_name.clone())
                                .or_default()
                                .insert(dest_name.clone());
                        }
                    }
                }
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
                    substitute_operand(left, &copy_to_src, &mut update_occurred, debug);
                    substitute_operand(right, &copy_to_src, &mut update_occurred, debug);

                    invalidate(&dest.to_string(), &mut copy_to_src, &mut src_to_copies);
                }
                Instruction::Not { expr, dest }
                | Instruction::Cast { expr, dest, .. }
                | Instruction::Len { expr, dest } => {
                    substitute_operand(expr, &copy_to_src, &mut update_occurred, debug);
        
                    invalidate(&dest.to_string(), &mut copy_to_src, &mut src_to_copies);
                }
                Instruction::MethodCall { args, dest, .. } => {
                    for arg in args {
                        // You may uncomment this if you want to try propagating into args
                        substitute_operand(arg, &copy_to_src, &mut update_occurred, debug);
                    }

                    if let Some(dest_op) = dest {
                        invalidate(&dest_op.to_string(), &mut copy_to_src, &mut src_to_copies);
                    }
                }
                Instruction::LoadString { dest, .. }
                | Instruction::LoadConst { dest, .. } => {
                    invalidate(&dest.to_string(), &mut copy_to_src, &mut src_to_copies);
                }
                Instruction::UJmp { .. }
                | Instruction::CJmp { .. }
                | Instruction::Ret { .. }
                | Instruction::Exit { .. } => {
                    if let Some(dest_name) = get_dest_var(instr) {
                        invalidate(&dest_name, &mut copy_to_src, &mut src_to_copies);
                    }
                }
            }
        }
    }

    update_occurred
}



// #################################################
// COMMON SUBEXPRESSION ELIMINATION
// #################################################

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
                if dead_code_elimination(cfg, debug) {
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
