/**
Dataflow code generation optimizations.
*/
use std::collections::{BTreeMap, BTreeSet, VecDeque};
use crate::{
    cfg::{Global, CFG}, regalloc::reg_alloc, state::*, tac::*, utils::cli::Optimization, x86::{Register, X86Operand}
};

// #################################################
// HELPERS
// #################################################

/// Map intersection for IN
fn intersect_maps(
    mapa: &BTreeMap<String, String>,
    mapb: &BTreeMap<String, String>
) -> BTreeMap<String, String> {
    let mut result = BTreeMap::new();
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
fn reverse_map(map: &BTreeMap<String, String>) -> BTreeMap<String, BTreeSet<String>> {
    let mut rev: BTreeMap<String, BTreeSet<String>> = BTreeMap::new();
    for (dst, src) in map {
        rev.entry(src.clone()).or_default().insert(dst.clone());
    }
    rev
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
fn compute_liveness(method_cfg: &mut CFG, debug: bool) -> (BTreeMap<i32, BTreeSet<String>>, BTreeMap<i32, BTreeSet<String>>) {
    // Compute predecessor and successor graphs
    let predecessors = compute_predecessors(&method_cfg);
    let successors = compute_successors(&method_cfg);

    // Variables that are live going into this block; BTreeMap keyed by block_id
    let mut in_map: BTreeMap<i32, BTreeSet<String>> = BTreeMap::new();
    // Variables that are live going out of this block; BTreeMap keyed by block_id
    let mut out_map: BTreeMap<i32, BTreeSet<String>> = BTreeMap::new();

    // Worklist of basic block ids (order doesn't matter since iterates until fixed point)
    let mut worklist: VecDeque<i32> = method_cfg.blocks.keys().copied().collect::<VecDeque<i32>>();


    // Run fixed point algorithm to get steady-state liveness maps
    while let Some(block_id) = worklist.pop_front() {
        // OUT[B] = ∪ IN[S] for successors S
        let out_set: BTreeSet<String> = successors
            .get(&block_id)
            .map(|succs| {
                succs.iter()
                    .filter_map(|s| in_map.get(s))
                    .fold(BTreeSet::new(), |mut acc, s| {
                        acc.extend(s.clone());
                        acc
                    })
            })
            .unwrap_or_default();

        let mut use_set = BTreeSet::new();
        let mut def_set = BTreeSet::new();

        // iterate in reverse within the basic block
        for instr in method_cfg.blocks.get(&block_id).unwrap().instructions.iter().rev() {
            let used_vars = instr.get_used_vars();
            let dest = instr.get_def_var();

            // populate use and def sets
            for var in used_vars {
                use_set.insert(var);
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

// webs
// different names for each web a1, a2
// block-level liveness above
// like DCE instruction level but add to sets instead of deleting instrutions


/// Dataflow equations for DCE (backwards analysis):
///     - OUT[B] = ⋃ IN[S] for all successors S of B
///     - IN[B] = USE[B] ∪ (OUT[B] - DEF[B])
fn dead_code_elimination(method_cfg: &mut CFG, debug: bool) -> bool {
    // basic-block level maps; we generate the instruction-level maps during iteration below
    let (_, out_maps) = compute_liveness(method_cfg, debug);
    let mut update_occurred = false;


    for (block_id, block) in method_cfg.blocks.iter_mut() {
        let out_set = out_maps.get(&block_id).unwrap_or(&BTreeSet::new()).clone();

        // We will add any non-dead code to this vector
        let mut new_instrs = Vec::new();
        let mut live = out_set.clone();

        
        // Work through basic block instructions from bottom up
        for instr in block.instructions.iter().rev() {
            let dest = instr.get_def_var();
            let used = instr.get_used_vars();
            
            // Avoid moving any instructions with a side-effect
            let has_side_effect = match instr {
                Instruction::MethodCall { .. }
                | Instruction::Ret { .. }
                | Instruction::CJmp { .. }
                | Instruction::UJmp { .. }
                | Instruction::Exit { .. } => true,

                // Any write to global var is a side effect
                 _=> {
                    if let Some(dest) = instr.get_def_var() {
                        method_cfg.locals.get(dest.as_str()).is_none()
                    } else {
                        false
                    }
                }
            };

            let keep_instr = match &dest {
                // TODO: add another condition to require liveness, and has a use
                // in a successor
                Some(var) => has_side_effect || (live.contains(var)),
                None => true,
            };


            if keep_instr {
                // Insert used variables into live set
                for var in &used {
                    live.insert(var.clone());
                }

                // Remove defined variables from the live set
                if let Some(var) = &dest {
                    // TODO: Don't (?) remove from liveness if is a def and a use
                    // This condition should NOT be necessary
                    if !&used.contains(var) {
                        live.remove(var);
                    }
                }
                new_instrs.push(instr.clone());
            } else {
                // DO NOT REMOVE: needed for testopt debugging
                if debug {
                    println!("DCE: Removed dead instruction: {:?}", instr);
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

/// Invalidate the hash table entries for a variable whose
/// value has been updated, so that we don't attempt to
/// do copy propagation of stale values.
fn invalidate(
    dest_name: &str,
    copy_to_src: &mut BTreeMap<String, String>,
    src_to_copies: &mut BTreeMap<String, BTreeSet<String>>,
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
    copy_to_src: &BTreeMap<String, String>,
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
fn substitute_operand(op: &mut Operand, copy_to_src: &BTreeMap<String, String>, update_occurred: &mut bool, debug: bool) {
    match op {
        Operand::LocalVar { name, typ, .. }=> {
            // Substitute with the original source
            let root_src = get_root_source(name, copy_to_src);
            // Check whether an udpate occurred
            if *name != root_src {
                // DO NOT REMOVE: needed for testopt debugging
                if debug {
                    println!("CP: Replacing {} with {}", name, root_src);
                }

                *op = Operand::LocalVar { name: root_src, typ: typ.clone(), reg: None };
                *update_occurred = true;
            }
        }

        Operand::GlobalVar { name, typ, .. } => {
            let root_src = get_root_source(name, copy_to_src);
            if *name != root_src {
                // DO NOT REMOVE: needed for testopt debugging
                if debug {
                    println!("CP: Replacing {} with {}", name, root_src);
                }

                *op = Operand::GlobalVar { name: root_src, typ: typ.clone(), reg: None };
                *update_occurred = true;
            }
        }

        Operand::LocalArrElement { index, .. } => {
            substitute_operand(index, copy_to_src, update_occurred, debug);
        }

        _ => {
            // Const, String, Argument – do nothing
        }
    }
}


// Worklist equations for copy propagation:
//      IN[B]  = ⋂ OUT[P] for all predecessors P of B
//      OUT[B] = GEN[B] ∪ (IN[B] - KILL[B])
// Returns a tuple (in_map, out_map)
fn compute_maps(method_cfg: &mut CFG, debug: bool) -> (BTreeMap<i32, CopyMap>, BTreeMap<i32, CopyMap> ) {
    // Compute predecessor and successor graphs
    let cfg_preds = compute_predecessors(&method_cfg);
    let cfg_succs = compute_successors(&method_cfg);

    // Copies that are valid going in to this block; BTreeMap keyed by block_id
    let mut in_map: BTreeMap<i32, CopyMap> = BTreeMap::new();
    // Copies that are valid going out of this block; BTreeMap keyed by block_id
    let mut out_map: BTreeMap<i32, CopyMap> = BTreeMap::new();

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
            BTreeMap::new()
        };
    
        // Copy state we'll update during this block
        let mut copy_to_src = in_copies.clone();
        let mut src_to_copies: BTreeMap<String, BTreeSet<String>> = reverse_map(&copy_to_src);
    
        for instr in method_cfg.blocks.get_mut(&block_id).unwrap().instructions.iter_mut() {
            match instr {
                Instruction::Assign { typ: _, src, dest } => {
                    // Kill copies that use dest, since this assignment updates its values
                    if let Operand::LocalVar { name: dest_name, .. } = dest.clone() {
                        invalidate(&dest.to_string(), &mut copy_to_src, &mut src_to_copies);

                        // Gen[B]: this is a direct assignment (a = b), add to tables
                        if let Operand::LocalVar { name: src_name, .. } = src {
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

                Instruction::MethodCall {args: _, dest , ..} => {
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
                    let dest = instr.get_def_var();
                    if let Some(dest) = dest {
                        invalidate(&dest, &mut copy_to_src, &mut src_to_copies);
                    }
                }
            }
        }
    
        // OUT[B] = GEN[B] ∪ (IN[B] - KILL[B])
        let out_copies: BTreeMap<String, String> = copy_to_src.clone();
    
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
                Instruction::Assign { typ: _, src, dest } => {
                    substitute_operand(src, &copy_to_src, &mut update_occurred, debug);

                    if let Operand::LocalVar { name: dest_name, .. } = dest.clone() {
                        invalidate(&dest.to_string(), &mut copy_to_src, &mut src_to_copies);

                        if let Operand::LocalVar { name: src_name, .. } = src {
                            copy_to_src.insert(dest_name.clone(), src_name.clone());
                            src_to_copies
                                .entry(src_name.clone())
                                .or_default()
                                .insert(dest_name.clone());
                        }
                    }
                }
                Instruction::Add { typ: _, left, right, dest }
                | Instruction::Subtract { typ: _, left, right, dest }
                | Instruction::Multiply { typ: _, left, right, dest }
                | Instruction::Divide { typ: _, left, right, dest }
                | Instruction::Modulo { typ: _, left, right, dest }
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
                | Instruction::Len { typ: _, expr, dest } => {
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
                    if let Some(dest_name) = instr.get_def_var() {
                        invalidate(&dest_name, &mut copy_to_src, &mut src_to_copies);
                    }
                }
            }
        }
    }

    update_occurred
}


// #################################################
// CONSTANT PROPAGATION
// #################################################

type ConstMap = BTreeMap<String, Operand>; // maps variable name → constant operand


fn intersect_constants(
    a: &ConstMap,
    b: &ConstMap
) -> ConstMap {
    let mut result = ConstMap::new();
    for (k, v1) in a {
        if let Some(v2) = b.get(k) {
            if v1 == v2 {
                result.insert(k.clone(), v1.clone());
            }
        }
    }
    result
}


fn compute_const_maps(method_cfg: &CFG, debug: bool) -> (BTreeMap<i32, ConstMap>, BTreeMap<i32, ConstMap>) {
    let preds = compute_predecessors(method_cfg);
    let succs = compute_successors(method_cfg);

    let mut in_map = BTreeMap::new();
    let mut out_map = BTreeMap::new();
    let mut worklist: VecDeque<i32> = method_cfg.blocks.keys().copied().collect();

    while let Some(block_id) = worklist.pop_front() {
        let in_consts = if let Some(preds) = preds.get(&block_id) {
            preds.iter()
                .filter_map(|p| out_map.get(p).cloned())
                .reduce(|a, b| intersect_constants(&a, &b))
                .unwrap_or_default()
        } else {
            BTreeMap::new()
        };

        let mut consts = in_consts.clone();

        for instr in &method_cfg.blocks[&block_id].instructions {
            match instr {
                Instruction::Assign { src, dest, .. } => {
                    if let Operand::Const { .. } = src {
                        consts.insert(dest.to_string(), src.clone());
                    } else {
                        consts.remove(&dest.to_string());
                    }
                }
                Instruction::LoadConst { dest, src , ..} => {
                    consts.insert(dest.to_string(), Operand::Const { value: *src, typ: dest.get_type(), reg: None });
                }
                Instruction::Not { dest, .. }
                | Instruction::Cast { dest, .. }
                | Instruction::Add { dest, .. }
                | Instruction::Multiply { dest, .. }
                | Instruction::Divide { dest, .. }
                | Instruction::Modulo { dest, .. }
                | Instruction::Subtract { dest, .. }
                | Instruction::Equal { dest, .. }
                | Instruction::NotEqual { dest, .. }
                | Instruction::Less { dest, .. }
                | Instruction::LessEqual { dest, .. }
                | Instruction::Greater { dest, .. }
                | Instruction::GreaterEqual { dest, .. }
                | Instruction::Len { dest, .. }
                | Instruction::MethodCall { dest: Some(dest), .. }
                | Instruction::LoadString { dest, .. } => {
                    consts.remove(&dest.to_string());
                }
                _ => {}
            }
        }

        if in_map.get(&block_id) != Some(&in_consts) || out_map.get(&block_id) != Some(&consts) {
            in_map.insert(block_id, in_consts);
            out_map.insert(block_id, consts);

            if let Some(succs) = succs.get(&block_id) {
                for s in succs {
                    worklist.push_back(*s);
                }
            }
        }
    }
    (in_map, out_map)
}


fn constant_propagation(method_cfg: &mut CFG, debug: bool) -> bool {
    let (in_maps, _) = compute_const_maps(method_cfg, debug);
    let mut changed = false;

    for (block_id, block) in method_cfg.blocks.iter_mut() {
        let mut consts = in_maps.get(&block_id).cloned().unwrap_or_default();
        let mut block_isn_ct = 0;
        for instr in block.get_mut_instructions() {
            println!("Looking at instruction: {:?}", instr);
            block_isn_ct += 1;
            let mut update = |op: &mut Operand| {
                if let Operand::LocalVar { name, typ, reg } = op {
                    if let Some(Operand::Const { value, .. }) = consts.get(name) {
                        if debug {
                            println!("CPROP: Replacing {} with const {}", name, value);
                        }
                        *op = Operand::Const {
                            value: *value,
                            typ: typ.clone(),
                            reg: reg.clone(),
                        };
                        changed = true;
                    }
                }
            };
            println!("Consts before instruction: {:?}", consts);

            match instr {
                Instruction::Assign { src, .. } => {
                    update(src);
                }
                Instruction::Add { left, right, .. }
                | Instruction::Subtract { left, right, .. }
                | Instruction::Multiply { left, right, .. }
                | Instruction::Divide { left, right, .. }
                | Instruction::Modulo { left, right, .. }
                | Instruction::Equal { left, right, .. }
                | Instruction::NotEqual { left, right, .. }
                | Instruction::Less { left, right, .. }
                | Instruction::LessEqual { left, right, .. }
                | Instruction::Greater { left, right, .. }
                | Instruction::GreaterEqual { left, right, .. } => {
                    update(left);
                    update(right);
                }
                Instruction::Not { expr, .. }
                | Instruction::Cast { expr, .. }
                | Instruction::Len { expr, .. } => {
                    update(expr);
                }
                Instruction::MethodCall { args, .. } => {
                    for arg in args.iter_mut() {
                        update(arg);
                    }
                }
                Instruction::LoadConst { src, dest, typ } => {
                    consts.insert(dest.to_string(), Operand::Const { value: *src, typ: typ.clone(), reg: None });
                },
                _ => {}
            }
        }
        println!("BLOCK INSTR COUNT: {:?}", block_isn_ct);
    }

    changed
}




// #################################################
// COMMON SUBEXPRESSION ELIMINATION
// #################################################

/// CSE: Map intersection for IN
fn intersect_expressions(
    mapa: &AvailableExpressions,
    mapb: &AvailableExpressions
) -> AvailableExpressions {
    let mut result = BTreeMap::new();
    for (k, val1) in mapa {
        if let Some(val2) = mapb.get(k) {
            if val1 == val2 {
                result.insert(k.clone(), val1.clone());
            }
        }
    }
    result
}

// Worklist equations for CSE:
//      IN[B]  = ⋂ OUT[P] for all predecessors P of B
//      OUT[B] = GEN[B] ∪ (IN[B] - KILL[B])
// Returns a tuple (in_map, out_map)
fn compute_expression_maps(method_cfg: &mut CFG, debug: bool) -> (BTreeMap<i32, AvailableExpressions>, BTreeMap<i32, AvailableExpressions> ) {
    // Compute predecessor and successor graphs
    let cfg_preds = compute_predecessors(&method_cfg);
    let cfg_succs = compute_successors(&method_cfg);

    // Expressions that are valid going in to this block; BTreeMap keyed by block_id
    let mut in_map: BTreeMap<i32, AvailableExpressions> = BTreeMap::new();
    // Expressions that are valid going out of this block; BTreeMap keyed by block_id
    let mut out_map: BTreeMap<i32, AvailableExpressions> = BTreeMap::new();

    // Worklist of basic block ids
    let mut worklist: VecDeque<i32> = method_cfg.blocks.keys().copied().collect::<VecDeque<i32>>();

    // Iterate until a fixed point
    while let Some(block_id) = worklist.pop_front() {

        // IN[B] = ⋂ OUT[P] for all predecessors P of B, where IN is the hash table containing available expressions
        let in_expressions = if let Some(preds) = cfg_preds.get(&block_id) {
            preds.iter()
                .filter_map(|p| out_map.get(p).cloned())
                .reduce(|a, b| intersect_expressions(&a, &b))
                .unwrap_or_default()
        } else {
            // If there are no predecessors, IN is empty
            BTreeMap::new()
        };
    
        let mut out_expressions = in_expressions.clone();  // maps expression to variable

        for instr in method_cfg.blocks.get_mut(&block_id).unwrap().instructions.iter_mut() {
            match instr {
                Instruction::Add { left, right, ref dest, typ: _ }
                | Instruction::Subtract { left, right, ref dest, typ: _ }
                | Instruction::Multiply { left, right, ref dest, typ: _ }
                | Instruction::Divide { left, right, ref dest, typ: _ }
                | Instruction::Modulo { left, right, ref dest, typ: _ } => {
                    let left_var = match left {
                        Operand::LocalVar { name, .. } => Some(name.clone()),
                        _ => None,  // only do CSE on local variables
                    };
            
                    let right_var = match right {
                        Operand::LocalVar { name, .. } => Some(name.clone()),
                        _ => None,  // only do CSE on local variables
                    };
        
                    let dest_var = match dest {
                        Operand::LocalVar { name, .. } => Some(name.clone()),
                        _ => None  // only do CSE on local variables
                    };

                    if let (Some(lv), Some(rv)) = (left_var, right_var) {
                        let expr = match instr {
                            Instruction::Add { .. } => Expression::Add(lv.clone(), rv.clone()),
                            Instruction::Subtract { .. } => Expression::Subtract(lv.clone(), rv.clone()),
                            Instruction::Multiply { .. } => Expression::Multiply(lv.clone(), rv.clone()),
                            Instruction::Divide { .. } => Expression::Divide(lv.clone(), rv.clone()),
                            Instruction::Modulo { .. } => Expression::Modulo(lv.clone(), rv.clone()),
                            _ => unreachable!(),
                        };

                        // kill expressions that involve dest
                        if dest_var.is_some() {
                            kill_expressions(&mut out_expressions, dest_var.as_ref().unwrap(), debug);
                        }

                        // generate the expression if dest is local and different from the operands
                        if dest_var.is_some() && dest_var.as_ref().unwrap() != &lv && dest_var.as_ref().unwrap() != &rv {
                            out_expressions.insert(expr, dest_var.as_ref().unwrap().to_string());
                        }
                    }
                }
                Instruction::Assign { dest, .. }
                | Instruction::Cast { dest, .. }
                | Instruction::Equal { dest, .. }
                | Instruction::Greater { dest, .. }
                | Instruction::GreaterEqual { dest, .. }
                | Instruction::Len { dest, .. }
                | Instruction::Less { dest, .. }
                | Instruction::LessEqual { dest, .. }
                | Instruction::LoadConst { dest, .. }
                | Instruction::LoadString { dest, .. }
                | Instruction::MethodCall { dest: Some(dest), .. }
                | Instruction::Not { dest, .. }
                | Instruction::NotEqual { dest, .. } => {
                    // kill expressions that involve dest
                    match dest {
                        Operand::LocalVar { name, .. } => kill_expressions(&mut out_expressions, name, debug),
                        _ => continue  // only do CSE on local variables
                    };
                }
                _ => continue  // skip other types of instructions
            }
        }
    
        // Update maps if either IN or OUT changed
        if in_map.get(&block_id) != Some(&in_expressions) || out_map.get(&block_id) != Some(&out_expressions) {
            in_map.insert(block_id, in_expressions);
            out_map.insert(block_id, out_expressions);
    
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

// kill all expressions involving var
fn kill_expressions(expressions: &mut AvailableExpressions, var: &String, debug: bool) {
    expressions.retain(|expr, _| {
        match expr {
            Expression::Add(left, right)
            | Expression::Subtract(left, right)
            | Expression::Multiply(left, right)
            | Expression::Divide(left, right)
            | Expression::Modulo(left, right) => {
                if var == left || var == right {
                    false
                } else {
                    true // only keep expr if its left and right are different from var
                }
            }
        }
    });
}

// Worklist equations for CSE:
//      IN[B]  = ⋂ OUT[P] for all predecessors P of B
//      OUT[B] = GEN[B] ∪ (IN[B] - KILL[B])
fn common_subexpression_elimination(method_cfg: &mut CFG, debug: bool) -> bool {
    let (in_maps, _) = compute_expression_maps(method_cfg, debug);
    let mut update_occurred = false;

    for (block_id, _) in method_cfg.blocks.clone() {
        let mut expressions = in_maps
            .get(&block_id)
            .cloned()
            .expect("couldn't find block id");

        for instr in method_cfg
            .blocks
            .get_mut(&block_id)
            .unwrap()
            .instructions
            .iter_mut()
        {
            match instr {
                Instruction::Add { left, right, ref dest, ref typ }
                | Instruction::Subtract { left, right, ref dest, ref typ }
                | Instruction::Multiply { left, right, ref dest, ref typ }
                | Instruction::Divide { left, right, ref dest, ref typ }
                | Instruction::Modulo { left, right, ref dest, ref typ } => {
                    let left_var = match left {
                        Operand::LocalVar { name, .. } => Some(name.clone()),
                        _ => None,  // only do CSE on local variables
                    };
            
                    let right_var = match right {
                        Operand::LocalVar { name, .. } => Some(name.clone()),
                        _ => None,  // only do CSE on local variables
                    };
        
                    let dest_var = match dest {
                        Operand::LocalVar { name, .. } => Some(name.clone()),
                        _ => None  // only do CSE on local variables
                    };

                    let mut insert = None;

                    if let (Some(lv), Some(rv)) = (left_var, right_var) {
                        let expr = match instr {
                            Instruction::Add { .. } => Expression::Add(lv.clone(), rv.clone()),
                            Instruction::Subtract { .. } => Expression::Subtract(lv.clone(), rv.clone()),
                            Instruction::Multiply { .. } => Expression::Multiply(lv.clone(), rv.clone()),
                            Instruction::Divide { .. } => Expression::Divide(lv.clone(), rv.clone()),
                            Instruction::Modulo { .. } => Expression::Modulo(lv.clone(), rv.clone()),
                            _ => unreachable!(),
                        };

                        // check if this expression is already available
                        if let Some(expression_var) = expressions.get(&expr) {
                            // replace this instruction with an assignment
                            *instr = Instruction::Assign {
                                dest: dest.clone(),
                                src: Operand::LocalVar { name: expression_var.clone(), typ: typ.clone(), reg: None },
                                typ: typ.clone()
                            };

                            update_occurred = true;
                            
                            // DO NOT REMOVE: needed for testopt debugging
                            if debug {
                                println!("CSE: Replacing expression {} with variable {}", expr, expression_var.clone());
                            }
                        } else {
                            // generate the expression if dest is local and different from the operands
                            if dest_var.is_some() && dest_var.as_ref().unwrap() != &lv && dest_var.as_ref().unwrap() != &rv {
                                insert = Some(expr);
                            }
                        }
                    }

                    // kill expressions that involve dest
                    if dest_var.is_some() {
                        kill_expressions(&mut expressions, dest_var.as_ref().unwrap(), debug);
                    }
                    
                    if insert.is_some() {
                        expressions.insert(insert.unwrap(), dest_var.as_ref().unwrap().to_string());
                    }
                }
                Instruction::Assign { dest, .. }
                | Instruction::Cast { dest, .. }
                | Instruction::Equal { dest, .. }
                | Instruction::Greater { dest, .. }
                | Instruction::GreaterEqual { dest, .. }
                | Instruction::Len { dest, .. }
                | Instruction::Less { dest, .. }
                | Instruction::LessEqual { dest, .. }
                | Instruction::LoadConst { dest, .. }
                | Instruction::LoadString { dest, .. }
                | Instruction::MethodCall { dest: Some(dest), .. }
                | Instruction::Not { dest, .. }
                | Instruction::NotEqual { dest, .. } => {
                    // kill expressions that involve dest
                    match dest {
                        Operand::LocalVar { name, .. } => kill_expressions(&mut expressions, name, debug),
                        _ => continue  // only do CSE on local variables
                    };
                }
                _ => continue  // skip other types of instructions
            }
        }
    }

    update_occurred
}


/// Perform multiple passes over the CFG to apply the given optimizations
/// Returns the optimized CFG
pub fn optimize_dataflow(method_cfgs: &mut BTreeMap<String, CFG>, optimizations: &BTreeSet<Optimization>, globals: &BTreeMap<String, Global>, debug: bool
) -> BTreeMap<String, CFG> {
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
                        println!("Copy propagation changed {}", method);
                    }
                }
            }
        }

        if optimizations.contains(&Optimization::Cse) {
            for (method, cfg) in method_cfgs.iter_mut() {
                if common_subexpression_elimination(cfg, debug) {
                    fixed_point = false;
                    if debug {
                        println!("Common subexpression elimination elimination changed {}", method);
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

        if optimizations.contains(&Optimization::Constprop) {
            println!("Trying const prop");
            for (method, cfg) in method_cfgs.iter_mut() {
                if constant_propagation(cfg, debug) {
                    println!("Yippeee");
                    fixed_point = false;
                    if debug {
                        println!("Constant propagation changed {}", method);
                    }
                } else {
                    println!("sad");
                }
            }
        }
        
    }

    // Register allocation optimization (NOT fixed point)
    if optimizations.contains(&Optimization::Regalloc) {
        reg_alloc(method_cfgs, globals, debug);
    }
    
    method_cfgs.clone()
}