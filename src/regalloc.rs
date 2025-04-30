use crate::{cfg::CFG, tac::{Instruction, Operand}, web::*, x86::{Register, X86Operand}};
use std::{cell::RefCell, collections::{BTreeMap, BTreeSet, HashMap, HashSet, VecDeque}, hash::Hash};
use crate::state::*;


// Initialize a counter for naming webs
thread_local! {
    static WEB_COUNTER: RefCell<usize> = RefCell::new(0);
}

fn next_web_id() -> i32 {
    WEB_COUNTER.with(|counter| {
        let mut cnt = counter.borrow_mut();
        let id = *cnt;
        *cnt += 1;
        id as i32
    })
}

fn compute_instr_map(method_cfg: &CFG) -> InstructionMap {
    let mut map = HashMap::new();

    for (block_id, block) in &method_cfg.blocks {
        for (instr_idx, instr) in block.instructions.iter().enumerate() {
            map.insert(InstructionIndex {block_id: *block_id, instr_index: instr_idx as i32}, instr.clone());
        }
    }
    InstructionMap(map)
}


// only consider local variables for now (no array elements)
fn compute_def_use_sets(method_cfg: &CFG) -> BTreeMap<i32, DefUse> {
    let mut block_def_use = BTreeMap::new();

    for (block_id, _) in method_cfg.blocks.iter() {
        let mut def_set = HashSet::new();
        let mut use_set = HashSet::new();

        for instr in method_cfg
            .blocks
            .get(block_id)
            .unwrap()
            .instructions
            .iter()
        {
            match instr {
                Instruction::Assign { src, dest, .. } => {
                    add_use(&mut use_set, &def_set, src);
                    add_def(&mut def_set, dest);
                }
                Instruction::Add { left, right, dest, .. }
                | Instruction::Subtract { left, right, dest, .. }
                | Instruction::Multiply { left, right, dest, .. }
                | Instruction::Divide { left, right, dest, .. }
                | Instruction::Modulo { left, right, dest, .. }
                | Instruction::Greater { left, right, dest }
                | Instruction::Less { left, right, dest }
                | Instruction::LessEqual { left, right, dest }
                | Instruction::GreaterEqual { left, right, dest }
                | Instruction::Equal { left, right, dest }
                | Instruction::NotEqual { left, right, dest } => {
                    add_use(&mut use_set, &def_set, left);
                    add_use(&mut use_set, &def_set, right);
                    add_def(&mut def_set, dest);
                }
                Instruction::Not { expr, dest }
                | Instruction::Cast { expr, dest, .. }
                | Instruction::Len { expr, dest, .. }
                | Instruction::LoadString { src: expr, dest } => {
                    add_use(&mut use_set, &def_set, expr);
                    add_def(&mut def_set, dest);
                }
                Instruction::MethodCall { args, dest, .. } => {
                    for arg in args {
                        add_use(&mut use_set, &def_set, arg);
                    }
                    if let Some(d) = dest {
                        add_def(&mut def_set, d);
                    }
                }
                Instruction::CJmp { condition, .. } => {
                    add_use(&mut use_set, &def_set, condition);
                }
                Instruction::Ret { value, .. } => {
                    if let Some(v) = value {
                        add_use(&mut use_set, &def_set, v);
                    }
                }
                Instruction::LoadConst { dest, .. } => {
                    add_def(&mut def_set, dest);
                }
                Instruction::Exit { .. } 
                | Instruction::UJmp { .. } => {
                    // no defs or uses
                }
            }
        }

        block_def_use.insert(block_id.clone(), DefUse { defs: def_set, uses: use_set });
    }

    block_def_use
}

fn remove_def(def_set: &mut HashSet<String>, operand: &Operand) {
    if let Some(name) = get_local_var_name(operand) {
        def_set.remove(&name);
    }
}

fn get_defs(instr: &Instruction) -> Option<Vec<String>> {
    match instr {
        Instruction::Assign { dest, .. } |
        Instruction::Add { dest, .. } |
        Instruction::Subtract { dest, .. } |
        Instruction::Multiply { dest, .. } |
        Instruction::Divide { dest, .. } |
        Instruction::Modulo { dest, .. } |
        Instruction::Not { dest, .. } |
        Instruction::Cast { dest, .. } |
        Instruction::Len { dest, .. } |
        Instruction::Greater { dest, .. } |
        Instruction::Less { dest, .. } |
        Instruction::LessEqual { dest, .. } |
        Instruction::GreaterEqual { dest, .. } |
        Instruction::Equal { dest, .. } |
        Instruction::NotEqual { dest, .. } |
        Instruction::LoadString { dest, .. } |
        Instruction::LoadConst { dest, .. } => {
            if let Operand::LocalVar { name, .. } = dest {
                Some(vec![name.clone()])
            } else {
                None
            }
        }
        Instruction::MethodCall { dest, .. } => {
            if let Some(Operand::LocalVar { name, .. }) = dest {
                Some(vec![name.clone()])
            } else {
                None
            }
        }
        _ => None,
    }
}

fn get_uses(instr: &Instruction) -> Option<Vec<String>> {
    let mut uses = Vec::new();
    match instr {
        Instruction::Assign { src, .. } |
        Instruction::LoadString { src, .. } => {
            if let Some(name) = get_local_var_name(src) {
                uses.push(name);
            }
        }

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
            if let Some(name) = get_local_var_name(left) {
                uses.push(name);
            }
            if let Some(name) = get_local_var_name(right) {
                uses.push(name);
            }
        }

        Instruction::Not { expr, .. } |
        Instruction::Cast { expr, .. } |
        Instruction::Len { expr, .. } => {
            if let Some(name) = get_local_var_name(expr) {
                uses.push(name);
            }
        }

        Instruction::CJmp { condition, .. } => {
            if let Some(name) = get_local_var_name(condition) {
                uses.push(name);
            }
        }

        Instruction::MethodCall { args, .. } => {
            for arg in args {
                if let Some(name) = get_local_var_name(arg) {
                    uses.push(name);
                }
            }
        }

        Instruction::Ret { value, .. } => {
            if let Some(val) = value {
                if let Some(name) = get_local_var_name(val) {
                    uses.push(name);
                }
            }
        }

        _ => {}
    }
    if uses.is_empty() {
        None
    } else {
        Some(uses)
    }
}


// Worklist equations for liveness analysis:
//      IN[B]  = USE[B] ∪ (OUT[B] - DEF[B])
//      OUT[B] = ∪ IN[S] for all successors S of B
// Returns a tuple (in_map, out_map)
fn compute_maps(method_cfg: &mut CFG, debug: bool) -> (HashMap<i32, LiveVariables>, HashMap<i32, LiveVariables>) {
    // Compute predecessor and successor graphs
    let cfg_preds = compute_predecessors(&method_cfg);
    let cfg_succs = compute_successors(&method_cfg);

    // Variables that are live going in to this block; hashmap keyed by block_id
    let mut in_map: HashMap<i32, LiveVariables> = HashMap::new();
    // Variables that are live going out of this block; hashmap keyed by block_id
    let mut out_map: HashMap<i32, LiveVariables> = HashMap::new();

    let def_use_sets = compute_def_use_sets(method_cfg);

    // Worklist of basic block ids
    let mut worklist: VecDeque<i32> = method_cfg.blocks.keys().copied().collect::<VecDeque<i32>>();


    // Iterate until a fixed point
    while let Some(block_id) = worklist.pop_front() {
        let defs = &def_use_sets.get(&block_id).unwrap().defs;
        let uses = &def_use_sets.get(&block_id).unwrap().uses;

        // compute OUT
        let mut out = HashSet::new();
        if let Some(succs) = cfg_succs.get(&block_id) {
            for succ_id in succs {
                if let Some(in_succ) = in_map.get(succ_id) {
                    out = &out | in_succ; // union
                }
            }
        }

        // compute IN
        let mut in_set = uses.clone();
        let out_minus_def: HashSet<_> = out.difference(defs).cloned().collect();
        in_set.extend(out_minus_def);

        // update maps
        in_map.insert(block_id, in_set.clone());
        out_map.insert(block_id, out.clone());

        // if anything changed, queue predecessors
        let old_in = in_map.get(&block_id).cloned().unwrap();
        let old_out = out_map.get(&block_id).cloned().unwrap();
        if in_set != old_in || out != old_out {
            if let Some(preds) = cfg_preds.get(&block_id) {
                for pred_id in preds {
                    if !worklist.contains(pred_id) {
                        worklist.push_back(*pred_id);
                    }
                }
            }
        }
    }

    (in_map, out_map)
}

fn add_use(use_set: &mut HashSet<String>, def_set: &HashSet<String>, operand: &Operand) {
    if let Some(name) = get_local_var_name(operand) {
        // don't include uses of variables that were defined within this basic block
        if !def_set.contains(&name) {
            use_set.insert(name);
        }
    }
}

fn add_def(def_set: &mut HashSet<String>, operand: &Operand) {
    if let Some(name) = get_local_var_name(operand) {
        def_set.insert(name);
    }
}

fn get_local_var_name(operand: &Operand) -> Option<String> {
    match operand {
        Operand::LocalVar { name, .. } => Some(name.clone()),
        _ => None
    }
}

fn instr_defs_var(inst: &Instruction, var: &str) -> bool {
    if let Some(def) = inst.get_def_var() {
        if def == var {
            return true;
        }
    }
    false
}


fn uses_from_def(cfg: &CFG, start_inst: InstructionIndex, var: &str) -> HashSet<InstructionIndex> {
    let mut uses = HashSet::new();
    let mut visited = HashSet::new(); // to avoid cycles
    let mut worklist = VecDeque::new();

    // Start at (block, index) right after start_inst
    worklist.push_back((start_inst.block_id, start_inst.instr_index + 1));

    while let Some((block_id, inst_idx)) = worklist.pop_front() {
        if !visited.insert((block_id, inst_idx)) {
            continue; // Already visited this (block, idx)
        }

        let block = &cfg.blocks[&block_id];
        let instructions = &block.instructions;

        if inst_idx >= instructions.len() as i32 {
            // Went past last instruction, move to successors
            for succ in cfg.successors(block_id) {
                worklist.push_back((succ, 0));
            }
            continue;
        }

        // Start scanning from inst_idx
        for i in inst_idx..instructions.len() as i32 {
            let inst = &instructions[i as usize];
            let inst_index = InstructionIndex { block_id, instr_index: i };

            if inst.get_used_vars().contains(var) {
                uses.insert(inst_index);
            }

            if instr_defs_var(inst, var) {
                break; // Variable redefined, stop exploring this path
            }
        }

        // If scanned to end of block without redefinition:
        if (inst_idx as usize) < instructions.len() &&
            instructions[inst_idx as usize..]
                .iter()
                .all(|inst| !instr_defs_var(inst, var))
        {
            for succ in cfg.successors(block_id) {
                worklist.push_back((succ, 0)); // start at instruction 0 in the successor
            }
        }
    }

    uses
}



fn add_new_use(use_set: &mut HashSet<String>, def_set: &HashSet<String>, operand: &Operand) {
    if let Some(name) = get_local_var_name(operand) {
        // don't include uses of variables that were defined within this basic block
        if !def_set.contains(&name) {
            use_set.insert(name);
        }
    }
}


fn defs_from_use(cfg: &CFG, start_inst: InstructionIndex, var: &str) -> HashSet<InstructionIndex> {
    let mut defs = HashSet::new();
    let mut visited = HashSet::new();
    let mut worklist = VecDeque::new();

    worklist.push_back((start_inst.block_id, start_inst.instr_index));

    while let Some((block_id, inst_idx)) = worklist.pop_front() {
        if !visited.insert((block_id, inst_idx)) {
            continue;
        }

        let block = &cfg.blocks[&block_id];
        let instructions = &block.instructions;

        // move to predecessor block
        if inst_idx < 0 {
            for pred_id in cfg.predecessors(block_id) {
                let pred_block = &cfg.blocks[&pred_id];
                let last_idx = (pred_block.instructions.len() - 1) as i32;
                worklist.push_back((pred_id, last_idx));
            }
            continue;
        }

         // Search backward within the block
        for i in (0..=inst_idx).rev() {
            let inst = &instructions[i as usize];

            if instr_defs_var(inst, var) {
                defs.insert(InstructionIndex {
                    block_id,
                    instr_index: i,
                });
                break; // Found a definition => stop this path
            }
        }

        // If no def found in this block before the start_idx, move to predecessors
        let def_found_in_block = (0..=inst_idx)
            .any(|i| instr_defs_var(&instructions[i as usize], var));

        if !def_found_in_block {
            for pred_id in cfg.predecessors(block_id) {
                let pred_block = &cfg.blocks[&pred_id];
                let last_idx = (pred_block.instructions.len() - 1) as i32;
                worklist.push_back((pred_id, last_idx));
            }
        }
    }

    defs
}


// #################################################
// COMPUTE WEBS
// #################################################

/// Compute live range webs for a given method. Each web has a unique
/// web index within its given method.
fn compute_webs(method_cfg: &CFG) -> BTreeMap<i32, Web> {
    let mut webs = BTreeMap::new();
    let mut visited_defs = HashSet::new();
    let mut visited_uses = HashSet::new();

    for (block_id, block) in &method_cfg.blocks {
        for (instr_idx, inst) in block.instructions.iter().enumerate() {
            if let Some(var) = inst.get_def_var() {
                let inst_idx = InstructionIndex {
                    block_id: *block_id,
                    instr_index: instr_idx as i32,
                };

                if visited_defs.contains(&inst_idx) {
                    continue;
                }

                let mut defs = HashSet::new();
                let mut uses = HashSet::new();
                let mut worklist = VecDeque::new();

                defs.insert(inst_idx);
                visited_defs.insert(inst_idx);
                worklist.push_back(inst_idx);

                // FP Algo to collect all associated defs and uses into one web
                while let Some(def_idx) = worklist.pop_front() {
                    let reachable_uses = uses_from_def(method_cfg, def_idx, &var);

                    for use_idx in reachable_uses {
                        if visited_uses.insert(use_idx) {
                            uses.insert(use_idx);

                            let reaching_defs = defs_from_use(method_cfg, use_idx, &var);

                            for reaching_def_idx in reaching_defs {
                                if visited_defs.insert(reaching_def_idx) {
                                    defs.insert(reaching_def_idx);
                                    worklist.push_back(reaching_def_idx);
                                }
                            }
                        }
                    }
                }

                let web_id = next_web_id();
                webs.insert(
                    web_id,
                    Web {
                        id: web_id,
                        variable: var.clone(),
                        defs: defs.into_iter().collect(),
                        uses: uses.into_iter().collect(),
                    },
                );
            }
        }
    }

    webs
}





// #################################################
// COMPUTE LIVENESS & INTERFERENCE
// #################################################


/// Computes liveness for each web_id, returning the in_map and out_map for the web
// /// at each basic block. Similar to DCE liveness but per-web instead of per-var.
// fn compute_webs_liveness(
//     method_cfg: &CFG,
//     method_webs: &BTreeMap<i32, Web>,
//     debug: bool,
// ) -> (HashMap<i32, HashSet<i32>>, HashMap<i32, HashSet<i32>>) {
//     let predecessors = compute_predecessors(&method_cfg);
//     let successors = compute_successors(&method_cfg);

//     let mut in_map: HashMap<i32, HashSet<i32>> = HashMap::new();  // block_id -> live-in webs
//     let mut out_map: HashMap<i32, HashSet<i32>> = HashMap::new(); // block_id -> live-out webs

//     let mut worklist: VecDeque<i32> = method_cfg.blocks.keys().copied().collect();

//     // Map each instruction to its web_id (like before)
//     let mut instr_to_web: HashMap<InstructionIndex, i32> = HashMap::new();
//     for (web_id, web) in method_webs {
//         for &def in &web.defs {
//             instr_to_web.insert(def, *web_id);
//         }
//         for &use_site in &web.uses {
//             instr_to_web.insert(use_site, *web_id);
//         }
//     }

//     while let Some(block_id) = worklist.pop_front() {
//         // OUT[B] = ⋃ IN[S] for all successors
//         let out_set: HashSet<i32> = successors
//             .get(&block_id)
//             .map(|succs| {
//                 succs.iter()
//                     .filter_map(|s| in_map.get(s))
//                     .fold(HashSet::new(), |mut acc, s| {
//                         acc.extend(s.clone());
//                         acc
//                     })
//             })
//             .unwrap_or_default();

//         let mut use_set = HashSet::new();
//         let mut def_set = HashSet::new();

//         for (i, instr) in method_cfg.blocks.get(&block_id).unwrap().instructions.iter().enumerate().rev() {
//             let inst_idx = InstructionIndex {
//                 block_id,
//                 instr_index: i as i32,
//             };

//             if let Some(&web_id) = instr_to_web.get(&inst_idx) {
//                 // If this instruction uses a variable
//                 if method_webs.get(&web_id).unwrap().uses.contains(&inst_idx) {
//                     use_set.insert(web_id);
//                 }

//                 // If this instruction defines a variable
//                 if method_webs.get(&web_id).unwrap().defs.contains(&inst_idx) {
//                     def_set.insert(web_id);
//                 }
//             }
//         }

//         // IN[B] = USE[B] ∪ (OUT[B] - DEF[B])
//         let mut in_set = use_set.clone();
//         in_set.extend(out_set.difference(&def_set).copied());

//         // If IN or OUT changed, propagate backwards
//         if in_map.get(&block_id) != Some(&in_set) || out_map.get(&block_id) != Some(&out_set) {
//             in_map.insert(block_id, in_set);
//             out_map.insert(block_id, out_set);

//             if let Some(preds) = predecessors.get(&block_id) {
//                 for pred in preds {
//                     worklist.push_back(*pred);
//                 }
//             }
//         }
//     }

//     // if debug {
//     //     println!("Computed Web Liveness: IN = {:#?}", in_map);
//     //     println!("Computed Web Liveness: OUT = {:#?}", out_map);

//     (in_map, out_map)
// }



/// Add an edge between two variables if they are ever live at the same program point
/// This is a liveness analysis.
// fn compute_interference(method_cfg: &CFG, method_webs: &BTreeMap<i32, Web>, debug: bool) -> InterferenceGraph {
//     let (in_map, out_map) = compute_webs_liveness(method_cfg, method_webs, debug);
//     let mut graph = InterferenceGraph::new();

//     // For mapping InstructionIndex -> web_id
//     let mut inst_to_web: HashMap<InstructionIndex, i32> = HashMap::new();
//     for (web_id, web) in method_webs {
//         for &def in &web.defs {
//             inst_to_web.insert(def, *web_id);
//         }
//         for &use_site in &web.uses {
//             inst_to_web.insert(use_site, *web_id);
//         }
//     }

//     // For each block
//     for (block_id, block) in &method_cfg.blocks {
//         // Start with live-out of the block
//         let mut live: HashSet<i32> = out_map.get(block_id).cloned().unwrap_or_default();

//         // Walk instructions in reverse (backwards analysis)
//         for (i, instr) in block.instructions.iter().enumerate().rev() {
//             let inst_idx = InstructionIndex {
//                 block_id: *block_id,
//                 instr_index: i as i32,
//             };

//             if let Some(&web_id) = inst_to_web.get(&inst_idx) {
//                 // If this instruction defines a web
//                 if method_webs[&web_id].defs.contains(&inst_idx) {
//                     // Interfere with all currently live webs
//                     for &live_web in &live {
//                         if live_web == web_id {
//                             continue;
//                         }

//                         // Add edges between defs of web_id and live_web
//                         for &d1 in &method_webs[&web_id].defs {
//                             for &d2 in &method_webs[&live_web].defs {
//                                 graph.add_edge(d1, d2);
//                             }
//                         }
//                     }

//                     // Kill: definition overrides old value
//                     live.remove(&web_id);
//                 }

//                 // If this instruction uses a web, it becomes live
//                 if method_webs[&web_id].uses.contains(&inst_idx) {
//                     live.insert(web_id);
//                 }
//             }
//         }
//     }

//     // Add all definition sites as graph nodes
//     for (_web_id, web) in method_webs {
//         for &def in &web.defs {
//             graph.nodes.insert(def);
//         }
//     }

//     if debug {
//         println!("Interference graph: {:#?}", graph);
//     }

//     graph
// }

// fn compute_interference(method_cfg: &CFG, method_webs: &BTreeMap<i32, Web>, debug: bool) -> InterferenceGraph { 
//     // For each web, compute HashSet<Webid, InstructionIndex>

//     // iterate over each web
//     // for each isntrution in web: add to HashMap<InstructionIndex, hashset<webids>> with webids that use this

//     // then, create the interference graph by iterating over the hashmap and adding edge between webs if they are both in the same value

// }


pub fn compute_interference(
    method_webs: &BTreeMap<i32, Web>,
    debug: bool,
) -> InterferenceGraph {
    let mut instr_to_webs: HashMap<InstructionIndex, HashSet<i32>> = HashMap::new();

    // Step 1: Map each instruction to the set of web IDs active (defs and uses)
    for (&web_id, web) in method_webs {
        for &instr in web.defs.iter().chain(web.uses.iter()) {
            instr_to_webs
                .entry(instr)
                .or_insert_with(HashSet::new)
                .insert(web_id);
        }
    }

    // Step 2: Initialize graph with all web IDs as nodes
    let mut graph = InterferenceGraph::new();
    for &web_id in method_webs.keys() {
        graph.nodes.insert(web_id);
    }

    // Step 3: Add edges between all pairs of web IDs that are co-live at any instruction
    for web_ids in instr_to_webs.values() {
        let ids: Vec<&i32> = web_ids.iter().collect();
        for i in 0..ids.len() {
            for j in (i + 1)..ids.len() {
                graph.add_edge(*ids[i], *ids[j]);
            }
        }
    }

    if debug {
        println!("Interference Graph: {:#?}", graph);
    }

    graph
}

/// Spill webs onto the stack or split their live ranges
fn compute_spill_costs() {
    todo!()
}


/// Performs graph-coloring algorithm, assigning every web
/// either a register or a stack space.
pub fn assign_registers(
    interference: &InterferenceGraph,
    registers: &BTreeSet<X86Operand>,

) -> HashMap<i32, Option<X86Operand>> {
    let mut stack: Vec<i32> = Vec::new();
    let mut removed: HashSet<i32> = HashSet::new();
    let graph = interference.edges.clone(); // Clone the interference graph

    let k = registers.len();

    // Step 1: Simplify graph
    loop {
        let mut removed_any = false;

        for (&node, neighbors) in &graph {
            if removed.contains(&node) {
                continue;
            }

            let degree = neighbors.iter().filter(|&&n| !removed.contains(&n)).count();

            if degree < k {
                stack.push(node);
                removed.insert(node);
                removed_any = true;
            }
        }

        if !removed_any {
            break;
        }
    }

    // Step 2: Spill if necessary (remaining nodes all have degree >= k)
    for &node in graph.keys() {
        if !removed.contains(&node) {
            stack.push(node); // May not be colorable
            removed.insert(node);
        }
    }

    // Step 3: Assign colors
    let mut coloring: HashMap<i32, Option<X86Operand>> = HashMap::new();

    while let Some(node) = stack.pop() {
        let mut used_colors = HashSet::new();

        if let Some(neighbors) = interference.neighbors(&node) {
            for neighbor in neighbors {
                if let Some(Some(color)) = coloring.get(neighbor) {
                    used_colors.insert(color.clone());
                }
            }
        }

        // Assign the first available register not used by neighbors
        let reg = registers.iter().find(|r| !used_colors.contains(*r)).cloned();
        coloring.insert(node, reg); // If no register available, reg = None (spill)
    }

    coloring
}




/// Modify the CFG based on the register assignments
fn apply_reg_assignments(method_cfg: &mut HashMap<String, CFG>, assignments: HashMap<Web, Option<X86Operand>>) {
    todo!()

}


/// Mutates CFG based on register assignments
pub fn reg_alloc(method_cfgs: &mut HashMap<String, CFG>, debug: bool) {
    // let mut webs: HashMap<&String, BTreeMap<i32, Web>> = HashMap::new();
    let mut method_to_instrs: HashMap<String, InstructionMap> = HashMap::new();

    // Start with just the truly general purpose registers
    let usable_registers: BTreeSet<X86Operand> = vec![
        // Caller saved
        // X86Operand::Reg(Register::Rax),
        // X86Operand::Reg(Register::Rcx),
        // X86Operand::Reg(Register::Rdx),
        // X86Operand::Reg(Register::Rip), 
        // X86Operand::Reg(Register::Rsi),
        // X86Operand::Reg(Register::Rdi),
        // X86Operand::Reg(Register::R8),
        // X86Operand::Reg(Register::R9),
        // X86Operand::Reg(Register::R10),
        // X86Operand::Reg(Register::R11),
        
        // Callee saved
        // X86Operand::Reg(Register::Rbx),
        // X86Operand::Reg(Register::Rbp),
        // X86Operand::Reg(Register::Rsp), 
        X86Operand::Reg(Register::R12),
        X86Operand::Reg(Register::R13),
        X86Operand::Reg(Register::R14),
        X86Operand::Reg(Register::R15),
    ].into_iter().collect();

    

    for (method_name, method_cfg) in method_cfgs {
        // Populate instruction map for later use
        let instr_map = compute_instr_map(method_cfg);
        method_to_instrs.insert(method_name.to_string(), instr_map);

        // Compute register assignments
        let method_webs: BTreeMap<i32, Web> = compute_webs(method_cfg);
        println!("webs for {method_name} is {:#?}", method_webs);

        // TODO: broken
        let interference = compute_interference(&method_webs, debug);
        println!("interference graph for {method_name} is {:#?}", interference);

        let register_assignments = assign_registers(&interference, &usable_registers);
        for (web_id, reg) in register_assignments {
            println!("assigning web {:#?} to register {:#?}", method_webs.get(&web_id), reg);
        }
        
        // TODO: apply assignments
    }

}


