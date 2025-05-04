use crate::{cfg::CFG, tac::{Instruction, Operand}, utils::print::html_web_graphs, web::*, x86::{Register, X86Operand}};
use std::{cell::RefCell, collections::{BTreeMap, BTreeSet, HashMap, HashSet, VecDeque}};
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
    let mut map = BTreeMap::new();

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
        let mut def_set = BTreeSet::new();
        let mut use_set = BTreeSet::new();

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

fn remove_def(def_set: &mut BTreeSet<String>, operand: &Operand) {
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
fn compute_maps(method_cfg: &mut CFG, debug: bool) -> (BTreeMap<i32, LiveVariables>, BTreeMap<i32, LiveVariables>) {
    // Compute predecessor and successor graphs
    let cfg_preds = compute_predecessors(&method_cfg);
    let cfg_succs = compute_successors(&method_cfg);

    // Variables that are live going in to this block; hashmap keyed by block_id
    let mut in_map: BTreeMap<i32, LiveVariables> = BTreeMap::new();
    // Variables that are live going out of this block; hashmap keyed by block_id
    let mut out_map: BTreeMap<i32, LiveVariables> = BTreeMap::new();

    let def_use_sets = compute_def_use_sets(method_cfg);

    // Worklist of basic block ids
    let mut worklist: VecDeque<i32> = method_cfg.blocks.keys().copied().collect::<VecDeque<i32>>();

    
    // Iterate until a fixed point
    while let Some(block_id) = worklist.pop_front() {
        let defs = &def_use_sets.get(&block_id).unwrap().defs;
        let uses = &def_use_sets.get(&block_id).unwrap().uses;

        // compute OUT
        let mut out = BTreeSet::new();
        if let Some(succs) = cfg_succs.get(&block_id) {
            for succ_id in succs {
                if let Some(in_succ) = in_map.get(succ_id) {
                    out = &out | in_succ; // union
                }
            }
        }

        // compute IN
        let mut in_set = uses.clone();
        let out_minus_def: BTreeSet<_> = out.difference(defs).cloned().collect();
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

fn add_use(use_set: &mut BTreeSet<String>, def_set: &BTreeSet<String>, operand: &Operand) {
    if let Some(name) = get_local_var_name(operand) {
        // don't include uses of variables that were defined within this basic block
        if !def_set.contains(&name) {
            use_set.insert(name);
        }
    }
}

fn add_def(def_set: &mut BTreeSet<String>, operand: &Operand) {
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


fn uses_from_def(cfg: &CFG, start_inst: InstructionIndex, var: &str) -> BTreeSet<InstructionIndex> {
    let mut uses = BTreeSet::new();
    let mut visited = BTreeSet::new(); // to avoid cycles
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



fn add_new_use(use_set: &mut BTreeSet<String>, def_set: &BTreeSet<String>, operand: &Operand) {
    if let Some(name) = get_local_var_name(operand) {
        // don't include uses of variables that were defined within this basic block
        if !def_set.contains(&name) {
            use_set.insert(name);
        }
    }
}


fn defs_from_use(cfg: &CFG, start_inst: InstructionIndex, var: &str) -> BTreeSet<InstructionIndex> {
    let mut defs = BTreeSet::new();
    let mut visited = BTreeSet::new();
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
                // unreachable block
                if pred_id == -1 {
                    continue;
                }
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
    let mut visited_defs = BTreeSet::new();

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

                let mut defs = BTreeSet::new();
                let mut uses = BTreeSet::new();
                let mut worklist = VecDeque::new();

                defs.insert(inst_idx);
                visited_defs.insert(inst_idx);
                worklist.push_back(inst_idx);

                // FP Algo to collect all associated defs and uses into one web
                while let Some(def_idx) = worklist.pop_front() {
                    let reachable_uses = uses_from_def(method_cfg, def_idx, &var);

                    for use_idx in reachable_uses {
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
// COMPUTE INTERFERENCE
// #################################################

fn block_is_part_of_loop(
    block_id: i32,
    preds: &HashMap<i32, HashSet<i32>>,
) -> bool {
    let mut visited = BTreeSet::new();
    let mut stack = vec![block_id];

    while let Some(current) = stack.pop() {
        if !visited.insert(current) {
            continue;
        }

        // If any predecessor is >= block_id, assume it's a backedge
        if let Some(pred_blocks) = preds.get(&current) {
            for &pred in pred_blocks {
                if pred >= block_id {
                    return true; // found a backedge → this block is part of a loop
                }
                stack.push(pred);
            }
        }
    }

    false // no backedge found
}


/// Compute use-liveness spans for all uses in all webs.
/// Requires instruction map and full CFG (with block predecessors).
/// Returns {(web_id : a_use_id_in_web) : {live region from that use to all defs}}
pub fn compute_use_liveness_spans(
    webs: &BTreeMap<i32, Web>,
    method_cfg: &CFG,
    instr_map: &InstructionMap,
) -> BTreeMap<(i32, InstructionIndex), BTreeSet<InstructionIndex>> {
    let mut result = BTreeMap::new();
    let preds = compute_predecessors(method_cfg);
    

    for (web_id, web) in webs {
        let var = &web.variable;

        for use_idx in &web.uses {
            if web.variable == "j".to_string(){
                println!("use idx: {:#?}", use_idx);
            }

            let mut visited = BTreeSet::new();
            let mut worklist = VecDeque::new();
            worklist.push_back(*use_idx);

            let mut live_span = BTreeSet::new();

            while let Some(idx) = worklist.pop_front() {
                if visited.contains(&idx) {
                    continue;
                }
                visited.insert(idx);

                let instr = instr_map.0.get(&idx).unwrap();
                // Stop walking if we hit a definition of the variable
                // Check if this def kills the variable in this control flow
                if let Some(def) = instr.get_def_var() {
                    // TODO: visited set for instructions; remove use from worklist
                    if def == *var && !web.uses.contains(&idx) {
                            continue;
                    }
                }

                if web.variable == "j".to_string(){
                    println!("inserting: {:#?}", idx );
                }

                live_span.insert(idx);

                // Walk upward to previous instruction in block or predecessor blocks
                if idx.instr_index > 0 {
                    let prev_idx = InstructionIndex {
                        block_id: idx.block_id,
                        instr_index: idx.instr_index - 1,
                    };
                    worklist.push_back(prev_idx);

                } else if let Some(pred_blocks) = preds.get(&idx.block_id) {
                    for &pred_block in pred_blocks {
                        let pred_instrs = &method_cfg.blocks[&pred_block].instructions;
                        if !pred_instrs.is_empty() {
                            let prev_idx = InstructionIndex {
                                block_id: pred_block,
                                instr_index: pred_instrs.len() as i32 - 1,
                            };
                            worklist.push_back(prev_idx);
                        }
                    }
                }
            }

            result.insert((*web_id, *use_idx), live_span);
        }
    }

    result
}


// compute DCE liveness IN and OUT for each block
// GEN, KILL, PASS THROUGH
// make { instruction: live_webs} 
// for each PASS THROUGH, marked as live for entire basic block (other ones go instruction level)
// for each KILL: 

pub fn combine_spans_per_web(
    use_liveness_spans: &BTreeMap<(i32, InstructionIndex), BTreeSet<InstructionIndex>>
) -> BTreeMap<i32, BTreeSet<InstructionIndex>> {
    let mut out: BTreeMap<i32, BTreeSet<InstructionIndex>> = BTreeMap::new();

    for ((web_id, _web_use), span_instrs) in use_liveness_spans {
        let entry = out.entry(*web_id).or_insert_with(BTreeSet::new);
        for instr in span_instrs {
            entry.insert(*instr);
        }
    }
    out
}


pub fn build_interference_graph_from_spans(
    use_liveness_spans: &BTreeMap<(i32, InstructionIndex), BTreeSet<InstructionIndex>>,
) -> InterferenceGraph {
    let mut graph = InterferenceGraph::new();
    println!("liveness spans are: {:#?}", combine_spans_per_web(use_liveness_spans));

    let items: Vec<_> = use_liveness_spans.iter().collect();

    for i in 0..items.len() {
        let ((web_id_a, _), span_a) = items[i];
        for j in (i + 1)..items.len() {
            let ((web_id_b, _), span_b) = items[j];

            if !span_a.is_disjoint(span_b) {
                graph.add_edge(*web_id_a, *web_id_b);
            }
        }
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
    method_webs: &BTreeMap<i32, Web>,
) -> BTreeMap<Web, Option<X86Operand>> {
    let mut stack: Vec<i32> = Vec::new();
    let mut removed: BTreeSet<i32> = BTreeSet::new();
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
    let mut coloring: BTreeMap<Web, Option<X86Operand>> = BTreeMap::new();

    while let Some(node) = stack.pop() {
        let mut used_colors = BTreeSet::new();

        if let Some(neighbors) = interference.neighbors(&node) {
            for neighbor in neighbors {
                let neighbor_web = method_webs.get(neighbor).expect("Should have found neighbor web");
                if let Some(Some(color)) = coloring.get(neighbor_web) {
                    used_colors.insert(color.clone());
                }
            }
        }

        // Assign the first available register not used by neighbors
        let reg = registers.iter().find(|r| !used_colors.contains(*r)).cloned();
        let key = method_webs.get(&node).expect("Shoudl have found vertex node");
        coloring.insert(key.clone(), reg); // If no register available, reg = None (spill)
    }

    coloring
}

// Taking a reference to an instruction, add in the info of which register to use in the definition of a variable
fn add_def_reg(instruction: &mut Instruction, register: &Option<X86Operand>) {
    match instruction {
        Instruction::Add { dest, .. }
        | Instruction::Subtract { dest, .. }
        | Instruction::Multiply { dest, .. }
        | Instruction::Divide { dest, .. }
        | Instruction::Modulo { dest, .. }
        | Instruction::Not { dest, .. }
        | Instruction::Cast { dest, ..}
        | Instruction::Len { dest, ..}
        | Instruction::Greater { dest, .. }
        | Instruction::Less { dest, .. }
        | Instruction::LessEqual { dest, .. }
        | Instruction::GreaterEqual { dest, .. }
        | Instruction::Equal { dest, .. }
        | Instruction::NotEqual { dest, .. }
        | Instruction::Assign { dest, .. }
        | Instruction::LoadConst { dest, .. }
        | Instruction::LoadString { dest, .. } => {
            dest.set_reg(register);
        }
        Instruction::MethodCall { dest, .. } => {
            if let Some(dest_op) = dest{
                dest_op.set_reg(register);
            }
        }
        Instruction::UJmp { .. }
        | Instruction::CJmp { .. }
        | Instruction::Ret { .. }
        | Instruction::Exit { .. } => {
            panic!("Should not be adding register to non_compatible instruction");
        }
    }
}


// Taking a reference to an instruction, add in the info of which register to use in the use of a variable with 
// name: variable
fn add_use_reg(instruction: &mut Instruction, register: &Option<X86Operand>, variable: &String) {
    // Recursively attempts to set the register on an operand (including nested uses in array indices)
    fn try_set(op: &mut Operand, reg: &Option<X86Operand>, var: &String) {
        if op.to_string() == *var {
            op.set_reg(reg);
        }

        match op {
            Operand::LocalArrElement { index, .. }
            | Operand::GlobalArrElement { index, .. } => {
                try_set(index, reg, var); // Recurse into index
            }
            _ => {}
        }
    }

    match instruction {
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
            try_set(left, register, variable);
            try_set(right, register, variable);
        }

        Instruction::Not { expr, .. }
        | Instruction::Cast { expr, .. }
        | Instruction::Len { expr, .. } => {
            try_set(expr, register, variable);
        }

        Instruction::Assign { src, dest, .. } => {
            try_set(src, register, variable);
            try_set(dest, register, variable); // Handles A[t1] = b case
        }

        Instruction::LoadString { src, .. } => {
            try_set(src, register, variable);
        }

        Instruction::CJmp { condition, .. } => {
            try_set(condition, register, variable);
        }

        Instruction::Ret { value, .. } => {
            if let Some(val) = value {
                try_set(val, register, variable);
            }
        }

        Instruction::MethodCall { args, .. } => {
            for arg in args.iter_mut() {
                try_set(arg, register, variable);
            }
        }

        Instruction::Exit { .. }
        | Instruction::UJmp { .. }
        | Instruction::LoadConst { .. } => {
            panic!("add_use_reg called on instruction with no register-assignable uses");
        }
    }
}



/// Modify the CFG based on the register assignments
fn apply_reg_assignments(method_cfg: &mut CFG, assignments: BTreeMap<Web, Option<X86Operand>>) {
    // println!("applying assignments:\n");
    // for (web, op) in assignments.clone() {
    //     if op.is_some() {
    //         println!(" {}: {:#?}", web.id, op.unwrap());
    //     } else {
    //         println!(" {}: {:#?}", web.id, op);
    //     }
    // }
    for (web, reg_opt) in assignments.iter() {
        // if *reg_opt == Some(X86Operand::Reg(Register::R13)) && (web.id == 16  || web.id == 17){
        //     continue;
        // }
        for web_def in web.defs.iter() {
            // Replace def instruction so def operand has register: reg_opt
            let mut instruction = method_cfg.get_instruction(web_def.block_id, web_def.instr_index);
            add_def_reg(instruction, reg_opt);
        }
        for web_use in web.uses.iter() {
            // Replace use instruction so use operand has register: reg_opt
            let mut instruction = method_cfg.get_instruction(web_use.block_id, web_use.instr_index);
            add_use_reg(instruction, reg_opt, &web.variable);
        }
    }

}


/// Mutates CFG based on register assignments
pub fn reg_alloc(method_cfgs: &mut BTreeMap<String, CFG>, debug: bool) {
    // for (name, cfg) in method_cfgs.clone() {
    //     println!("{}", name);
    // }
    // let mut webs: HashMap<&String, BTreeMap<i32, Web>> = HashMap::new();
    let mut method_to_instrs: BTreeMap<String, InstructionMap> = BTreeMap::new();

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

    // For building HTML visualizer
    let mut web_data: BTreeMap<String, (BTreeMap<i32, Web>, InterferenceGraph, InstructionMap)> = BTreeMap::new();
    let mut register_data: BTreeMap<String, BTreeMap<i32, Option<X86Operand>>> = BTreeMap::new();

    for (method_name, method_cfg) in method_cfgs {
        // Build instruction map
        let instr_map = compute_instr_map(method_cfg);
        method_to_instrs.insert(method_name.to_string(), instr_map.clone());

        // Build webs and interference graph
        let method_webs = compute_webs(method_cfg);
        let live_spans = compute_use_liveness_spans(&method_webs, method_cfg, &instr_map);
        let interference = build_interference_graph_from_spans(&live_spans);

        if debug {
            println!("webs for {method_name} is {:#?}", method_webs);
            println!("interference graph for {method_name} is {:#?}", interference);
        }

        // Save data for visualization
        web_data.insert(method_name.clone(), (method_webs.clone(), interference.clone(), instr_map.clone()));


        // Assign registers
        let register_assignments = assign_registers(&interference, &usable_registers, &method_webs);
        if debug {
            for (web, reg) in &register_assignments {
                println!("assigning web {:#?} to register {:#?}", web, reg);
            }
        }

        // register information for visualization
        register_data.insert(method_name.clone(), register_assignments
            .iter()
            .map(|(web, reg)| (web.id, reg.clone()))
            .collect());

        apply_reg_assignments(method_cfg, register_assignments);
    }

    // Generate visual HTML for all methods
    html_web_graphs(&web_data, &register_data, "reg_alloc.html".to_string());

}


