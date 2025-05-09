use crate::codegen::ARGUMENT_REGISTERS;
use crate::state::*;
use crate::{
    cfg::{Global, CFG},
    tac::{Instruction, Operand},
    utils::print::html_web_graphs,
    web::*,
    x86::{Register, X86Operand},
};
use std::{
    cell::RefCell,
    collections::{BTreeMap, BTreeSet, HashMap, HashSet, VecDeque},
};

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
            map.insert(
                InstructionIndex {
                    block_id: *block_id,
                    instr_index: instr_idx as i32,
                },
                instr.clone(),
            );
            map.insert(
                InstructionIndex {
                    block_id: *block_id,
                    instr_index: instr_idx as i32,
                },
                instr.clone(),
            );
        }
    }
    InstructionMap(map)
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
            let inst_index = InstructionIndex {
                block_id,
                instr_index: i,
            };

            if inst.get_used_vars().contains(var) {
                uses.insert(inst_index);
            }

            if instr_defs_var(inst, var) {
                break; // Variable redefined, stop exploring this path
            }
        }

        // If scanned to end of block without redefinition:
        if (inst_idx as usize) < instructions.len()
            && instructions[inst_idx as usize..]
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
        let def_found_in_block =
            (0..=inst_idx).any(|i| instr_defs_var(&instructions[i as usize], var));

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
fn compute_webs(method_cfg: &CFG, globals: &BTreeMap<String, Global>) -> BTreeMap<i32, Web> {
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

                // Do not assign webs to globals
                if globals.contains_key(&var) {
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

fn block_is_part_of_loop(block_id: i32, preds: &HashMap<i32, HashSet<i32>>) -> bool {
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

pub fn combine_spans_per_web(
    use_liveness_spans: &BTreeMap<(i32, InstructionIndex), BTreeSet<InstructionIndex>>,
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
    precolored: &HashMap<i32, X86Operand>,
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

    // Step 2.5, insert the precoloring
    let mut coloring: BTreeMap<Web, Option<X86Operand>> = BTreeMap::new();
    // Step 2.5: Record all precolored webs in coloring
    for (&node, reg) in precolored {
        if let Some(web) = method_webs.get(&node) {
            coloring.insert(web.clone(), Some(reg.clone()));
        }
    }

    // Step 3: Assign colors

    while let Some(node) = stack.pop() {
        // skip if already precolored
        if precolored.contains_key(&node) {
            continue;
        }

        let mut used_colors = BTreeSet::new();

        if let Some(neighbors) = interference.neighbors(&node) {
            for neighbor in neighbors {
                let neighbor_web = method_webs
                    .get(neighbor)
                    .expect("Should have found neighbor web");
                if let Some(Some(color)) = coloring.get(neighbor_web) {
                    used_colors.insert(color.clone());
                }
            }
        }

        // Assign the first available register not used by neighbors
        let reg = registers
            .iter()
            .find(|r| !used_colors.contains(*r))
            .cloned();
        let key = method_webs
            .get(&node)
            .expect("Shoudl have found vertex node");
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
        | Instruction::Cast { dest, .. }
        | Instruction::Len { dest, .. }
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
            if let Some(dest_op) = dest {
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
            Operand::LocalArrElement { index, .. } | Operand::GlobalArrElement { index, .. } => {
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

        Instruction::Exit { .. } | Instruction::UJmp { .. } | Instruction::LoadConst { .. } => {
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
            let instruction = method_cfg.get_instruction(web_def.block_id, web_def.instr_index);
            add_def_reg(instruction, reg_opt);
        }
        for web_use in web.uses.iter() {
            // Replace use instruction so use operand has register: reg_opt
            let instruction = method_cfg.get_instruction(web_use.block_id, web_use.instr_index);
            add_use_reg(instruction, reg_opt, &web.variable);
        }
    }
}

/// Mutates CFG based on register assignments
pub fn reg_alloc(
    method_cfgs: &mut BTreeMap<String, CFG>,
    globals: &BTreeMap<String, Global>,
    debug: bool,
) {
    // for (name, cfg) in method_cfgs.clone() {
    //     println!("{}", name);
    // }
    // let mut webs: HashMap<&String, BTreeMap<i32, Web>> = HashMap::new();
    let mut method_to_instrs: BTreeMap<String, InstructionMap> = BTreeMap::new();

    // Start with just the truly general purpose registers
    let usable_registers: BTreeSet<X86Operand> = vec![
        // Caller saved (volatile) — can be freely used, but caller must save if needed across calls
        // X86Operand::Reg(Register::Rax),  // Return value
        X86Operand::Reg(Register::Rdi), // 1st argument
        X86Operand::Reg(Register::Rsi), // 2nd argument
        // X86Operand::Reg(Register::Rdx),  // 3rd argument, also used in division
        X86Operand::Reg(Register::Rcx), // 4th argument, also shift count, loop counter
        X86Operand::Reg(Register::R8),  // 5th argument
        X86Operand::Reg(Register::R9),  // 6th argument
        // X86Operand::Reg(Register::R10),  // Scratch (caller-saved temp), rarely reserved by ABI
        // X86Operand::Reg(Register::R11),  // Scratch (caller-saved temp), rarely reserved by ABI

        // // Callee saved (non-volatile) — must be preserved by callee across calls
        X86Operand::Reg(Register::Rbx), // Callee saved (general-purpose)
        // X86Operand::Reg(Register::Rbp),  // Frame/base pointer
        // X86Operand::Reg(Register::Rsp),  // Stack pointer (NEVER allocate)
        X86Operand::Reg(Register::R12), // Callee saved (you’re using this for allocation)
        X86Operand::Reg(Register::R13), // Callee saved
        X86Operand::Reg(Register::R14), // Callee saved
        X86Operand::Reg(Register::R15), // Callee saved

                                        // Special
                                        // X86Operand::Reg(Register::Rip),  // Instruction pointer (NEVER allocate)
    ]
    .into_iter()
    .collect();

    // For building HTML visualizer
    let mut web_data: BTreeMap<String, (BTreeMap<i32, Web>, InterferenceGraph, InstructionMap)> =
        BTreeMap::new();
    let mut register_data: BTreeMap<String, BTreeMap<i32, Option<X86Operand>>> = BTreeMap::new();

    for (method_name, method_cfg) in method_cfgs {
        // Build instruction map
        let instr_map = compute_instr_map(method_cfg);
        method_to_instrs.insert(method_name.to_string(), instr_map.clone());

        // Build webs and interference graph
        let method_webs = compute_webs(method_cfg, globals);
        let live_spans = compute_use_liveness_spans(&method_webs, method_cfg, &instr_map);
        let interference = build_interference_graph_from_spans(&live_spans);

        if debug {
            // println!("webs for {method_name} is {:#?}", method_webs);
            // println!("interference graph for {method_name} is {:#?}", interference);
        }

        // Save data for visualization
        web_data.insert(
            method_name.clone(),
            (method_webs.clone(), interference.clone(), instr_map.clone()),
        );

        // Precolor all of the arguments to stay in their registers so they dont clobber eachother
        // TODO what if the arguments arent that important to be in register for later
        let mut precolored: HashMap<i32, X86Operand> = HashMap::new();
        for (&pos, temp) in &method_cfg.param_to_temp {
            if pos < 6 {
                let var = &temp.name;
                // find the web whose `variable == var`
                if let Some((&web_id, _)) = method_webs.iter().find(|(_, w)| &w.variable == var) {
                    precolored.insert(web_id, ARGUMENT_REGISTERS[pos as usize].clone());
                }
            }
        }

        // Assign registers
        // println!("Assigning Regs");
        let register_assignments =
            assign_registers(&interference, &usable_registers, &method_webs, &precolored);
        if debug {
            // for (web, reg) in &register_assignments {
            //     println!("assigning web {:#?} to register {:#?}", web, reg);
            // }
        }
        // println!("Finish Assigning");
        // println!("Finish Assigning");

        // register information for visualization
        register_data.insert(
            method_name.clone(),
            register_assignments
                .iter()
                .map(|(web, reg)| (web.id, reg.clone()))
                .collect(),
        );
        register_data.insert(
            method_name.clone(),
            register_assignments
                .iter()
                .map(|(web, reg)| (web.id, reg.clone()))
                .collect(),
        );

        apply_reg_assignments(method_cfg, register_assignments);
    }

    // Generate visual HTML for all methods
    //html_web_graphs(&web_data, &register_data, "reg_alloc.html".to_string());
}
