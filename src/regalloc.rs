use crate::{cfg::CFG, tac::Instruction, x86::X86Operand, tac::Operand, tac::InstructionRef};
use std::{cell::RefCell, collections::{BTreeMap, HashMap, HashSet, VecDeque}};

// Def and reachable uses must be in same web
// All defs that reach a common use must be in same web
// All defs and uses to the variable within a web will be 
// done on the same register.
#[derive(Debug, Eq, PartialEq, Clone)]
struct Web {
    id: i32,
    variable: String,
    defs: Vec<InstructionRef>,
    uses: Vec<InstructionRef>,
}

#[derive(Debug, Eq, PartialEq, Clone)]
struct InterferenceGraph {
    id_to_web: BTreeMap<i32, Web>,
    outgoing_edges: BTreeMap<i32, HashSet<Edge>>
}

#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub struct Edge {
    pub src_web: i32,
    pub dest_web: i32,
}

struct DefUse {
    defs: HashSet<String>, // all variables that are defined in this basic block
    uses: HashSet<String> // all variables that are used in this basic block
}

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
                    process_use(&mut use_set, &def_set, src);
                    process_def(&mut def_set, dest);
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
                    process_use(&mut use_set, &def_set, left);
                    process_use(&mut use_set, &def_set, right);
                    process_def(&mut def_set, dest);
                }
                Instruction::Not { expr, dest }
                | Instruction::Cast { expr, dest, .. }
                | Instruction::Len { expr, dest, .. }
                | Instruction::LoadString { src: expr, dest } => {
                    process_use(&mut use_set, &def_set, expr);
                    process_def(&mut def_set, dest);
                }
                Instruction::MethodCall { args, dest, .. } => {
                    for arg in args {
                        process_use(&mut use_set, &def_set, arg);
                    }
                    if let Some(d) = dest {
                        process_def(&mut def_set, d);
                    }
                }
                Instruction::CJmp { condition, .. } => {
                    process_use(&mut use_set, &def_set, condition);
                }
                Instruction::Ret { value, .. } => {
                    if let Some(v) = value {
                        process_use(&mut use_set, &def_set, v);
                    }
                }
                Instruction::LoadConst { dest, .. } => {
                    process_def(&mut def_set, dest);
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

fn process_use(use_set: &mut HashSet<String>, def_set: &HashSet<String>, operand: &Operand) {
    if let Some(name) = get_local_var_name(operand) {
        // don't include uses of variables that were defined within this basic block
        if !def_set.contains(&name) {
            use_set.insert(name);
        }
    }
}

fn process_def(def_set: &mut HashSet<String>, operand: &Operand) {
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

fn uses_from_def(cfg: &CFG, start_inst: *const Instruction, var: &str) -> HashSet<InstructionRef> {
    let mut uses = HashSet::new();
    let mut visited = HashSet::new(); // to avoid cycles in CFG
    let mut worklist = VecDeque::new();

    // Start at (block, index) right after start_inst
    let (start_block_id, start_index) = cfg.locate_instruction(start_inst);
    worklist.push_back((start_block_id, start_index + 1));

    while let Some((block_id, inst_idx)) = worklist.pop_front() {
        if !visited.insert((block_id, inst_idx)) {
            continue; // Already visited this (block, idx)
        }

        let block = &cfg.blocks[&block_id];
        let instructions = &block.instructions;

        // Start scanning from inst_idx
        for i in inst_idx..instructions.len() as i32 {
            let inst = &instructions[i as usize];

            if inst.get_used_vars().contains(var) {
                uses.insert(InstructionRef(inst));
            }

            if instr_defs_var(inst, var) {
                break; // Variable redefined, stop exploring this path
            }
        }

        // If you scanned to end of block without redefinition:
        if instructions[inst_idx as usize..]
            .iter()
            .all(|inst| !instr_defs_var(inst, var))
        {
            // Add all successors to worklist
            for edge in cfg.successors(block_id) {
                worklist.push_back((edge, 0)); // start at instruction 0 in the successor
            }
        }
    }

    uses
}


fn defs_from_use(cfg: &CFG, start_inst: *const Instruction, var: &str) -> HashSet<InstructionRef> {
    let mut defs = HashSet::new();
    let mut visited = HashSet::new(); // (block_id, instruction_idx) pairs
    let mut worklist = VecDeque::new();

    // Start at (block, index) AT the use instruction
    let (start_block_id, start_index) = cfg.locate_instruction(start_inst);
    worklist.push_back((start_block_id, start_index));

    while let Some((block_id, inst_idx)) = worklist.pop_front() {
        if !visited.insert((block_id, inst_idx)) {
            continue; // Already visited
        }

        let block = &cfg.blocks[&block_id];
        let instructions = &block.instructions;

        // Search backward within the block
        for i in (0..=inst_idx).rev() {
            let inst = &instructions[i as usize];

            if instr_defs_var(inst, var) {
                defs.insert(InstructionRef(inst));
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

fn compute_webs(method_cfg: &CFG) -> BTreeMap<i32, Web> {
    let mut webs = BTreeMap::new();
    let mut visited_defs = HashSet::new();
    let mut visited_uses = HashSet::new();

    for (block_id, block) in &method_cfg.blocks {
        for inst in &block.instructions {
            if let Some(var) = inst.get_def_var() {
                let inst_ref = InstructionRef(inst);

                if visited_defs.contains(&inst_ref) {
                    continue;
                }

                let mut defs = HashSet::new();
                let mut uses = HashSet::new();
                let mut worklist = VecDeque::new();

                defs.insert(inst_ref);
                visited_defs.insert(inst_ref);
                worklist.push_back(inst_ref);

                // FP Algo to collect all associated defs and uses into one web
                while let Some(def) = worklist.pop_front() {
                    let def_inst = def.0;

                    let reachable_uses = uses_from_def(method_cfg, def_inst, &var);

                    for use_ref in reachable_uses {
                        if visited_uses.insert(use_ref) {
                            uses.insert(use_ref);

                            let reaching_defs = defs_from_use(method_cfg, unsafe { &*use_ref.0 }, &var);

                            for reaching_def in reaching_defs {
                                if visited_defs.insert(reaching_def) {
                                    defs.insert(reaching_def);
                                    worklist.push_back(reaching_def);
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


fn compute_interference() {
    todo!()

}


fn compute_spill_costs() {
    todo!()
}


fn apply_reg_assignments(web_assignments: HashMap<&String, BTreeMap<i32, Web>>, method_cfgs: &mut HashMap<String, CFG>,) {

}


/// Performs graph-coloring algorithm, assigning every web
/// either a register or a stack space.
pub fn reg_alloc(method_cfgs: &mut HashMap<String, CFG>, debug: bool) -> BTreeMap<i32, X86Operand> {

    let mut webs: HashMap<&String, BTreeMap<i32, Web>> = HashMap::new();

    for (method_name, cfg) in method_cfgs {
        let method_ranges: BTreeMap<i32, Web> = compute_webs(cfg);
        webs.insert(method_name, method_ranges);

    }



    let out = BTreeMap::new();
    out
}


