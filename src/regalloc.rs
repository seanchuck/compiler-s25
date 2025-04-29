use crate::{cfg::CFG, tac::Instruction, x86::X86Operand, tac::Operand, tac::InstructionRef};
use std::{cell::RefCell, collections::{BTreeMap, HashMap, HashSet, VecDeque}};
use crate::state::*;

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

#[derive(Debug, Clone)]
pub struct InterferenceGraph {
    pub edges: HashMap<String, HashSet<String>> // maps each variable to the variables its live range conflicts with
}

impl InterferenceGraph {
    pub fn new() -> Self {
        InterferenceGraph {
            edges: HashMap::new(),
        }
    }

    pub fn add_edge(&mut self, u: String, v: String) {
        self.edges.entry(u.clone()).or_insert_with(HashSet::new).insert(v.clone());
        self.edges.entry(v).or_insert_with(HashSet::new).insert(u);
    }

    pub fn neighbors(&self, u: &String) -> Option<&HashSet<String>> {
        self.edges.get(u)
    }
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



// add an edge between two variables if they are ever live at the same program point
fn compute_interference(method_cfg: &mut CFG, debug: bool) -> BTreeMap<i32, InterferenceGraph> {
    let (in_map, out_map) = compute_maps(method_cfg, debug);
    let mut interference_map: BTreeMap<i32, InterferenceGraph> = BTreeMap::new();

    for (block_id, block) in method_cfg.blocks.iter() {
        let in_set = todo!();  // get from in_map
        let out_set = todo!();  // get from out_map

        // process instructions backwards
        let mut live_out: HashSet<String> = todo!(); // initialize to OUT set of this basic block
        for instr in block.instructions.iter().rev() {
            // compute the set of variables that are live going into this instruction

            let mut live_in = live_out.clone();

            // remove variables defined by this instruction
            if let Some(defs) = get_defs(instr) {
                for def in defs {
                    live_in.remove(&def);
                }
            }

            // add variables used by this instruction
            if let Some(uses) = get_uses(instr) {
                for use_var in uses {
                    live_in.insert(use_var);
                }
            }

            

            // the IN set of this instruction becomes the OUT set of the previous one
            live_out = live_in;
        }
    }

    interference_map
}

fn add_new_use(use_set: &mut HashSet<String>, def_set: &HashSet<String>, operand: &Operand) {
    if let Some(name) = get_local_var_name(operand) {
        // don't include uses of variables that were defined within this basic block
        if !def_set.contains(&name) {
            use_set.insert(name);
        }
    }
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

    println!("webs are: {:#?}", webs);

    let out = BTreeMap::new();
    out
}


