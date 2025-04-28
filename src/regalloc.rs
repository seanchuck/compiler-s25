use crate::state::LiveVariables;
use crate::{cfg::CFG, tac::Instruction, x86::X86Operand};
use crate::tac::Operand;
use crate::state::*;
use std::{cell::RefCell, collections::{BTreeMap, HashMap, HashSet, VecDeque}};

// Def and reachable uses must be in same web
// All defs that reach a common use must be in same web
// All defs and uses to the variable within a web will be 
// done on the same register.
#[derive(Debug, Eq, PartialEq, Clone)]
struct Web {
    id: i32,
    variable: String,
    defs: Vec<Instruction>,
    uses: Vec<Instruction>,
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
        Operand::LocalVar(name, _) => Some(name.clone()),
        _ => None
    }
}

fn uses_from_def() -> HashSet<&Instruction>{
    todo!()
}

fn defs_from_use() -> HashSet<&Instruction>{
    todo!()
}


fn compute_live_ranges(method_cfg: &CFG) -> BTreeMap<i32, Web> { 
    todo!()
}


fn compute_interference() {
    todo!()

}


fn compute_spill_costs() {
    todo!()
}



/// Performs graph-coloring algorithm, assigning every web
/// either a register or a stack space.
fn reg_alloc(method_cfgs: &mut HashMap<String, CFG>, debug: bool) -> BTreeMap<i32, X86Operand> {
    todo!();

    let live_ranges: HashMap<&String, BTreeMap<i32, Web>> = HashMap::new();

    for (method_name, cfg) in method_cfgs {
        let method_ranges: BTreeMap<i32, Web> = compute_live_ranges(cfg);
        live_ranges.insert(method_name, method_ranges);

    }

    let out = BTreeMap::new();
    out
}


