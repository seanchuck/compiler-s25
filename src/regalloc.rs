use crate::{cfg::CFG, tac::Instruction, x86::X86Operand};
use std::{cell::RefCell, collections::{BTreeMap, HashMap, HashSet}};

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



// Initialize a counter for naming webs
thread_local! {
    static WEB_COUNTER: RefCell<usize> = RefCell::new(0);
}


///
fn compute_live_ranges(method_cfg: &CFG) -> BTreeMap<i32, Web>{ 
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

    let live_ranges = HashMap::new();

    for (method_name, cfg) in method_cfgs {
        let method_ranges = compute_live_ranges(cfg);
        live_ranges.insert(method_name, method_ranges);

    }

    let out = BTreeMap::new();
    out
}


