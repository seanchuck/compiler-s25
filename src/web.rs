use std::{collections::{HashMap, HashSet}, hash::Hash};
use crate::tac::Instruction;



#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub struct InstructionIndex {
    pub block_id: i32,
    pub instr_index: i32,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct InstructionMap(pub HashMap<InstructionIndex, Instruction>);


// Def and reachable uses must be in same web
// All defs that reach a common use must be in same web
// All defs and uses to the variable within a web will be 
// done on the same register.
#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Web {
    pub id: i32,
    pub variable: String,
    pub defs: Vec<InstructionIndex>,
    pub uses: Vec<InstructionIndex>,
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

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct DefUse {
    pub defs: HashSet<String>, // all variables that are defined in this basic block
    pub uses: HashSet<String> // all variables that are used in this basic block
}
