use crate::tac::Instruction;
use std::{
    collections::{HashMap, HashSet, BTreeMap},
    hash::Hash,
};

// Def and reachable uses must be in same web
// All defs that reach a common use must be in same web
// All defs and uses to the variable within a web will be
// done on the same register.
#[derive(Debug, Eq, PartialEq, Clone, Hash, PartialOrd, Ord)]
pub struct Web {
    pub id: i32,
    pub variable: String,
    pub defs: Vec<InstructionIndex>, // defs must all have the same variable name
    pub uses: Vec<InstructionIndex>,
}

#[derive(Debug, Clone)]
pub struct InterferenceGraph {
    pub nodes: HashSet<i32>, // Web IDs
    pub edges: HashMap<i32, HashSet<i32>>, // Undirected edges between interfering web IDs
}

impl InterferenceGraph {
    pub fn new() -> Self {
        InterferenceGraph {
            nodes: HashSet::new(),
            edges: HashMap::new(),
        }
    }

    pub fn add_edge(&mut self, u: i32, v: i32) {
        if u == v {
            return; // prevent self-loops
        }
        self.nodes.insert(u);
        self.nodes.insert(v);
        self.edges.entry(u).or_insert_with(HashSet::new).insert(v);
        self.edges.entry(v).or_insert_with(HashSet::new).insert(u);
    }

    pub fn neighbors(&self, u: &i32) -> Option<&HashSet<i32>> {
        self.edges.get(u)
    }
}


#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug, PartialOrd, Ord)]
pub struct InstructionIndex {
    pub block_id: i32,
    pub instr_index: i32,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct InstructionMap(pub BTreeMap<InstructionIndex, Instruction>);

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct DefUse {
    pub defs: HashSet<String>, // all variables that are defined in this basic block
    pub uses: HashSet<String>, // all variables that are used in this basic block
}
