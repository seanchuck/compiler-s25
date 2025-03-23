/**
Control flow graph (CFG) representation.

Consists of basic blocks and directed edges 
between those basic blocks.
**/

use std::collections::{BTreeMap, BTreeSet};
use crate::linear_ir::*;

#[derive(Debug, Clone)]
pub struct CFG {
    // BTreeMap is hash map that allows constant iteration order
    blocks: BTreeMap<i32, BasicBlock>, // Maps the ID of basic block to its representation
    edges: DirectedGraph, // Maps basic block ID to its children in the CFG
}

#[derive(Debug, Clone)]
pub struct DirectedGraph {
    // Maps basic block to vector of children
    children: BTreeMap<i32, BTreeSet<i32>>
}


#[derive(Debug, Clone)]
pub struct BasicBlock {
    // Basic block is just a vector of instructions
    instructions: Vec<Instruction>,
    id: i32
}

impl CFG {
    pub fn new() -> CFG {
        CFG {
            blocks: BTreeMap::new(),
            edges: DirectedGraph {
                children: BTreeMap::new(),
            },
        }
    }

    /// Add a basic block with a given ID
    pub fn add_block(&mut self, block: &BasicBlock) {
        self.blocks.insert(block.get_id(), block.clone()); // TODO: clone causes weird interactions
        self.edges.children.entry(block.get_id()).or_insert_with(BTreeSet::new);
    }

    /// Add a child block to a parent block
    pub fn add_edge(&mut self, parent_id: i32, child_id: i32) {
        self.edges.children
            .entry(parent_id)
            .or_insert_with(BTreeSet::new)
            .insert(child_id);
    }

    /// Get children of a block
    pub fn get_children(&self, block_id: i32) -> Option<&BTreeSet<i32>> {
        self.edges.children.get(&block_id)
    }

}

impl BasicBlock {
    pub fn new(id: i32) -> BasicBlock {
        BasicBlock { instructions: Vec::new(), id}
    }

    pub fn add_instruction(&mut self, instruction: Instruction) {
        self.instructions.push(instruction);
    }

    pub fn get_id(&self) -> i32 {
        self.id
    }
}




// #[derive(Debug, Clone)]
// pub struct DestructedNode {
//     begin: Box<BasicBlock>,
//     end: Box<BasicBlock>
// }


// #[derive(Debug, Clone, Copy)]
// pub enum TAC {
//     Assignment,
//     Operation,
//     Conditional,
//     Nop,
// }


// impl DestructedNode {
//     pub fn new() -> Self {
//         let mut start = BasicBlock::new();
//         let end = BasicBlock::new();

//         start.true_next = Some(Box::new(end));


//         DestructedNode {
//             begin: Box::new(start),
//             end: Box::new(end.clone()),
//         }
//     }
// }


// impl BasicBlock {
//     pub fn new() -> Self {
//         BasicBlock {
//             instructions: vec![],
//             true_next: None,
//             false_next: None,
//         }
//     }

//     pub fn get_condition(&self) -> Option<TAC> {
//         self.instructions.last().and_then(|tac| match tac {
//             TAC::Conditional => Some(TAC::Conditional),
//             _ => None,
//         })
//     }
    
// }
