/**
Control flow graph (CFG) representation.

Consists of basic blocks and directed edges 
between those basic blocks.
**/

use std::collections::{BTreeMap, BTreeSet};
use crate::linear_ir::*;

#[derive(Debug, Clone,)]
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
