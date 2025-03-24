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
    // entry block has ID 0
    blocks: BTreeMap<i32, BasicBlock>, // Maps the ID of basic block to its representation
    // no need to store edges because basic blocks end with a jump/branch instruction
}


#[derive(Debug, Clone)]
pub struct BasicBlock {
    // Basic block is just a vector of instructions
    instructions: Vec<Instruction>,
    id: i32,
    label: Option<String>
}

impl CFG {
    pub fn new() -> CFG {
        CFG {
            blocks: BTreeMap::new()
        }
    }

    /// Add a basic block with a given ID
    pub fn add_block(&mut self, block: &BasicBlock) {
        self.blocks.insert(block.get_id(), block.clone());
    }
}

impl BasicBlock {
    pub fn new(id: i32) -> BasicBlock {
        BasicBlock { instructions: Vec::new(), id, label: None }
    }

    pub fn add_instruction(&mut self, instruction: Instruction) {
        self.instructions.push(instruction);
    }

    pub fn get_id(&self) -> i32 {
        self.id
    }
}