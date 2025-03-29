/**
Control flow graph (CFG) representation.

Consists of basic blocks and directed edges
between those basic blocks.
**/

use std::collections::{BTreeMap, HashMap};
use crate::tac::*;


// #################################################
// CONSTANTS
// #################################################

pub const UNREACHABLE_BLOCK: i32 = -1; // id for an unreachable block
const ELEMENT_SIZE: i32 = 8; // for now, allocate 8 bytes for everything no matter the type


// #################################################
// STRUCT DEFINITIONS
// #################################################

#[derive(Debug, Clone)]
pub struct CFG {
    // BTreeMap is hash map that allows constant iteration order
    // entry block has ID 0
    pub blocks: BTreeMap<i32, BasicBlock>, // Maps the ID of basic block to its representation
                                       // no need to store edges because basic blocks end with a jump/branch instruction
    pub stack_size: i32, // total space to allocate on the stack for this method
    pub locals: BTreeMap<String, Local>
}

#[derive(Debug, Clone)]
pub struct BasicBlock {
    // Basic block is just a vector of instructions
    instructions: Vec<Instruction>,
    id: i32, // TODO: can add meaningful labels to each BB instead of referring to them by ID
}

#[derive(Debug, Clone)]
pub struct Global {
    pub name: String, 
    pub length: Option<i32> // if array
}

#[derive(Debug, Clone)]
pub struct Local {
    pub stack_offset: i32,
    pub length: Option<i32> // if array
}

#[derive(Debug, Clone)]
pub struct Loop {
    pub break_to: i32,    // ID of basic block following the loop
    pub continue_to: i32, // ID of loop's header basic block
}

#[derive(Debug, Clone)]
pub struct CFGScope {
    pub parent: Option<Box<CFGScope>>,
    pub local_to_temp: HashMap<String, String> // maps local variable to temp variable
}


// #################################################
// IMPLEMENTATIONS
// #################################################

impl CFGScope {
    /// Returns this global variable, or the temp variable associated to this local variable
    pub fn lookup_var(&self, var: String) -> Operand {
        if let Some(temp) = self.local_to_temp.get(&var) {
            Operand::LocalVar(temp.to_string())
        } else if let Some(parent) = &self.parent {
            parent.lookup_var(var)
        } else {
            // assume it is in the global CFGScope
            Operand::GlobalVar(var)
        }
    }

    /// Returns this global array element, or the temp array element associated to this local array element
    pub fn lookup_arr(&self, arr: String, idx: Operand) -> Operand {
        if let Some(temp) = self.local_to_temp.get(&arr) {
            Operand::LocalArrElement(temp.to_string(), Box::new(idx))
        } else if let Some(parent) = &self.parent {
            parent.lookup_arr(arr, idx)
        } else {
            // assume it is in the global CFGScope
            Operand::GlobalArrElement(arr, Box::new(idx))
        }
    }

    /// Add a new local variable to the CFGScope
    pub fn add_local(&mut self, local: String, temp: String) {
        self.local_to_temp.insert(local, temp);
    }
}


impl CFG {
    pub fn new() -> CFG {
        CFG {
            blocks: BTreeMap::new(),
            stack_size: 0,
            locals: BTreeMap::new()
        }
    }

    /// Get all basic blocks
    pub fn get_blocks(self) -> BTreeMap<i32, BasicBlock> {
        self.blocks
    }

    /// Get basic block with ID
    fn get_block_with_id(&mut self, id: i32) -> &mut BasicBlock {
        let block = self.blocks.get_mut(&id);
        if block.is_none() {
            panic!("block with id {id} does not exist");
        }
        block.unwrap()
    }

    /// Add a basic block with a given ID
    pub fn add_block(&mut self, block: &BasicBlock) {
        self.blocks.insert(block.get_id(), block.clone());
    }

    /// Add instruction to block with ID
    pub fn add_instruction_to_block(&mut self, id: i32, instruction: Instruction) {
        if id == -1 {
            return; // do nothing, because this is an unreachable block
        } else {
            let block = self.get_block_with_id(id);
            block.add_instruction(instruction);
        }
    }

    /// Allocate space on the stack for a new temp var
    pub fn add_temp_var(&mut self, temp: String, length: Option<i32>) {
        let size: i32;
        if length.is_some() {
            // add one to store the array length
            size = (length.unwrap() + 1) * ELEMENT_SIZE;
        } else {
            size = ELEMENT_SIZE;
        }

        self.stack_size += size;
        self.locals.insert(temp, Local { stack_offset: -self.stack_size, length: length });
    }
}


impl BasicBlock {
    pub fn new(id: i32) -> BasicBlock {
        BasicBlock {
            instructions: Vec::new(),
            id
        }
    }

    pub fn get_id(&self) -> i32 {
        self.id
    }

    pub fn get_instructions(self) -> Vec<Instruction> {
        self.instructions
    }

    fn add_instruction(&mut self, instruction: Instruction) {
        self.instructions.push(instruction);
    }
}
