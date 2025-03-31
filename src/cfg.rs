use crate::tac::*;
/**
Control flow graph (CFG) representation.

Consists of basic blocks and directed edges
between those basic blocks.
**/
use std::collections::BTreeMap;

pub const ELEMENT_SIZE: i64 = 8; // for now, allocate 8 bytes for everything no matter the type

#[derive(Debug, Clone)]
pub struct CFG {
    // BTreeMap is hash map that allows constant iteration order
    // entry block has ID 0
    pub blocks: BTreeMap<i32, BasicBlock>, // Maps the ID of basic block to its representation
    // no need to store edges because basic blocks end with a jump/branch instruction
    pub stack_size: i64, // total space to allocate on the stack for this method
    pub locals: BTreeMap<String, Local>,
}

#[derive(Debug, Clone)]
pub struct BasicBlock {
    // Basic block is just a vector of instructions
    instructions: Vec<Instruction>,
    id: i32, // TODO: can add meaningful labels to each BB instead of referring to them by ID
}

pub struct Global {
    pub name: String,
    pub length: Option<i32>, // if array
}

#[derive(Debug, Clone)]
pub struct Local {
    stack_offset: i64,
    length: Option<i64>, // if array
}

impl CFG {
    pub fn new() -> CFG {
        CFG {
            blocks: BTreeMap::new(),
            stack_size: 0,
            locals: BTreeMap::new(),
        }
    }

    /// Get all basic blocks
    pub fn get_blocks(&self) -> &BTreeMap<i32, BasicBlock> {
        &self.blocks
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
    pub fn add_temp_var(&mut self, temp: String, length: Option<i64>) {
        let size: i64;
        if length.is_some() {
            // add one to store the array length
            size = (length.unwrap() + 1) * ELEMENT_SIZE;
        } else {
            size = ELEMENT_SIZE;
        }

        self.stack_size += size;
        self.locals.insert(
            temp,
            Local {
                stack_offset: -self.stack_size,
                length: length,
            },
        );
    }

    /// Get the stack offset of a temp var
    pub fn get_stack_offset(&self, temp: &String) -> i64 {
        self.locals.get(temp).unwrap().stack_offset
    }
}

impl BasicBlock {
    pub fn new(id: i32) -> BasicBlock {
        BasicBlock {
            instructions: Vec::new(),
            id,
        }
    }

    pub fn get_id(&self) -> i32 {
        self.id
    }

    pub fn get_instructions(&self) -> &Vec<Instruction> {
        &self.instructions
    }

    fn add_instruction(&mut self, instruction: Instruction) {
        self.instructions.push(instruction);
    }
}
