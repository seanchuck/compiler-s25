/**
 Control flow graph (CFG) representation.
 
 Consists of basic blocks and directed edges
 between those basic blocks.
 **/
 use std::collections::BTreeMap;
 use crate::tac::*;


#[derive(Debug, Clone)]
pub struct CFG {
    // BTreeMap is hash map that allows constant iteration order
    // entry block has ID 0
    pub blocks: BTreeMap<i32, BasicBlock>, // Maps the ID of basic block to its representation
                                       // no need to store edges because basic blocks end with a jump/branch instruction
}

#[derive(Debug, Clone)]
pub struct BasicBlock {
    // Basic block is just a vector of instructions
    instructions: Vec<Instruction>,
    id: i32,
    _label: Option<String>, // TODO: can add meaningful labels to each BB instead of referring to them by ID
}

#[derive(Debug, Clone)]
pub struct Loop {
    pub break_to: i32,    // ID of basic block following the loop
    pub continue_to: i32, // ID of loop's header basic block
}

impl CFG {
    pub fn new() -> CFG {
        CFG {
            blocks: BTreeMap::new(),
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
}

impl BasicBlock {
    pub fn new(id: i32) -> BasicBlock {
        BasicBlock {
            instructions: Vec::new(),
            id,
            _label: None,
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
