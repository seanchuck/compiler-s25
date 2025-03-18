use std::rc::Rc;


#[derive(Debug, Clone)]
pub struct DestructedNode {
    begin: Box<BasicBlock>,
    end: Box<BasicBlock>
}


#[derive(Debug, Clone)]
pub struct BasicBlock {
    instructions: Vec<TAC>,
    true_next: Option<Box<BasicBlock>>,
    false_next: Option<Box<BasicBlock>>
}

#[derive(Debug, Clone, Copy)]
pub enum TAC {
    Assignment,
    Operation,
    Conditional,
    Nop,
}


impl DestructedNode {
    pub fn new() -> Self {
        let mut start = BasicBlock::new();
        let end = BasicBlock::new();

        start.true_next = Some(Box::new(end));


        DestructedNode {
            begin: Box::new(start),
            end: Box::new(end.clone()),
        }
    }
}


impl BasicBlock {
    pub fn new() -> Self {
        BasicBlock {
            instructions: vec![],
            true_next: None,
            false_next: None,
        }
    }

    pub fn get_condition(&self) -> Option<TAC> {
        self.instructions.last().and_then(|tac| match tac {
            TAC::Conditional => Some(TAC::Conditional),
            _ => None,
        })
    }
    
}
