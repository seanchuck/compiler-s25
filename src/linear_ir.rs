/*
Linearized IR. Includes instructions that are in the 
simple three-address-code (or very similar) format.

Similar to the AST.rs file, but includes 
instructions that are simplified, and TAC.
*/

use std::fmt;

use crate::ast::Type;
use crate::token::Literal;

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Operand {
    Id(String),
    Const(Literal),
}

impl fmt::Display for Operand {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Operand::Const(lit) => write!(f, "{}", lit),
            Operand::Id(name) => write!(f, "{}", name)
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Instruction {
    // BINARY OPERATIONS
    // t <- X + Y
    Add {
        left: Box<Operand>,
        right: Box<Operand>,
        dest: Box<Operand>,
    },
    Subtract {
        left: Box<Operand>,
        right: Box<Operand>,
        dest: Box<Operand>,
    },
    Multiply {
        left: Box<Operand>,
        right: Box<Operand>,
        dest: Box<Operand>,
    },
    Divide {
        left: Box<Operand>,
        right: Box<Operand>,
        dest: Box<Operand>,
    },
    Modulo {
        left: Box<Operand>,
        right: Box<Operand>,
        dest: Box<Operand>,
    },
    // And and Or not needed because of short-circuiting
    // And {
    //     left: Box<Operand>,
    //     right: Box<Operand>,
    //     dest: Box<Operand>,
    // },
    // Or {
    //     left: Box<Operand>,
    //     right: Box<Operand>,
    //     dest: Box<Operand>,
    // },

    // UNARY OPERATIONS
    // t <- !X
    // Neg not needed; replaced by subtraction from 0
    // Neg {
    //     expr: Box<Operand>,
    //     dest: Box<Operand>,
    // },
    Not {
        expr: Box<Operand>,
        dest: Box<Operand>,
    },
    Cast {
        expr: Box<Operand>,
        dest: Box<Operand>,
        target_type: Type,
    },
    Len {
        expr: Box<Operand>,
        dest: Box<Operand>,
    },

    // CONDITIONALS
    // t <- X > Y
    Greater {
        left: Box<Operand>,
        right: Box<Operand>,
        dest: Box<Operand>,
    },
    Less{
        left: Box<Operand>,
        right: Box<Operand>,
        dest: Box<Operand>,
    },
    LessEqual {
        left: Box<Operand>,
        right: Box<Operand>,
        dest: Box<Operand>,
    },
    GreaterEqual {
        left: Box<Operand>,
        right: Box<Operand>,
        dest: Box<Operand>,
    },
    Equal {
        left: Box<Operand>,
        right: Box<Operand>,
        dest: Box<Operand>,
    },
    NotEqual {
        left: Box<Operand>,
        right: Box<Operand>,
        dest: Box<Operand>,
    },

    // CONTROL FLOW 
    MethodCall {
        name: String,
        args: Vec<Operand>, // Does not completely follow TAC as 
                            // method call can have many arguments
        dest: Option<Box<Operand>>,
    },
    UJmp { // unconditonal jump
        id: i32, // ID of basic block to jump to
    },
    Branch { // conditional jump
        condition: Box<Operand>,
        true_target: i32, // ID of basic block to jump to if condition is true
        false_target: i32 // ID of basic block to jump to if condition is false
    },
    Ret {
        value: Option<Box<Operand>>,
    },

    // SPECIAL
    ArrAccess {
        array: Box<Operand>,
        index: Box<Operand>,
        dest: Box<Operand>,
    },
    Assign {
        src: Box<Operand>,
        dest: Box<Operand>,
    },
    // Nop,
}


