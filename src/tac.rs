/*
Linearized IR. Includes instructions that are mostly in
the simple three-address-code (TAC) format.

Similar to the AST.rs file, but includes
instructions that are simplified, and TAC.
*/

use crate::ast::Type;
use crate::token::Literal;
use std::fmt;

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Operand {
    GlobalVar(String),
    GlobalArrElement(String, Box<Operand>), // name and index
    String(usize), // ID of string constant
    LocalVar(String),
    LocalArrElement(String, Box<Operand>),
    Const(Literal),
    Argument(usize) // position of the argument
}

impl fmt::Display for Operand {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Operand::Const(lit) => write!(f, "{}", lit),
            Operand::String(id) => write!(f, "str{}", id),
            Operand::GlobalVar(name) => write!(f, "{}", name),
            Operand::GlobalArrElement(name, idx) => write!(f, "{}[{}]", name, idx),
            Operand::LocalVar(name) => write!(f, "{}", name),
            Operand::LocalArrElement(name, idx) => write!(f, "{}[{}]", name, idx),
            Operand::Argument(pos) => write!(f, "arg{}", pos)
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Instruction {
    // BINARY OPERATIONS
    // t <- X + Y
    Add {
        left: Operand,
        right: Operand,
        dest: Operand,
    },
    Subtract {
        left: Operand,
        right: Operand,
        dest: Operand,
    },
    Multiply {
        left: Operand,
        right: Operand,
        dest: Operand,
    },
    Divide {
        left: Operand,
        right: Operand,
        dest: Operand,
    },
    Modulo {
        left: Operand,
        right: Operand,
        dest: Operand,
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
        expr: Operand,
        dest: Operand,
    },
    Cast {
        expr: Operand,
        dest: Operand,
        target_type: Type,
    },
    Len {
        expr: Operand,
        dest: Operand,
    },

    // CONDITIONALS
    // t <- X > Y
    Greater {
        left: Operand,
        right: Operand,
        dest: Operand,
    },
    Less {
        left: Operand,
        right: Operand,
        dest: Operand,
    },
    LessEqual {
        left: Operand,
        right: Operand,
        dest: Operand,
    },
    GreaterEqual {
        left: Operand,
        right: Operand,
        dest: Operand,
    },
    Equal {
        left: Operand,
        right: Operand,
        dest: Operand,
    },
    NotEqual {
        left: Operand,
        right: Operand,
        dest: Operand,
    },

    // CONTROL FLOW
    MethodCall {
        name: String,
        args: Vec<Operand>, // Does not completely follow TAC as
        // method call can have many arguments
        dest: Option<Operand>,
    },
    UJmp {
        // unconditonal jump
        id: i32, // ID of basic block to jump to
    },
    Branch {
        // conditional jump
        condition: Operand,
        true_target: i32,  // ID of basic block to jump to if condition is true
        false_target: i32, // ID of basic block to jump to if condition is false
    },
    Ret {
        value: Option<Operand>,
    },

    // SPECIAL
    // ArrAccess {
    //     array: Box<Operand>,
    //     index: Box<Operand>,
    //     dest: Box<Operand>,
    // },
    Assign {
        src: Operand,
        dest: Operand,
    },
    // Nop,

    // MEMORY
    LoadString {
        src: Operand,
        dest: Operand,
    },
}
