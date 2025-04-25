/*
Linearized IR. Includes instructions that are mostly in
the simple three-address-code (TAC) format.

Similar to the AST.rs file, but includes
instructions that are simplified, and TAC.
*/

use crate::ast::Type;
use std::fmt;

#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub enum Operand {
    GlobalVar(String),
    GlobalArrElement(String, Box<Operand>), // name and index
    String(i32),                            // ID of string constant
    LocalVar(String),
    LocalArrElement(String, Box<Operand>),
    Const(i64),
    Argument(i32), // position of the argument
}
    
    impl fmt::Display for Operand {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            match self {
                Operand::Const(val) => write!(f, "{}", val),
                Operand::String(id) => write!(f, "str{}", id),
                Operand::GlobalVar(name) => write!(f, "{}", name),
                Operand::LocalVar(name) => write!(f, "{}", name),
                Operand::GlobalArrElement(name, idx) => write!(f, "{}[{}]", name, idx),
                Operand::LocalArrElement(name, idx) => write!(f, "{}[{}]", name, idx),
                Operand::Argument(pos) => write!(f, "arg{}", pos),
            }
        }
    }
    
impl Operand {
    pub fn get_name(&self) -> Option<&str> {
        match self {
            Operand::GlobalVar(name) => Some(name),
            Operand::GlobalArrElement(name, _) => Some(name),
            Operand::LocalVar(name) => Some(name),
            Operand::LocalArrElement(name, _) => Some(name),
            _ => None,
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
    // UNARY OPERATIONS
    // t <- !X

    // Neg not needed; replaced by subtraction from 0
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
        name: String, // Name of the method containing the Bblock
        id: i32,      // ID of basic block to jump to
    },
    CJmp {
        // conditional jump
        name: String, // Name of the method containing the Bblock
        condition: Operand,
        id: i32, // ID of basic block to jump to if condition is true
    },
    Ret {
        value: Option<Operand>,
    },

    // SPECIAL
    Assign {
        src: Operand,
        dest: Operand,
    },

    // MEMORY
    LoadString {
        src: Operand,
        dest: Operand,
    },

    // Runtime Exit
    Exit {
        exit_code: i64,
    },

    LoadConst {
        src: i64,
        dest: Operand,
    },
}
