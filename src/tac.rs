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
    GlobalVar(String, Type),
    GlobalArrElement(String, Box<Operand>, Type), // name and index
    String(i32, Type),                            // ID of string constant
    LocalVar(String, Type),
    LocalArrElement(String, Box<Operand>, Type),
    Const(i64, Type),
    Argument(i32, Type), // position of the argument and its type
}

impl Operand {
    pub fn get_type(& self) -> Type {
        match self {
            Operand::GlobalVar(_, typ) => typ.clone(),
            Operand::GlobalArrElement(_, _, typ) => typ.clone(),
            Operand::String(_, typ) => typ.clone(),
            Operand::LocalVar(_, typ) => typ.clone(),
            Operand::LocalArrElement(_, _, typ) => typ.clone(),
            Operand::Const(_, typ) => typ.clone(),
            Operand::Argument(_, typ) => typ.clone(),
        }
    }
}

impl fmt::Display for Operand {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Operand::Const(val, _) => write!(f, "{}", val),
            Operand::String(id, _) => write!(f, "str{}", id),
            Operand::GlobalVar(name, _) => write!(f, "{}", name),
            Operand::GlobalArrElement(name, idx, _) => write!(f, "{}[{}]", name, idx),
            Operand::LocalVar(name, _) => write!(f, "{}", name),
            Operand::LocalArrElement(name, idx, _) => write!(f, "{}[{}]", name, idx),
            Operand::Argument(pos, _) => write!(f, "arg{}", pos),
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Instruction {
    // ARITHMETIC BINARY OPERATIONS
    // t <- X + Y
    Add {
        typ: Type,
        left: Operand,
        right: Operand,
        dest: Operand,
    },
    Subtract {
        typ: Type,
        left: Operand,
        right: Operand,
        dest: Operand,
    },
    Multiply {
        typ: Type,
        left: Operand,
        right: Operand,
        dest: Operand,
    },
    Divide {
        typ: Type,
        left: Operand,
        right: Operand,
        dest: Operand,
    },
    Modulo {
        typ: Type,
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
        typ: Type, // is array type int or long
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
        args: Vec<Operand>,
        return_type: Type,
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
        typ: Type,
        value: Option<Operand>,
    },

    // SPECIAL
    Assign {
        typ: Type,
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
        typ: Type
    },
}

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub struct InstructionRef(pub *const Instruction);
