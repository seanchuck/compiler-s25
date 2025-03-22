/*
Linearized IR. Includes instructions that are in the 
simple three-address-code (or very similar) format.

Similar to the AST.rs file, but includes 
instructions that are simplified, and TAC.
*/

use crate::ast::Type;
use crate::token::Literal;

#[allow(dead_code)]
#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Operand {
    Id(String),
    Const(Literal),
}

#[allow(dead_code)]
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
    And { // Make sure to short-circuit!
        left: Box<Operand>,
        right: Box<Operand>,
        dest: Box<Operand>,
    },
    Or { // Make sure to short-circuit!
        left: Box<Operand>,
        right: Box<Operand>,
        dest: Box<Operand>,
    },

    // UNARY OPERATIONS
    // t <- X + Y
    Neg {
        expr: Box<Operand>,
        dest: Box<Operand>,
    },
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
        label: String,
    },
    CJmp { // conditional jump
        condition: Box<Operand>,
        label: String,
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
    Label {
        name: String,
    },
    Nop,
}


