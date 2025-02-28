/*
Create IR by augmenting AST with scope to
have stronger semantic understanding of the program.

program, block, and method must contain scope
*/
use crate::ast::{BinaryOp,Type, UnaryOp};
use crate::scope::{Scope, TableEntry};
use crate::token::Literal; use std::cell::RefCell;

use std::collections::HashMap;
use std::rc::Rc;

/// The root intermediate representation (IR) enum
#[allow(dead_code)]
#[derive(Debug)]
pub enum IRNode {
    Program(IRProgram),
    Method(IRMethod),
    Block(IRBlock),
}

/// Represents the IR structure of a program
#[allow(dead_code)]
#[derive(Debug)]
pub struct IRProgram {
    pub global_scope: Rc<RefCell<Scope>>,  // Holds local vars and methods
    pub methods: HashMap<String, Rc<IRMethod>>,  // Methods are shared references
}

/// Represents a method in the IR
#[allow(dead_code)]
#[derive(Debug)]
pub struct IRMethod {
    pub name: String,
    pub return_type: Type,
    pub params: Vec<(Type, String)>,
    pub scope: Rc<RefCell<Scope>>,  // Stores local variables
    pub body: IRBlock, // Statements are reference-counted
}

/// Represents a block of statements in the IR
#[allow(dead_code)]
#[derive(Debug)]
pub struct IRBlock {
    pub scope: Rc<RefCell<Scope>>,  // Holds variables declared inside the block
    pub statements: Vec<Rc<IRStatement>>,  // Statements in this block
}

/// IR representation for statements
#[allow(dead_code)]
#[derive(Debug)]
pub enum IRStatement {
    VarDecl {
        name: String,
        typ: Type,
        is_array: bool,
    },
    Assignment {
        target: String,
        expr: IRExpr,
    },
    MethodCall {
        method_name: String,
        args: Vec<IRExpr>,
    },
    If {
        condition: IRExpr,
        then_block: Rc<IRBlock>,
        else_block: Option<Rc<IRBlock>>,
    },
    While {
        condition: IRExpr,
        block: Rc<IRBlock>,
    },
    For {
        var: String,
        init: IRExpr,
        condition: IRExpr,
        update: IRExpr,
        block: Rc<IRBlock>,
    },
    Return {
        expr: Option<IRExpr>,
    },
    Break,
    Continue,
}

/// IR representation for expressions
#[allow(dead_code)]
#[derive(Debug)]
pub enum IRExpr {
    Literal(Literal),
    Identifier(TableEntry),
    ArrayAccess {
        id: String,
        index: Rc<IRExpr>,
    },
    MethodCall {
        method_name: String,
        args: Vec<Rc<IRExpr>>,
    },
    BinaryExpr {
        op: BinaryOp,
        left: Rc<IRExpr>,
        right: Rc<IRExpr>,
        typ: Type,
    },
    UnaryExpr {
        op: UnaryOp,
        expr: Rc<IRExpr>,
        typ: Type,
    },
    Cast {
        target_type: Type,
        expr: Rc<IRExpr>,
    },
    Len {
        id: String,
    },
}