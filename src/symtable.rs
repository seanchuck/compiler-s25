/*
Data structure for creating symbol tables.

Symbol table AST is a tree similar to the result of the 
parse, but augmented with scope to have 
tronger semantic understanding of the program.

The following non-terminals create new scopes:
    - program
    - block
    - method
*/
use crate::ast::{BinaryOp,Type, UnaryOp};
use crate::scope::{Scope, TableEntry};
use crate::token::Literal; use std::cell::RefCell;

use std::collections::HashMap;
use std::rc::Rc;

/// The root intermediate representation (IR) enum
#[allow(dead_code)]
#[derive(Debug)]
pub enum SymNode {
    Program(SymProgram),
    Method(SymMethod),
    Block(SymBlock),
}

/// Represents the IR structure of a program
#[allow(dead_code)]
#[derive(Debug)]
pub struct SymProgram {
    pub global_scope: Rc<RefCell<Scope>>,  // Holds local vars and methods
    pub methods: HashMap<String, Rc<SymMethod>>,  // Methods are shared references
}

/// Represents a method in the IR
#[allow(dead_code)]
#[derive(Debug)]
pub struct SymMethod {
    pub name: String,
    pub return_type: Type,
    pub params: Vec<(Type, String)>,
    pub scope: Rc<RefCell<Scope>>,  // Stores local variables
    pub body: SymBlock, // Statements are reference-counted
}

/// Represents a block of statements in the IR
#[derive(Debug)]
pub struct SymBlock {
    pub scope: Rc<RefCell<Scope>>,  // Holds variables declared inside the block
    pub statements: Vec<Rc<IRStatement>>,  // Statements in this block
}

/// IR representation for statements
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
        then_block: Rc<SymBlock>,
        else_block: Option<Rc<SymBlock>>,
    },
    While {
        condition: IRExpr,
        block: Rc<SymBlock>,
    },
    For {
        var: String,
        init: IRExpr,
        condition: IRExpr,
        update: IRExpr,
        block: Rc<SymBlock>,
    },
    Return {
        expr: Option<IRExpr>,
    },
    Break,
    Continue,
}

/// IR representation for expressions
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