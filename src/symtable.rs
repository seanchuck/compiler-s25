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


#[derive(Debug)]
pub struct Program {
    pub global_scope: Rc<RefCell<Scope>>,  // holds local vars and methods
    pub methods: HashMap<String, IRMethod>,
}


// Represents the intermediate representation of a function
#[derive(Debug)]
pub struct IRMethod {
    name: String,
    return_type: Type,
    params: Vec<(Type, String)>,
    
    // stores local variables
    scope: Scope,
    statements: Vec<IRStatement>,
}

#[derive(Debug)]
pub struct IRBlock {
    scope: Scope,
    statements: Vec<IRStatement>,
}

// IR representation for statements with semantic checks
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
        then_block: IRBlock,
        else_block: Option<IRBlock>,
    },
    While {
        condition: IRExpr,
        block: IRBlock,
    },
    For {
        var: String,
        init: IRExpr,
        condition: IRExpr,
        update: IRExpr,
        block: IRBlock,
    },
    Return {
        expr: Option<IRExpr>,
    },
    Break,
    Continue,
}

// IR representation for expressions with type information
#[derive(Debug)]
pub enum IRExpr {
    Literal(Literal),
    Identifier(TableEntry),
    ArrayAccess {
        id: String,
        index: Box<IRExpr>,
    },
    MethodCall {
        method_name: String,
        args: Vec<IRExpr>,
    },
    BinaryExpr {
        op: BinaryOp,
        left: Box<IRExpr>,
        right: Box<IRExpr>,
        typ: Type,
    },
    UnaryExpr {
        op: UnaryOp,
        expr: Box<IRExpr>,
        typ: Type,
    },
    Cast {
        target_type: Type,
        expr: Box<IRExpr>,
    },
    Len {
        id: String,
    },
}
