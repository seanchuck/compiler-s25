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

use crate::ast::{BinaryOp, Type, UnaryOp};
use crate::scope::{Scope, TableEntry};
use crate::token::{Literal, Span};
use std::cell::RefCell;

use std::collections::HashMap;
use std::rc::Rc;

/// The root for the symbol table AST
#[allow(dead_code)]
#[derive(Debug)]
pub enum SymNode {
    Program(SymProgram),
    Method(SymMethod),
    Block(SymBlock),
}

#[allow(dead_code)]
#[derive(Debug)]
pub struct SymProgram {
    pub global_scope: Rc<RefCell<Scope>>, // Holds local vars and methods
    pub methods: HashMap<String, Rc<SymMethod>>, // Methods are shared references
    pub span: Span,
}

/// Represents a method in the IR
#[allow(dead_code)]
#[derive(Debug)]
pub struct SymMethod {
    pub name: String,
    pub return_type: Type,
    pub params: Vec<(Type, String, Span)>,
    pub scope: Rc<RefCell<Scope>>, // Stores local variables
    pub body: SymBlock,            // Statements are reference-counted
    pub span: Span,
}

/// Represents a block of statements in the IR
#[derive(Debug)]
pub struct SymBlock {
    pub scope: Rc<RefCell<Scope>>, // Holds variables declared inside the block
    pub statements: Vec<Rc<SymStatement>>, // Statements in this block
    pub span: Span,
}

/// IR representation for statements
#[allow(dead_code)]
#[derive(Debug)]
pub enum SymStatement {
    VarDecl {
        name: String,
        typ: Type,
        is_array: bool,
        span: Span,
    },
    Assignment {
        target: String,
        expr: SymExpr,
        span: Span,
    },
    MethodCall {
        method_name: String,
        args: Vec<SymExpr>,
        span: Span,
    },
    If {
        condition: SymExpr,
        then_block: Rc<SymBlock>,
        else_block: Option<Rc<SymBlock>>,
        span: Span,
    },
    While {
        condition: SymExpr,
        block: Rc<SymBlock>,
        span: Span,
    },
    For {
        var: String,
        init: SymExpr,
        condition: SymExpr,
        update: SymExpr,
        block: Rc<SymBlock>,
        span: Span,
    },
    Return {
        expr: Option<SymExpr>,
        span: Span,
    },
    Break {
        span: Span,
    },
    Continue {
        span: Span,
    },
}

/// IR representation for expressions
#[allow(dead_code)]
#[derive(Debug)]
pub enum SymExpr {
    Literal {
        value: Literal,
        span: Span,
    },
    Identifier {
        entry: TableEntry,
        span: Span,
    },
    ArrayAccess {
        id: String,
        index: Rc<SymExpr>,
        span: Span,
    },
    MethodCall {
        method_name: String,
        args: Vec<Rc<SymExpr>>,
        span: Span,
    },
    BinaryExpr {
        op: BinaryOp,
        left: Rc<SymExpr>,
        right: Rc<SymExpr>,
        typ: Type,
        span: Span,
    },
    UnaryExpr {
        op: UnaryOp,
        expr: Rc<SymExpr>,
        typ: Type,
        span: Span,
    },
    Cast {
        target_type: Type,
        expr: Rc<SymExpr>,
        span: Span,
    },
    Len {
        id: String,
        span: Span,
    },
}
