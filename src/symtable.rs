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

use crate::ast::{AssignOp, BinaryOp, Type, UnaryOp};
use crate::scope::{Scope, TableEntry};
use crate::token::{Literal, Span};
use std::cell::RefCell;

use std::collections::HashMap;
use std::rc::Rc;

/// Used to package the objects that need
/// to be passed throughout the semantic checks.
pub struct SemanticContext {
    pub filename: String,
    pub error_found: bool,
}

#[derive(Debug)]
pub struct SymProgram {
    pub global_scope: Rc<RefCell<Scope>>, // Holds local vars and methods
    pub methods: HashMap<String, Rc<SymMethod>>, // Methods are shared references
    pub span: Span,
}

/// Represents a method in the IR
#[derive(Debug)]
pub struct SymMethod {
    // pub is_import: bool,
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
#[derive(Debug, Clone)]
pub enum SymStatement {
    VarDecl {
        name: String,
        typ: Type,
        length: Option<Literal>,
        span: Span,
    },
    Assignment {
        target: SymExpr, //Now supports both `Identifier` and `ArrAccess`
        expr: SymExpr,
        span: Span,
        op: AssignOp,
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
        update: Box<SymStatement>,
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
    Error,
}

/// IR representation for expressions
#[derive(Debug, Clone)]
pub enum SymExpr {
    Literal {
        value: Literal,
        span: Span,
    },
    Identifier {
        entry: TableEntry,
        span: Span,
    },
    ArrAccess {
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
        span: Span,
    },
    UnaryExpr {
        op: UnaryOp,
        expr: Rc<SymExpr>,
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
    Error {
        span: Span,
    },
}
