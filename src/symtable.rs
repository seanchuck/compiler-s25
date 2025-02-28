// ir.rs
//
// Intermediate Representation (IR) module that uses a Scope for symbol management
// and provides functions to convert the AST into a lower-level IR.

use std::collections::HashMap;
use crate::ast::{AST, Statement, Expr, Type, BinaryOp, UnaryOp};
use crate::token::Literal; // if needed for literal values

/// A simple datatype for IR. This mirrors our AST’s types.
#[derive(Debug, Clone)]
pub enum Datatype {
    Int,
    Long,
    Bool,
    Void,
}

/// A scope contains a symbol table (mapping variable names to their datatype)
/// and an optional parent scope.
#[derive(Debug)]
pub struct Scope {
    symbols: HashMap<String, Datatype>,
    parent: Option<Box<Scope>>,
}

impl Scope {
    /// Create a new (global) scope.
    pub fn new() -> Self {
        Scope {
            symbols: HashMap::new(),
            parent: None,
        }
    }

    /// Create a new child scope from an existing parent.
    pub fn new_child(parent: Scope) -> Self {
        Scope {
            symbols: HashMap::new(),
            parent: Some(Box::new(parent)),
        }
    }

    /// Insert a new symbol into the scope.
    pub fn insert(&mut self, name: String, datatype: Datatype) {
        self.symbols.insert(name, datatype);
    }

    /// Lookup a symbol in this scope or any parent scope.
    pub fn lookup(&self, name: &str) -> Option<&Datatype> {
        if let Some(datatype) = self.symbols.get(name) {
            Some(datatype)
        } else if let Some(ref parent_scope) = self.parent {
            parent_scope.lookup(name)
        } else {
            None
        }
    }
}

/// The Intermediate Representation (IR) is defined as an enum.
/// Here we represent programs, functions, variable declarations, assignments,
/// control flow, and expressions.
#[derive(Debug)]
pub enum IR {
    /// The entire program: globals and functions.
    Program {
        globals: Vec<IR>,
        functions: Vec<IR>,
    },
    /// A function with a name, return type, parameters, body and a scope
    /// for its local symbols.
    Function {
        name: String,
        return_type: Datatype,
        params: Vec<(String, Datatype)>,
        body: Vec<IR>,
        scope: Scope,
    },
    /// A variable declaration.
    VarDecl {
        name: String,
        datatype: Datatype,
    },
    /// An assignment statement.
    Assign {
        target: String,
        expr: Box<IR>,
    },
    /// An if statement with a condition and then/else bodies.
    If {
        condition: Box<IR>,
        then_body: Vec<IR>,
        else_body: Option<Vec<IR>>,
    },
    /// A while loop.
    While {
        condition: Box<IR>,
        body: Vec<IR>,
    },
    /// A for loop.
    For {
        var: String,
        init: Box<IR>,
        condition: Box<IR>,
        update: Box<IR>,
        body: Vec<IR>,
    },
    /// A return statement.
    Return {
        expr: Option<Box<IR>>,
    },
    /// An IR expression wrapped as a statement.
    Expression(IRExpr),
}

/// IR expressions. We re-use the BinaryOp and UnaryOp from the AST.
#[derive(Debug)]
pub enum IRExpr {
    Binary {
        op: BinaryOp,
        left: Box<IRExpr>,
        right: Box<IRExpr>,
    },
    Unary {
        op: UnaryOp,
        expr: Box<IRExpr>,
    },
    Literal(String),
    Identifier(String),
    FunctionCall {
        name: String,
        args: Vec<IRExpr>,
    },
}