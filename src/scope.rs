/*
The symbol table stores only essential type information 
needed for semantic validation, rather than full AST nodes.
*/

use crate::ast::Type;
use std::{cell::RefCell, collections::HashMap, rc::Rc};


/// Represents a scope in semantic analysis, containing:
///     - A symbol table that maps variable and method names to TableEntry
///     - An optional parent scope for nested lookups
#[derive(Debug)]
pub struct Scope {
    // allow multiple references to parent scope, while enabling
    // mutability as needed
    parent: Option<Rc<RefCell<Scope>>>,
    table: HashMap<String, TableEntry>,
}

/// Symbol table entry type, representing locally-
/// declared variables or Method.
/// Have just enough information to perform semantic
/// check, without requiring, e.g., full method bodies.
#[derive(Debug, Clone)]
pub enum TableEntry {
    Variable {
        name: String,
        typ: Type,
        is_array: bool,
    },
    Method {
        name: String,
        return_type: Type,
        params: Vec<(Type, String)>,
    },
}

/// Functions for creating new scopes
/// and adding to symbol tables.
impl Scope {
    /// Create a new (global) scope.
    pub fn new() -> Self {
        Scope {
            table: HashMap::new(),
            parent: None,
        }
    }

    /// Create a new child scope from an existing parent.
    pub fn add_child(parent: Rc<RefCell<Scope>>) -> Self {
        Scope {
            table: HashMap::new(),
            parent: Some(parent.clone()), // Keep parent reference instead of moving ownership
        }
    }
    

    /// Insert a new symbol into the symbol table for the scope.
    pub fn insert(&mut self, name: String, datatype: TableEntry) {
        self.table.insert(name, datatype);
    }

    /// Lookup a symbol in this scope or any parent scope.
    pub fn lookup(&self, name: &str) -> Option<TableEntry> {
        if let Some(entry) = self.table.get(name) {
            Some(entry.clone())
        } else if let Some(ref parent_scope) = self.parent {
            parent_scope.borrow().lookup(name)
        } else {
            None
        }
    }
    

}
