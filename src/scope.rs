/*
The symbol table stores only essential type information
needed for semantic validation, rather than full AST nodes.
*/

use crate::ast::Type;
use crate::token::Span;
use std::{cell::RefCell, collections::HashMap, rc::Rc};

/// Represents a scope in semantic analysis, containing:
///     - A symbol table that maps variable and method names to TableEntry
///     - An optional parent scope for nested lookups
#[derive(Debug)]
pub struct Scope {
    // allow multiple references to parent scope, while enabling
    // mutability as needed
    pub parent: Option<Rc<RefCell<Scope>>>,
    pub table: HashMap<String, TableEntry>,
    pub id: Option<String>, // for debugging purposes
    pub enclosing_block: Option<EnclosingBlock>, // can be a method_decl (main) or a block (while (){})
                                          // can be recursively looked up
}

#[derive(Debug, Clone)]
pub enum EnclosingBlock {
    Method(String),
    Loop

}

/// Symbol table entry type, representing locally-
/// declared variables or Method.
/// Have just enough information to perform semantic
/// check, without requiring, e.g., full method bodies.
#[allow(dead_code)]
#[derive(Debug, Clone)]
pub enum TableEntry {
    Variable {
        name: String,
        typ: Type,
        is_array: bool,
        span: Span,
    },
    Method {
        name: String,
        return_type: Type,
        params: Vec<(Type, String)>,
        span: Span,
    },
    Import {
        name: String,
        span: Span,
    },
}

/// Functions for creating new scopes
impl Scope {
    /// Create a new global scope
    pub fn new() -> Self {
        Scope {
            table: HashMap::new(),
            parent: None,
            id: Some("Global scope".to_string()),
            enclosing_block: None,
        }
    }

    /// Add a child scope with a pointer back to the parent.
    /// Optionally, define the method this scope represents.
    pub fn add_child(parent: Rc<RefCell<Scope>>, enclosing_block: Option<EnclosingBlock>) -> Self {
        let parent_id = parent.borrow().id.clone().unwrap_or_else(|| "??".to_string());
    
        Scope {
            table: HashMap::new(),
            parent: Some(parent),
            id: Some(format!("Child of: {}", parent_id)),
            enclosing_block, // Now tracks both methods and loops
        }
    }
    

    pub fn insert(&mut self, name: String, datatype: TableEntry) {
        self.table.insert(name, datatype);
    }

    pub fn lookup(&self, name: &str) -> Option<TableEntry> {
        if let Some(entry) = self.table.get(name) {
            Some(entry.clone())
        } else if let Some(ref parent_scope) = self.parent {
            parent_scope.borrow().lookup(name)
        } else {
            None
        }
    }

    /// ✅ Checks if the current scope or any parent scope is inside a loop
    pub fn is_inside_loop(&self) -> bool {
        let mut current_scope: Option<Rc<RefCell<Scope>>> = self.parent.clone(); // ✅ Start at parent

        while let Some(scope_rc) = current_scope {
            let scope_ref = scope_rc.borrow(); // ✅ Borrow the scope
            
            if let Some(EnclosingBlock::Loop) = &scope_ref.enclosing_block {
                return true; // ✅ Found a loop, return immediately
            }

            current_scope = scope_ref.parent.clone(); // ✅ Move up scope tree safely
        }

        false 
    }

    /// ✅ Recursively finds the closest enclosing method scope, if it exists
    pub fn find_enclosing_method(&self) -> Option<String> {
        let mut current_scope: Option<Rc<RefCell<Scope>>> = self.parent.clone(); // ✅ Start from parent

        while let Some(scope_rc) = current_scope {
            let scope_ref = scope_rc.borrow();

            if let Some(EnclosingBlock::Method(name)) = &scope_ref.enclosing_block {
                return Some(name.clone()); // ✅ Return method name if found
            }

            current_scope = scope_ref.parent.clone(); // ✅ Move up the scope tree
        }

        None 
    }



}
