/*
Perform semantic checks on the AST produced by parsing.
Build a symbol table to enable these checks.
*/
use core::panic;
use std::cell::RefCell;
use std::rc::Rc;
use std::collections::HashMap;
use crate::ast::*;
use crate::parse::parse;
use crate::symtable::*;
use crate::scope::*;
use crate::utils::print::*;

// #################################################
// AST --> SYMBOL TABLE AST CONSTRUCTION
// #################################################
/// Builds the IR representation of a program
pub fn build_symbol_table(ast: &AST) -> SymProgram {
    match ast {
        AST::Program { imports: _, fields, methods, span } => {
            let global_scope = Rc::new(RefCell::new(Scope::new()));
            let mut method_bodies = HashMap::new();

            // Process global variables
            for field in fields {
                match **field {
                    AST::FieldDecl { ref typ, ref decls, ref span } => {
                        for decl in decls {
                            match **decl {
                                AST::Identifier { ref id, ref span } => {
                                    global_scope.borrow_mut().insert(id.clone(), TableEntry::Variable {
                                        name: id.clone(),
                                        typ: typ.clone(),
                                        is_array: false,
                                        span: span.clone(),
                                    });
                                }
                                AST::ArrayFieldDecl { ref id, ref size, ref span } => {
                                    let _array_size = size.parse::<usize>().expect("Invalid array size");
                                    global_scope.borrow_mut().insert(id.clone(), TableEntry::Variable {
                                        name: id.clone(),
                                        typ: typ.clone(),
                                        is_array: true,
                                        span: span.clone(),
                                    });
                                }
                                _ => panic!("Unexpected field declaration type!"),
                            }
                        }
                    }
                    _ => panic!("Unexpected field declaration type!"),
                }
            }

            // Process method definitions
            for method in methods {
                match **method {
                    AST::MethodDecl { ref return_type, ref name, ref params, ref block, ref span } => {
                        let ir_method = build_method(method, Rc::clone(&global_scope));
                        global_scope.borrow_mut().insert(name.clone(), TableEntry::Method {
                            name: name.clone(),
                            return_type: return_type.clone(),
                            params: params.iter().map(|p| (p.typ.clone(), match p.name.as_ref() {
                                AST::Identifier { id, .. } => id.clone(),
                                _ => panic!("Invalid parameter name"),
                            })).collect(),
                            span: span.clone(),
                        });
                        method_bodies.insert(ir_method.name.clone(), Rc::new(ir_method));
                    }
                    _ => panic!("Expected AST::MethodDecl"),
                }
            }

            SymProgram {
                global_scope,
                methods: method_bodies,
                span: span.clone(),
            }
        }
        _ => panic!("Expected AST::Program"),
    }
}

/// Builds an IR representation of a method
pub fn build_method(method: &AST, parent_scope: Rc<RefCell<Scope>>) -> SymMethod {
    match method {
        AST::MethodDecl { return_type, name, params, block, span } => {
            let method_scope = Rc::new(RefCell::new(Scope::add_child(parent_scope))); 

            for param in params {
                match param.name.as_ref() {
                    AST::Identifier { id, span } => {
                        method_scope.borrow_mut().insert(id.clone(), TableEntry::Variable {
                            name: id.clone(),
                            typ: param.typ.clone(),
                            is_array: false,
                            span: span.clone(),
                        });
                    }
                    _ => panic!("Invalid parameter name"),
                }
            }

            let ir_body = match **block {
                AST::Block { .. } => build_block(block, Rc::clone(&method_scope)), 
                _ => panic!("Expected method body to be a block"),
            };

            SymMethod {
                name: name.clone(),
                return_type: return_type.clone(),
                params: params.iter()
                    .map(|p| (
                        p.typ.clone(),
                        match p.name.as_ref() {
                            AST::Identifier { id, .. } => id.clone(),
                            _ => panic!("Invalid parameter name"),
                        },
                        p.span.clone()
                    ))
                    .collect(),
                scope: Rc::clone(&method_scope), 
                body: ir_body,
                span: span.clone(),
            }
            
        }
            
        _ => panic!("Expected AST::MethodDecl"),
    }
}
/// Builds an IR representation of a block
pub fn build_block(block: &AST, parent_scope: Rc<RefCell<Scope>>) -> SymBlock {
    match block {
        AST::Block { field_decls, statements, span } => {
            let is_function_body = Rc::strong_count(&parent_scope) == 2;

            let scope = if is_function_body {
                Rc::clone(&parent_scope)
            } else {
                Rc::new(RefCell::new(Scope::add_child(parent_scope)))
            };

            let mut sym_statements = Vec::new();

            // Process field declarations
            for field in field_decls {
                match **field {
                    AST::FieldDecl { ref typ, ref decls, ref span } => {
                        for decl in decls {
                            match **decl {
                                AST::Identifier { ref id, ref span } => {
                                    scope.borrow_mut().insert(id.clone(), TableEntry::Variable {
                                        name: id.clone(),
                                        typ: typ.clone(),
                                        is_array: false,
                                        span: span.clone()
                                    });

                                    sym_statements.push(Rc::new(SymStatement::VarDecl {
                                        name: id.clone(),
                                        typ: typ.clone(),
                                        is_array: false,
                                        span: span.clone(),
                                    }));
                                }
                                AST::ArrayFieldDecl { ref id, ref size, ref span } => {
                                    let _array_size = size.parse::<usize>().expect("Invalid array size");

                                    scope.borrow_mut().insert(id.clone(), TableEntry::Variable {
                                        name: id.clone(),
                                        typ: typ.clone(),
                                        is_array: true,
                                        span: span.clone()
                                    });

                                    sym_statements.push(Rc::new(SymStatement::VarDecl {
                                        name: id.clone(),
                                        typ: typ.clone(),
                                        is_array: true,
                                        span: span.clone(),
                                    }));
                                }
                                _ => panic!("Unexpected field declaration type!"),
                            }
                        }
                    }
                    _ => panic!("Unexpected field declaration type!"),
                }
            }

            // Process statements
            for stmt in statements {
                let sym_stmt = build_statement(stmt, Rc::clone(&scope));
                sym_statements.push(Rc::new(sym_stmt));
            }

            SymBlock {
                scope: Rc::clone(&scope),
                statements: sym_statements,
                span: span.clone(),
            }
        }
        _ => panic!("Expected AST::Block"),
    }
}

/// Build IR representation of statement
pub fn build_statement(statement: &AST, scope: Rc<RefCell<Scope>>) -> SymStatement {
    match statement {
        AST::Statement(Statement::Assignment { location, expr, op: _, span }) => {
            if let AST::Identifier { id, span: id_span } = location.as_ref() {
                if scope.borrow_mut().lookup(id).is_none() {
                    panic!(
                        "Variable `{}` used before declaration at line {}, column {}",
                        id, id_span.sline, id_span.scol
                    );
                }

                SymStatement::Assignment {
                    target: id.clone(),
                    expr: build_expr(expr, Rc::clone(&scope)),
                    span: span.clone(),
                }
            } else {
                panic!("Unsupported assignment target in AST: {:#?}", statement);
            }
        }

        AST::Statement(Statement::MethodCall { method_name, args, span }) => SymStatement::MethodCall {
            method_name: method_name.clone(),
            args: args.iter().map(|arg| build_expr(arg, Rc::clone(&scope))).collect(),
            span: span.clone(),
        },

        AST::Statement(Statement::If { condition, then_block, else_block, span }) => SymStatement::If {
            condition: build_expr(condition, Rc::clone(&scope)),
            then_block: Rc::new(build_block(then_block, Rc::clone(&scope))),
            else_block: else_block.as_ref().map(|blk| Rc::new(build_block(blk, Rc::clone(&scope)))),
            span: span.clone(),
        },

        AST::Statement(Statement::While { condition, block, span }) => SymStatement::While {
            condition: build_expr(condition, Rc::clone(&scope)),
            block: Rc::new(build_block(block, Rc::clone(&scope))),
            span: span.clone(),
        },

        AST::Statement(Statement::For { var, init, condition, update, block, span }) => {
            scope.borrow_mut().insert(var.clone(), TableEntry::Variable {
                name: var.clone(),
                typ: Type::Int, // TODO: Determine type dynamically
                is_array: false,
                span: span.clone()
            });

            let update_expr = match update.as_ref() {
                AST::Statement(Statement::Assignment { location, expr, .. }) => {
                    if let AST::Identifier { .. } = location.as_ref() {
                        build_expr(expr, Rc::clone(&scope))
                    } else {
                        panic!("For loop update must be an assignment to an identifier, got: {:#?}", location);
                    }
                }
                _ => panic!("For loop update must be an assignment statement, got: {:#?}", update),
            };

            SymStatement::For {
                var: var.clone(),
                init: build_expr(init, Rc::clone(&scope)),
                condition: build_expr(condition, Rc::clone(&scope)),
                update: update_expr,
                block: Rc::new(build_block(block, Rc::clone(&scope))),
                span: span.clone(),
            }
        }

        AST::Statement(Statement::Return { expr, span }) => SymStatement::Return {
            expr: expr.as_ref().map(|e| build_expr(e, Rc::clone(&scope))),
            span: span.clone(),
        },

        AST::Statement(Statement::Break { span }) => SymStatement::Break { span: span.clone() },
        AST::Statement(Statement::Continue { span }) => SymStatement::Continue { span: span.clone() },

        _ => panic!("========\nError in build_statement: unexpected AST node:\n {:#?}\n========", statement),
    }
}



/// **Converts an AST expression into an IR expression**
/// **Converts an AST expression into a SymExpr**
pub fn build_expr(expr: &AST, scope: Rc<RefCell<Scope>>) -> SymExpr {
    match expr {
        AST::Expr(Expr::Literal { lit, span }) => SymExpr::Literal {
            value: lit.clone(),
            span: span.clone(),
        },

        AST::Expr(Expr::BinaryExpr { op, left, right, span }) => SymExpr::BinaryExpr {
            op: op.clone(),
            left: Rc::new(build_expr(left, Rc::clone(&scope))),
            right: Rc::new(build_expr(right, Rc::clone(&scope))),
            typ: Type::Int, // TODO: Type inference
            span: span.clone(),
        },

        AST::Expr(Expr::UnaryExpr { op, expr, span }) => SymExpr::UnaryExpr {
            op: op.clone(),
            expr: Rc::new(build_expr(expr, Rc::clone(&scope))),
            typ: Type::Int,
            span: span.clone(),
        },

        AST::Expr(Expr::MethodCall { method_name, args, span }) => SymExpr::MethodCall {
            method_name: method_name.clone(),
            args: args.iter().map(|arg| Rc::new(build_expr(arg, Rc::clone(&scope)))).collect(),
            span: span.clone(),
        },

        AST::Expr(Expr::ArrAccess { id, index, span }) => SymExpr::ArrayAccess {
            id: id.clone(),
            index: Rc::new(build_expr(index, Rc::clone(&scope))),
            span: span.clone(),
        },

        AST::Expr(Expr::Len { id, span }) => {
            if let AST::Identifier { id, .. } = id.as_ref() {
                SymExpr::Len {
                    id: id.clone(),
                    span: span.clone(),
                }
            } else {
                panic!("Expected an identifier in Len expression, found: {:#?}", id);
            }
        },

        AST::Identifier { id, span } => {
            if let Some(entry) = scope.borrow_mut().lookup(id) {
                SymExpr::Identifier {
                    entry: entry.clone(),
                    span: span.clone(),
                }
            } else {
                panic!(
                    "Variable `{}` used before declaration at line {}, column {}",
                    id, span.sline, span.scol
                );
            }
        },

        _ => panic!("========\nError in build_expr: unexpected AST node:\n {:#?}\n========", expr),
    }
}


// #################################################
// SEMANTIC CHECKING
// #################################################

// pub fn check_program(scoped_tree: &SymProgram) {
//     match scoped_tree {
//         SymProgram { global_scope, methods } => {
//             for (name, method_body) in methods.iter() {
//                 println!("we got this method: {:?}", name);
//             }

//         },
//         _=> {
//             panic!("expected SymProgram!");
//         }
//     }
// }

// pub fn check_method(scoped_tree: &SymMethod) {

// }

/// Generates the IR from an AST
pub fn check_semantics(
    file: &str,
    filename: &str,
    writer: &mut Box<dyn std::io::Write>,
    verbose: bool,
) {
    let parse_tree = parse(file, filename, writer, false).expect("Parsing failed");
    // save_dot_file(&parse_tree, "parse_tree");
    let sym_tree = build_symbol_table(&parse_tree);

    if verbose {
        println!("succesffully built symbol table!!!!!");
        println!("=================SYMBOL TABLE====================");
        print_symtree(&sym_tree);
    }

    // check_program(&sym_tree);

}
