/*
Build an IR that only has the information necessary
for semantic checking.
*/

use core::panic;
use std::cell::RefCell;
use std::rc::Rc;
use std::collections::HashMap;
use crate::ast::*;
use crate::parse::parse;
use crate::symtable::*;
use crate::scope::*;


/// Builds the IR representation of a program
pub fn build_program(ast: &AST) -> IRProgram {
    match ast {
        AST::Program { imports: _, fields, methods } => {
            let global_scope = Rc::new(RefCell::new(Scope::new()));
            let mut method_bodies = HashMap::new();

            // Process global variables
            for field in fields {
                match **field {
                    AST::FieldDecl { ref typ, ref decls } => {
                        for decl in decls {
                            match **decl {
                                AST::Identifier(ref name) => {
                                    global_scope.borrow_mut().insert(name.clone(), TableEntry::Variable {
                                        name: name.clone(),
                                        typ: typ.clone(),
                                        is_array: false,
                                    });
                                }
                                AST::ArrayFieldDecl { ref id, ref size } => {
                                    let _array_size = size.parse::<usize>().expect("Invalid array size");
                                    global_scope.borrow_mut().insert(id.clone(), TableEntry::Variable {
                                        name: id.clone(),
                                        typ: typ.clone(),
                                        is_array: true,
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
                    AST::MethodDecl { ref return_type, ref name, ref params, block: _ } => {
                        // Build IR, and method to current scope
                        let ir_method = build_method(method, Rc::clone(&global_scope));
                        global_scope.borrow_mut().insert(ir_method.name.clone(), TableEntry::Method{name: name.clone(), return_type: return_type.clone(), params: params.clone()});
                        method_bodies.insert(ir_method.name.clone(), Rc::new(ir_method));

                    }
                    _ => {panic!("expected method")}
                }
                
            }

            IRProgram {
                global_scope: global_scope,
                methods: method_bodies,
            }
        }
        _ => panic!("Expected AST::Program"),
    }
}

/// Builds an IR representation of a method
pub fn build_method(ast_method: &AST, parent_scope: Rc<RefCell<Scope>>) -> IRMethod {
    match ast_method {
        AST::MethodDecl { return_type, name, params, block } => {
            let method_scope = Rc::new(RefCell::new(Scope::add_child(parent_scope))); // ✅ Create function scope

            // Register parameters in the same scope as function body
            for (typ, param_name) in params {
                method_scope.borrow_mut().insert(param_name.clone(), TableEntry::Variable {
                    name: param_name.clone(),
                    typ: typ.clone(),
                    is_array: false,
                });
            }

            // Pass `method_scope` directly instead of making a new one in `build_block`
            let ir_body = match **block {
                AST::Block { .. } => build_block(block, Rc::clone(&method_scope)), // ✅ Same scope for method body
                _ => panic!("Expected method body to be a block"),
            };

            IRMethod {
                name: name.clone(),
                return_type: return_type.clone(),
                params: params.clone(),
                scope: Rc::clone(&method_scope), // Same scope shared with `block`
                body: ir_body,
            }
        }
        _ => panic!("Expected AST::MethodDecl"),
    }
}


/// Builds an IR representation of a block
pub fn build_block(ast_block: &AST, parent_scope: Rc<RefCell<Scope>>) -> IRBlock {
    match ast_block {
        AST::Block { field_decls, statements } => {
            let is_function_body = Rc::strong_count(&parent_scope) == 2; // Detect if this is a function body by checking the refcnt

            let scope = if is_function_body {
                Rc::clone(&parent_scope) // Reuse function scope if inside method
            } else {
                Rc::new(RefCell::new(Scope::add_child(parent_scope))) // Create a new scope only for nested blocks
            };

            let mut ir_statements = Vec::new();

            // Process field declarations in the correct scope
            for field in field_decls {
                match **field {
                    AST::FieldDecl { ref typ, ref decls } => {
                        for decl in decls {
                            match **decl {
                                AST::Identifier(ref name) => {
                                    scope.borrow_mut().insert(name.clone(), TableEntry::Variable {
                                        name: name.clone(),
                                        typ: typ.clone(),
                                        is_array: false,
                                    });

                                    ir_statements.push(Rc::new(IRStatement::VarDecl {
                                        name: name.clone(),
                                        typ: typ.clone(),
                                        is_array: false,
                                    }));
                                }
                                AST::ArrayFieldDecl { ref id, ref size } => {
                                    let _array_size = size.parse::<usize>().expect("Invalid array size");

                                    scope.borrow_mut().insert(id.clone(), TableEntry::Variable {
                                        name: id.clone(),
                                        typ: typ.clone(),
                                        is_array: true,
                                    });

                                    ir_statements.push(Rc::new(IRStatement::VarDecl {
                                        name: id.clone(),
                                        typ: typ.clone(),
                                        is_array: true,
                                    }));
                                }
                                _ => panic!("Unexpected field declaration type!"),
                            }
                        }
                    }
                    _ => panic!("Unexpected field declaration type!"),
                }
            }

            for stmt in statements {
                let ir_stmt = build_statement(stmt, Rc::clone(&scope));
                ir_statements.push(Rc::new(ir_stmt));
            }

            IRBlock {
                scope: Rc::clone(&scope),
                statements: ir_statements,
            }
        }
        _ => panic!("Expected AST::Block"),
    }
}



/// Build IR representation of statement
pub fn build_statement(ast_stmt: &AST, scope: Rc<RefCell<Scope>>) -> IRStatement {
    match ast_stmt {
        AST::Statement(Statement::Assignment { location, expr, op: _ }) => {
            if let AST::Identifier(target) = location.as_ref() {
                if scope.borrow().lookup(&target).is_none() {
                    panic!("Variable `{}` used before declaration", target);
                }

                IRStatement::Assignment {
                    target: target.clone(),
                    expr: build_expr(expr, Rc::clone(&scope)), // ✅ Pass scope to `build_expr`
                }
            } else {
                panic!("Unsupported assignment target in AST");
            }
        }

        AST::Statement(Statement::MethodCall { method_name, args }) => IRStatement::MethodCall {
            method_name: method_name.clone(),
            args: args.iter().map(|arg| build_expr(arg, Rc::clone(&scope))).collect(),
        },

        AST::Statement(Statement::If { condition, then_block, else_block }) => IRStatement::If {
            condition: build_expr(condition, Rc::clone(&scope)),
            then_block: Rc::new(build_block(then_block, Rc::clone(&scope))),
            else_block: else_block.as_ref().map(|blk| Rc::new(build_block(blk, Rc::clone(&scope)))),
        },

        AST::Statement(Statement::While { condition, block }) => IRStatement::While {
            condition: build_expr(condition, Rc::clone(&scope)),
            block: Rc::new(build_block(block, Rc::clone(&scope))),
        },

        AST::Statement(Statement::For { var, init, condition, update, block }) => {
            // Register loop variable
            scope.borrow_mut().insert(var.clone(), TableEntry::Variable {
                name: var.clone(),
                typ: Type::Int, // TODO: Determine type dynamically
                is_array: false,
            });

            IRStatement::For {
                var: var.clone(),
                init: build_expr(init, Rc::clone(&scope)),
                condition: build_expr(condition, Rc::clone(&scope)),
                update: build_expr(update, Rc::clone(&scope)),
                block: Rc::new(build_block(block, Rc::clone(&scope))),
            }
        }

        AST::Statement(Statement::Return { expr }) => IRStatement::Return {
            expr: expr.as_ref().map(|e| build_expr(e, Rc::clone(&scope))),
        },

        AST::Statement(Statement::Break) => IRStatement::Break,
        AST::Statement(Statement::Continue) => IRStatement::Continue,

        _ => panic!("Unexpected AST node in build_statement"),
    }
}

/// **Converts an AST expression into an IR expression**
pub fn build_expr(ast_expr: &AST, scope: Rc<RefCell<Scope>>) -> IRExpr {
    match ast_expr {
        AST::Expr(Expr::Literal(lit)) => IRExpr::Literal(lit.clone()),

        AST::Expr(Expr::BinaryExpr { op, left, right }) => IRExpr::BinaryExpr {
            op: op.clone(),
            left: Rc::new(build_expr(left, Rc::clone(&scope))),
            right: Rc::new(build_expr(right, Rc::clone(&scope))),
            typ: Type::Int, // TODO: Type inference
        },

        AST::Expr(Expr::UnaryExpr { op, expr }) => IRExpr::UnaryExpr {
            op: op.clone(),
            expr: Rc::new(build_expr(expr, Rc::clone(&scope))),
            typ: Type::Int,
        },

        AST::Expr(Expr::MethodCall { method_name, args }) => IRExpr::MethodCall {
            method_name: method_name.clone(),
            args: args.iter().map(|arg| Rc::new(build_expr(arg, Rc::clone(&scope)))).collect(),
        },

        AST::Expr(Expr::ArrAccess { id, index }) => IRExpr::ArrayAccess {
            id: id.clone(),
            index: Rc::new(build_expr(index, Rc::clone(&scope))),
        },

        AST::Expr(Expr::Len { id }) => IRExpr::Len {
            id: id.clone(),
        },

        AST::Identifier(name) => {
            if let Some(entry) = scope.borrow().lookup(name) {
                IRExpr::Identifier(entry.clone())
            } else {
                panic!("Variable `{}` used before declaration", name);
            }
        },

        _ => panic!("Unexpected AST node in build_expr"),
    }
}


/// Generates the IR from an AST
pub fn generate_ir(
    file: &str,
    filename: &str,
    writer: &mut Box<dyn std::io::Write>,
    verbose: bool,
) {
    let parse_tree = parse(file, filename, writer, false).expect("Parsing failed");
    let ir_program = build_program(&parse_tree);

    if verbose {
        println!("{:#?}", ir_program);
    }
}
