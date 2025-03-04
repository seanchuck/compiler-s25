/*
Perform semantic checks on the AST produced by parsing.
Build a symbol table to enable these checks.

Some of the 23 semantic checks are performed while building the 
symbol table, and others are performed explicitly after.

Use-before-declaration is caught by the grammar/parser.


Be careful with how you pass Scope:
    - Use Rc<Refcell<Scope>> for shared, mutable references.
    - Use &Scope for read-only references.



TODO: AVOID MULTIPLE MESSAGES FOR SAME ERROR
*/

use core::panic;
use std::cell::RefCell;
use std::collections::HashMap;
use std::collections::HashSet;
use std::rc::Rc;

use crate::ast::*;
use crate::parse::parse;
use crate::scope::*;
use crate::symtable::*;
use crate::token::Literal;
use crate::token::Span;
use crate::utils::print::*;

// #################################################
// AST --> SYMBOL TABLE AST CONSTRUCTION
// #################################################
// Some of the semantic checks are also performed 
// During this phase

/// Build the symbol-table-augmented version of the AST
pub fn build_symbol_table(
    ast: &AST,
    filename: &str,
    writer: &mut dyn std::io::Write,
) -> SymProgram {
    match ast {
        AST::Program {
            imports,
            fields,
            methods,
            span,
        } => {
            let global_scope = Rc::new(RefCell::new(Scope::new()));
            let mut method_bodies = HashMap::new();

            // RULE 1: No duplicate imports
            check_duplicate_imports(imports, filename, writer);
            check_main_exists(methods, filename, writer);

            // Process import methods, which are handled slightly diff. from method_decls
            for import in imports {
                match **import {
                    AST::ImportDecl { ref id, span } => {

                        global_scope.borrow_mut().insert(
                            id.clone(),
                            TableEntry::Import { 
                                name: id.clone(),
                                span // implements copy
                            }
                        );
                    }
                    _ => panic!("Invalid import!")
                }
            }

            // Process global variables
            for field in fields {
                match **field {
                    AST::FieldDecl {
                        ref typ,
                        ref decls,
                        ref span,
                    } => {
                        for decl in decls {
                            match **decl {
                                // scalar variable declaration
                                AST::Identifier { ref id, ref span } => {
                                    global_scope.borrow_mut().insert(
                                        id.clone(),
                                        TableEntry::Variable {
                                            name: id.clone(),
                                            typ: typ.clone(),
                                            is_array: false,
                                            span: span.clone(),
                                        },
                                    );
                                }

                                // array variabe declaration
                                AST::ArrayFieldDecl {
                                    ref id,
                                    ref size,
                                    ref span,
                                } => {
                                    check_array_size(size, span, filename, writer);

                                    // Insert variable into the global scope
                                    global_scope.borrow_mut().insert(
                                        id.clone(),
                                        TableEntry::Variable {
                                            name: id.clone(),
                                            typ: typ.clone(),
                                            is_array: true,
                                            span: span.clone(),
                                        },
                                    );
                                }
                                _ => panic!("Unexpected field declaration type")
                            }
                        }
                    }
                    _ => panic!("invalid field declaration!")
                }
            }

            // Process method definitions
            for method in methods {
                match **method {
                    AST::MethodDecl {
                        ref return_type,
                        ref name,
                        ref params,
                        ref block,
                        ref span,
                    } => {
                        println!("serint method{}", name);
                        // Insert function into table before processing body
                        global_scope.borrow_mut().insert(
                            name.clone(),
                            TableEntry::Method {
                                name: name.clone(),
                                return_type: return_type.clone(),
                                params: params
                                    .iter()
                                    .map(|p| {
                                        (
                                            p.typ.clone(),
                                            match p.name.as_ref() {
                                                AST::Identifier { id, .. } => id.clone(),
                                                _ => panic!("Invalid parameter name"),
                                            },
                                        )
                                    })
                                    .collect(),
                                span: span.clone(),
                            },
                        );
                        println!("method table is now: {:#?}", global_scope);
                        
                        // Now process the function body
                        let method = build_method(method, Rc::clone(&global_scope), filename, writer);

                        // Store the method body as well
                        method_bodies.insert(method.name.clone(), Rc::new(method));

                    }
                    _ => unreachable!(),
                }
            }

            // Return the symbol-table-augmented AST
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
pub fn build_method(
    method: &AST,
    parent_scope: Rc<RefCell<Scope>>,
    filename: &str,
    writer: &mut dyn std::io::Write,
) -> SymMethod {
    match method {
        AST::MethodDecl {
            return_type,
            name,
            params,
            block,
            span,
        } => {
            let method_scope = Rc::new(RefCell::new(Scope::add_child(parent_scope, Some(name.clone()))));

            // Process method params, and add into method's outer-most scope
            for param in params {
                match param.name.as_ref() {
                    AST::Identifier { id, span } => {
                        method_scope.borrow_mut().insert(
                            id.clone(),
                            TableEntry::Variable {
                                name: id.clone(),
                                typ: param.typ.clone(),
                                is_array: false,
                                span: span.clone(),
                            },
                        );
                    }
                    _ => unreachable!(),
                }
            }

            // Process method body
            let method_body = match **block {
                AST::Block { .. } => build_block(block, Rc::clone(&method_scope), filename, writer),
                _ => panic!("Expected method body to be a block"),
            };

            SymMethod {
                name: name.clone(),
                return_type: return_type.clone(),
                params: params
                    .iter()
                    .map(|p| {
                        (
                            p.typ.clone(),
                            match p.name.as_ref() {
                                AST::Identifier { id, .. } => id.clone(),
                                _ => panic!("Invalid parameter name"),
                            },
                            p.span.clone(),
                        )
                    })
                    .collect(),
                scope: Rc::clone(&method_scope),
                body: method_body,
                span: span.clone(),
            }
        }
        _ => panic!("Expected AST::MethodDecl"),
    }
}

/// Builds an IR representation of a block
pub fn build_block(
    block: &AST,
    parent_scope: Rc<RefCell<Scope>>,
    filename: &str,
    writer: &mut dyn std::io::Write,
) -> SymBlock {
    match block {
        AST::Block {
            field_decls,
            statements,
            span,
        } => {
            // Don't create a new scope if this is just a function body
            let is_function_body = Rc::strong_count(&parent_scope) == 2;
            let scope = if is_function_body {
                Rc::clone(&parent_scope)
            } else {
                Rc::new(RefCell::new(Scope::add_child(parent_scope, None)))
            };

            let mut sym_statements = Vec::new();

            // Block creates a new scope that can have its own field decls
            for field in field_decls {
                match **field {
                    AST::FieldDecl {
                        ref typ,
                        ref decls,
                        ref span,
                    } => {
                        for decl in decls {
                            match **decl {
                                // Scalar field declaration
                                AST::Identifier { ref id, ref span } => {
                                    scope.borrow_mut().insert(
                                        id.clone(),
                                        TableEntry::Variable {
                                            name: id.clone(),
                                            typ: typ.clone(),
                                            is_array: false,
                                            span: span.clone(),
                                        },
                                    );

                                    sym_statements.push(Rc::new(SymStatement::VarDecl {
                                        name: id.clone(),
                                        typ: typ.clone(),
                                        is_array: false,
                                        span: span.clone(),
                                    }));
                                }

                                // Array field declaration
                                AST::ArrayFieldDecl {
                                    ref id,
                                    ref size,
                                    ref span,
                                } => {
                                    check_array_size(size, span, filename, writer);

                                    scope.borrow_mut().insert(
                                        id.clone(),
                                        TableEntry::Variable {
                                            name: id.clone(),
                                            typ: typ.clone(),
                                            is_array: true,
                                            span: span.clone(),
                                        },
                                    );

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
                let sym_stmt = build_statement(stmt, Rc::clone(&scope), filename, writer);
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

/// Convert AST statements into
pub fn build_statement(
    statement: &AST,
    scope: Rc<RefCell<Scope>>,
    filename: &str,
    writer: &mut dyn std::io::Write,
) -> SymStatement {
    match statement {
        AST::Statement(Statement::Assignment {
            location,
            expr,
            op: _,
            span,
        }) => {
            match location.as_ref() {
                // Plain variable assignment (x = 3;)
                AST::Identifier { id, span: id_span } => {
                    check_used_before_decl(id, scope.clone(), span, filename, writer);

                    SymStatement::Assignment {
                        target: id.clone(),
                        expr: build_expr(expr, Rc::clone(&scope), filename, writer),
                        span: span.clone(),
                    }
                }

                AST::Expr(Expr::ArrAccess {
                    id,
                    index,
                    span: arr_span,
                }) => {
                    if let Some(TableEntry::Variable {
                        typ,
                        is_array,
                        span: decl_span,
                        ..
                    }) = scope.borrow_mut().lookup(id)
                    {
                        if !is_array {
                            panic!(
                                "Variable `{}` used as an array but declared as a non-array at line {}, column {}",
                                id, decl_span.sline, decl_span.scol
                            );
                        }
                    } else {
                        panic!(
                            "Array `{}` used before declaration at line {}, column {}",
                            id, arr_span.sline, arr_span.scol
                        );
                    }

                    SymStatement::Assignment {
                        target: id.clone(),
                        expr: build_expr(expr, Rc::clone(&scope), filename, writer),
                        span: span.clone(),
                    }
                }

                _ => panic!(
                    "Unsupported assignment target in AST at line {}, column {}: {:#?}",
                    span.sline, span.scol, statement
                ),
            }
        }

        AST::Statement(Statement::MethodCall {
            method_name,
            args,
            span,}) => {

            check_methodcall(&method_name, &args, span, scope.clone(), false, filename, writer);

            SymStatement::MethodCall {
            method_name: method_name.clone(),
            args: args
                .iter()
                .map(|arg| build_expr(arg, Rc::clone(&scope), filename, writer))
                .collect(),
            span: span.clone(),
            }
        },

        AST::Statement(Statement::If {
            condition,
            then_block,
            else_block,
            span,
        }) => SymStatement::If {
            condition: build_expr(condition, Rc::clone(&scope), filename, writer),
            then_block: Rc::new(build_block(then_block, Rc::clone(&scope), filename, writer)),
            else_block: else_block
                .as_ref()
                .map(|blk| Rc::new(build_block(blk, Rc::clone(&scope), filename, writer))),
            span: span.clone(),
        },

        AST::Statement(Statement::While {
            condition,
            block,
            span,
        }) => SymStatement::While {
            condition: build_expr(condition, Rc::clone(&scope), filename, writer),
            block: Rc::new(build_block(block, Rc::clone(&scope), filename, writer)),
            span: span.clone(),
        },

        AST::Statement(Statement::For {
            var,
            init,
            condition,
            update,
            block,
            span,
        }) => {
            scope.borrow_mut().insert(
                var.clone(),
                TableEntry::Variable {
                    name: var.clone(),
                    typ: Type::Int, // TODO: Determine type dynamically
                    is_array: false,
                    span: span.clone(),
                },
            );

            let update_expr = match update.as_ref() {
                AST::Statement(Statement::Assignment { location, expr, .. }) => {
                    if let AST::Identifier { .. } = location.as_ref() {
                        build_expr(expr, Rc::clone(&scope), filename, writer)
                    } else {
                        panic!(
                            "For loop update must be an assignment to an identifier, got: {:#?}",
                            location
                        );
                    }
                }
                _ => panic!(
                    "For loop update must be an assignment statement, got: {:#?}",
                    update
                ),
            };

            SymStatement::For {
                var: var.clone(),
                init: build_expr(init, Rc::clone(&scope), filename, writer),
                condition: build_expr(condition, Rc::clone(&scope), filename, writer),
                update: update_expr,
                block: Rc::new(build_block(block, Rc::clone(&scope), filename, writer)),
                span: span.clone(),
            }
        }

        AST::Statement(Statement::Return { expr, span }) => {
            check_return_value(expr.as_ref(), &scope.borrow(), span, filename, writer);
            SymStatement::Return {
            expr: expr
                .as_ref()
                .map(|e| build_expr(e, Rc::clone(&scope), filename, writer)),
            span: span.clone(),
            }
        }

        AST::Statement(Statement::Break { span }) => SymStatement::Break { span: span.clone() },
        AST::Statement(Statement::Continue { span }) => {
            SymStatement::Continue { span: span.clone() }
        }

        _ => panic!(
            "Error in build_statement: unexpected AST node:\n {:#?}",
            statement
        ),
    }
}

/// **Converts an AST expression into a SymExpr**
pub fn build_expr(
    expr: &AST,
    scope: Rc<RefCell<Scope>>,
    filename: &str,
    writer: &mut dyn std::io::Write,
) -> SymExpr {
    match expr {
        AST::Expr(Expr::BinaryExpr {
            op,
            left,
            right,
            span,
        }) => SymExpr::BinaryExpr {
            op: op.clone(),
            left: Rc::new(build_expr(left, Rc::clone(&scope), filename, writer)),
            right: Rc::new(build_expr(right, Rc::clone(&scope), filename, writer)),
            typ: Type::Int, // TODO: Type inference
            span: span.clone(),
        },

        AST::Expr(Expr::UnaryExpr { op, expr, span }) => SymExpr::UnaryExpr {
            op: op.clone(),
            expr: Rc::new(build_expr(expr, Rc::clone(&scope), filename, writer)),
            typ: Type::Int,
            span: span.clone(),
        },

        AST::Expr(Expr::MethodCall {
            method_name,
            args,
            span,
        }) => {
            check_methodcall(&method_name, args, span, scope.clone(), true, filename, writer);
            SymExpr::MethodCall {
            method_name: method_name.clone(),
            args: args
                .iter()
                .map(|arg| Rc::new(build_expr(arg, Rc::clone(&scope), filename, writer)))
                .collect(),
            span: span.clone(),
            }
        },

        AST::Expr(Expr::ArrAccess { id, index, span }) => SymExpr::ArrayAccess {
            id: id.clone(),
            index: Rc::new(build_expr(index, Rc::clone(&scope), filename, writer)),
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
        }

        AST::Expr(Expr::Cast {
            target_type,
            expr,
            span,
        }) => SymExpr::Cast {
            target_type: target_type.clone(),
            expr: Rc::new(build_expr(expr, Rc::clone(&scope), filename, writer)),
            span: span.clone(),
        },

        AST::Expr(Expr::Literal { lit, span }) => SymExpr::Literal {
            value: lit.clone(),
            span: span.clone(),
        },

        AST::Identifier { ref id, ref span } => {
            // RULE 2: no identifier is used before being declared
            check_used_before_decl(id, Rc::clone(&scope), span, filename, writer);

            let entry = scope.borrow_mut().lookup(id).unwrap();
            SymExpr::Identifier {
                entry: entry.clone(),
                span: span.clone(),
            }
        }

        _ => panic!(
            "Error in build_expr: unexpected AST node:\n {:#?}",expr),
    }
}



// #################################################
// HELPERS
// #################################################

fn format_error_message(id: &str, span: Option<&Span>, filename: &str, msg: &str) -> String {
    match span {
        Some(span) => format!(
            "Semantic error in \"{}\" (line {}, col {}): {} `{}`",
            filename, span.sline, span.scol, msg, id
        ),
        None => format!(
            "Semantic error in \"{}\": {} `{}`",
            filename, msg, id
        ),
    }
}

/// Infer the type of an expression from the given scope
fn infer_expr_type(expr: &AST, scope: &Scope) -> Type {
    match expr {
        // Integer, Boolean, and Long Literals
        AST::Expr(Expr::Literal { lit, .. }) => match lit {
            Literal::Int(_) => Type::Int,
            Literal::Bool(_) => Type::Bool,
            Literal::Long(_) => Type::Long,
            _=> Type::Unknown
        },

        // Variable Reference (Check Scope)
        AST::Identifier { id, .. } => match scope.lookup(id) {
            Some(TableEntry::Variable { typ, .. }) => typ.clone(),
            _ => Type::Unknown,
        },

        // Binary Expressions (`+`, `-`, `*`, `/`, `%`, etc.)
        AST::Expr(Expr::BinaryExpr { left, right, op, .. }) => {
            let left_type = infer_expr_type(left, scope);
            let right_type = infer_expr_type(right, scope);

            match op {
                BinaryOp::Add | BinaryOp::Subtract | BinaryOp::Multiply |
                BinaryOp::Divide | BinaryOp::Modulo => {
                    if left_type == Type::Int && right_type == Type::Int {
                        Type::Int
                    } else if left_type == Type::Long && right_type == Type::Long {
                        Type::Long
                    } else {
                        Type::Unknown // Type mismatch
                    }
                }
                BinaryOp::And | BinaryOp::Or => {
                    if left_type == Type::Bool && right_type == Type::Bool {
                        Type::Bool
                    } else {
                        Type::Unknown
                    }
                }
                BinaryOp::Equal | BinaryOp::NotEqual | BinaryOp::Less |
                BinaryOp::Greater | BinaryOp::LessEqual | BinaryOp::GreaterEqual => Type::Bool,
            }
        },

        // Unary Expressions (`-`, `!`)
        AST::Expr(Expr::UnaryExpr { op, expr, .. }) => {
            let expr_type = infer_expr_type(expr, scope);
            match op {
                UnaryOp::Neg => {
                    if expr_type == Type::Int || expr_type == Type::Long {
                        expr_type
                    } else {
                        Type::Unknown
                    }
                }
                UnaryOp::Not => {
                    if expr_type == Type::Bool {
                        Type::Bool
                    } else {
                        Type::Unknown
                    }
                }
            }
        },

        // Array Access (`arr[i]`)
        AST::Expr(Expr::ArrAccess { id, index, .. }) => {
            let index_type = infer_expr_type(index, scope);
            if index_type != Type::Int {
                return Type::Unknown; // Array indices must be `int`
            }

            match scope.lookup(id) {
                Some(TableEntry::Variable { typ, .. }) => match typ {
                    Type::Int => Type::Int,
                    Type::Long => Type::Long,
                    Type::Bool => Type::Bool,
                    _ => Type::Unknown,
                },
                _ => Type::Unknown, // Array not found in scope
            }
        },

        // Method Call (`foo(5, true)`)
        AST::Expr(Expr::MethodCall { method_name, args, .. }) => {
            match scope.lookup(method_name) {
                Some(TableEntry::Method { return_type, params, .. }) => {
                    return_type.clone()
                },
                // Imports always return `int`
                Some(TableEntry::Import { name, span }) => Type::Int,
                _ => Type::Unknown, // Undefined method
            }
        },

        // Casting (`(int) x`)
        AST::Expr(Expr::Cast { target_type, expr, .. }) => {
            let expr_type = infer_expr_type(expr, scope);
            if expr_type == Type::Int || expr_type == Type::Long {
                target_type.clone()
            } else {
                Type::Unknown // Invalid cast
            }
        },

        // `len(arr)`
        AST::Expr(Expr::Len { id, span }) => {
            Type::Int
        },

        // Unknown return type
        _ => Type::Unknown,
    }
}


// #################################################
// SEMANTIC CHECKING
// #################################################
// There are 23 semantic rules we need to follow
// Some are checked while the SymTree is being made.
// Others are checked later explicitly.

/// RULE 1
fn check_duplicate_imports(imports: &[Box<AST>], filename: &str, writer: &mut dyn std::io::Write) {
    let mut seen = HashSet::new();

    for import in imports {
        if !seen.insert(import) {
            match **import {
                AST::ImportDecl { ref id, ref span } => {
                    writeln!(
                        writer,
                        "{}",
                        format_error_message(id, Some(span), filename, "duplicate import")
                    )
                    .expect("Failed to write output!");
                }
                _ => unreachable!(),
            }
        }
    }
}

/// RULE 2
fn check_used_before_decl(
    id: &str,
    scope: Rc<RefCell<Scope>>,
    span: &Span,
    filename: &str,
    writer: &mut dyn std::io::Write,
) {
    let scope_ref = scope.borrow();

    if scope_ref.lookup(id).is_none() {
        writeln!(
            writer,
            "{}",
            format_error_message(id, Some(span), filename, "Use before declaration")
        )
        .expect("Failed to write output!");
    }
}

/// RULE 3
fn check_main_exists(methods: &Vec<Box<AST>>, filename: &str, writer: &mut dyn std::io::Write) {
    let mut has_main = false;

    for method in methods {
        match **method {
            AST::MethodDecl { ref return_type, ref name, ref params, .. } => {
                if name == "main" && *return_type == Type::Void && params.is_empty() {
                    has_main = true;
                    break;
                }
            }
            _ => continue,
        }
    }
    if !has_main {
        let error_msg = format_error_message("main", None, filename, "Missing entry point:");
        writeln!(writer, "{}", error_msg).expect("Failed to write error message");
    }
}

/// Rules 4-6
fn check_methodcall(
    method_name: &String,
    args: &Vec<Box<AST>>, // Now considering actual arguments
    span: &Span,
    scope: Rc<RefCell<Scope>>,
    is_expr: bool,
    filename: &str,
    writer: &mut dyn std::io::Write,
) {
    // Lookup the method in the scope
    let scope = scope.borrow(); // Borrow Scope
    let method_entry = scope.lookup(method_name);

    match method_entry {
        // 1. Regular method
        Some(TableEntry::Method { return_type, params, span, .. }) => {
            // Validate argument count
            if args.len() != params.len() {
                let err_msg = format_error_message(
                    method_name,
                    Some(&span),
                    filename,
                    &format!(
                        "Incorrect number of arguments for method. Expected {}, but got {}.",
                        params.len(),
                        args.len()
                    ),
                );
                writeln!(writer, "{}", err_msg).unwrap();
            }

            // Validate argument types; string and char not allowed for non-imports
            for ((expected_type, param_name), arg) in params.iter().zip(args.iter()) {
                let arg_type = infer_expr_type(arg, &scope); // Assuming `infer_expr_type()` exists
                if arg_type != *expected_type {
                    let err_msg = format_error_message(
                        method_name,
                        Some(&span),
                        filename,
                        &format!(
                            "Type mismatch for parameter `{}`. Expected `{:?}`, but got `{:?}`.",
                            param_name, expected_type, arg_type
                        ),
                    );
                    writeln!(writer, "{}", err_msg).unwrap();
                }
            }

            // Check if method returns a value when used in an expression
            if is_expr && matches!(return_type, Type::Void) {
                let err_msg = format_error_message(
                    method_name,
                    Some(&span),
                    filename,
                    "Method does not return a value but is used in an expression.",
                );
                writeln!(writer, "{}", err_msg).unwrap();
            }
        },

        // 2. Imported method (external function)
        Some(TableEntry::Import { name, span }) => {
            // For imports, we only need to validate their existence
            return;
        },

        // 3. Method declaration is shadowed by variable declaration in stricter scope
        Some(TableEntry::Variable { name, typ, is_array, span }) => {
            let err_msg = format_error_message(
                method_name,
                Some(&span),
                filename,
                "Variable declaration shadows method declaration"
            );
            writeln!(writer, "{}", err_msg).unwrap();

        },

        // Method is not found in the scope → Report error
        None => {
            let err_msg = format_error_message(
                method_name,
                Some(&span),
                filename,
                "Call to undefined method.",
            );
            writeln!(writer, "{}", err_msg).unwrap();
        }
    }
}


// Rules 7-8
fn check_return_value(
    ret: Option<&Box<AST>>, 
    scope: &Scope,
    span: &Span, 
    filename: &str, 
    writer: &mut dyn std::io::Write
) {
    println!("we checking this retu");
    // Get the method name from scope
    let method_name = match &scope.enclosing_method {
        Some(name) => name.clone(),
        None => {
            writeln!(
                writer,
                "{}:{}:{}: ERROR: Return statement outside of a function.",
                filename, span.sline, span.scol
            ).expect("Failed to write error message");
            return;
        }
    };

    // Lookup method details in the symbol table
    let method_entry = match scope.lookup(&method_name) {
        Some(TableEntry::Method { return_type, .. }) => return_type.clone(),
        _ => {
            writeln!(
                writer,
                "{}:{}:{}: ERROR: Could not find method '{}' in symbol table.",
                filename, span.sline, span.scol, method_name
            ).expect("Failed to write error message");
            return;
        }
    };

    match (&ret, method_entry) {
        // Case 1: Return value in a void function (Illegal)
        (Some(_), Type::Void) => {
            writeln!(
                writer,
                "{}:{}:{}: ERROR: Return statement with a value in a void function.",
                filename, span.sline, span.scol
            ).expect("Failed to write error message");
        }

        // Case 2: No return value in a function that requires one (Illegal)
        (None, return_type) if return_type != Type::Void => {
            writeln!(
                writer,
                "{}:{}:{}: ERROR: Missing return value in function returning '{:#?}'.",
                filename, span.sline, span.scol, return_type
            ).expect("Failed to write error message");
        }

        // Case 3: Return type mismatch (Illegal)
        (Some(expr), return_type) => {
            let expr_type = infer_expr_type(expr, scope);
            if expr_type != return_type {
                writeln!(
                    writer,
                    "{}:{}:{}: ERROR: Return type mismatch. Expected '{:#?}', found '{:#?}'.",
                    filename, span.sline, span.scol, return_type, expr_type
                ).expect("Failed to write error message");
            }
        }

        _ => {println!("this ret ok");} // Valid return statement
    }
}


fn check_array_size(size: &str, span: &Span, filename: &str, writer: &mut dyn std::io::Write) {
    let is_valid = if let Some(stripped) = size.strip_prefix("0x") {
        usize::from_str_radix(stripped, 16).is_ok()
    } else {
        size.parse::<usize>().is_ok()
    };

    if !is_valid {
        writeln!(
            writer,
            "{}",
            format_error_message(size, Some(span), filename, "Invalid array size:")
        )
        .expect("Failed to write error message");
    }
}


/// Perform all semantic checks not already performed
/// during SymTree construction.
pub fn check_semantics(sym_tree: SymProgram, filename: &str, writer: &mut dyn std::io::Write) {

}

/// Semantically check the given file by parsing it and 
/// turning the AST into a symbol table tree.
pub fn semcheck(file: &str, filename: &str, writer: &mut dyn std::io::Write, verbose: bool) {
    let parse_tree: AST = parse(file, filename, writer, false).expect("Parsing failed");
    let sym_tree: SymProgram = build_symbol_table(&parse_tree, filename, writer);

    if verbose {
        println!("succesffully built symbol table!!!!!");
        println!("=================SYMBOL TABLE====================");
        print_symtree(&sym_tree);
    }

    // check_program(&sym_tree);
}
