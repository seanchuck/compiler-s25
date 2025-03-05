/*
Perform semantic checks on the AST produced by parsing.
Build a symbol table to enable these checks.

Some of the 23 semantic checks are performed while building the 
symbol table, and others are performed explicitly after.

Use-before-declaration is caught by the grammar/parser.

This semantic chcecker was written by :
    1. Constructing a modified AST with symbol tables and scopes
    2. Writing functions to check each rule and "injecting"
        them into the construction of the modified AST.


Be careful with how you pass Scope:
    - Use Rc<Refcell<Scope>> for shared, mutable references.
- Use &Scope for read-only references.

TODO: AVOID MULTIPLE MESSAGES FOR SAME ERROR
TODO: DON"T CREATE THE NODE IF THE CHECKS FAIL

TODO: rules 12 - 14
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
// HELPERS -- These are super important!
// #################################################

/// Formats an error message for printing to stdout.
/// Also flags that an error has occurred, so we can
/// return a non-zero exist code once checking is finished.
fn format_error_message(invalid_token: &str, span: Option<&Span>, msg: &str, context: &mut SemanticContext) -> String {
    // direct checker to panic after completing semantic checks
    context.error_found = true;

    match span {
        Some(span) => format!(
            "~~~{} (line {}, col {}): semantic error:\n|\t{} `{}`",
            context.filename, span.sline, span.scol, msg, invalid_token
        ),
        None => format!(
            "~~~{}: semantic error: \n|\t{} `{}`",
            context.filename, msg, invalid_token
        ),
    }
}

/// Infer the type of an expression from the given scope
/// The legal types are: Int, Long, Bool, Void, Unknown.
fn infer_expr_type(expr: &AST, scope: &Scope) -> Type {
    match expr {
        // Integer, Boolean, and Long Literals
        AST::Expr(Expr::Literal { lit, .. }) => match lit {
            Literal::Int(_) => Type::Int,
            Literal::Bool(_) => Type::Bool,
            Literal::Long(_) => Type::Long,
            _=> Type::Unknown
        },

        // Variable Reference (Check scope table)
        AST::Identifier { id, .. } => {
            let entry = scope.lookup(id);
            match entry {
                Some(TableEntry::Variable { typ, .. }) => typ.clone(),
                Some(_) => Type::Unknown, // Should never happen, but defensive
                None => {
                    println!("DEBUG: Variable `{}` not found in scope!", id);
                    Type::Unknown
                }
            }
        }        

        // Binary Expressions (`+`, `-`, `*`, `/`, `%`, etc.): evaluate recursively
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
        }
        

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
                // CHECK: array access must be type array
                Some(TableEntry::Variable { typ, is_array, .. }) if is_array => typ.clone(),
                Some(_) => Type::Unknown, // Non-array variable used incorrectly
                None => Type::Unknown, // Variable not declared
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
// AST --> SYMBOL TABLE AST CONSTRUCTION
// #################################################
// Some of the semantic checks are also performed 
// During this phase

/// Build the symbol-table-augmented version of the AST
pub fn build_symbol_table(
    ast: &AST,
    writer: &mut dyn std::io::Write,
    context: &mut SemanticContext
) -> SymProgram {
    match ast {
        AST::Program {
            imports,
            fields,
            methods,
            span,
        } => {

            // Create the global scope for the program, with no parent or enclosing methods
            let global_scope = Rc::new(RefCell::new(Scope::new()));
            let mut method_bodies = HashMap::new();

            // RULE 1: No duplicate imports
            check_duplicate_imports(imports, writer, context);
            check_main_exists(methods, writer, context);

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

                                    // must have static, legal size
                                    check_array_size(size, span, writer, context);

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
                        // println!("serint method{}", name);
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
                        // println!("method table is now: {:#?}", global_scope);
                        
                        // Now process the function body
                        let method = build_method(method, Rc::clone(&global_scope), writer, context);

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
    writer: &mut dyn std::io::Write,
    context: &mut SemanticContext
) -> SymMethod {
    match method {
        AST::MethodDecl {
            return_type,
            name,
            params,
            block,
            span,
        } => {

            // Create a new scope for this method, with the appropriate parent scope and enclosing method
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
                AST::Block { .. } => build_block(block, Rc::clone(&method_scope), writer, context),
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
    writer: &mut dyn std::io::Write,
    context: &mut SemanticContext
) -> SymBlock {
    match block {
        AST::Block {
            field_decls,
            statements,
            span,
        } => {
            // Don't create a new scope if this is just the outer method body (void main(){})
            let is_function_body = parent_scope.borrow().enclosing_method.is_some();

            let scope = if is_function_body {
                Rc::clone(&parent_scope) // get a reference to the same scope
            } else {
                // Do create new scope if this is within method body (Void main(){ if(){} })
                // and inherit the enclosing method
                Rc::new(RefCell::new(Scope::add_child(
                    Rc::clone(&parent_scope),
                    parent_scope.borrow().enclosing_method.clone(), 
                )))
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
                                    check_array_size(size, span, writer, context);

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
                let sym_stmt = build_statement(stmt, Rc::clone(&scope), writer, context);
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
    writer: &mut dyn std::io::Write,
    context: &mut SemanticContext
) -> SymStatement {
    // println!("building statement: {:#?}", statement);
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
                    check_used_before_decl(id, scope.clone(), span, writer, context);
                    let entry = scope.borrow().lookup(id).expect("Variable should be declared");
                
                    SymStatement::Assignment {
                        target: SymExpr::Identifier {
                            entry: entry.clone(),  // ✅ Store full TableEntry info instead of just `id`
                            span: id_span.clone(),
                        },
                        expr: build_expr(expr, Rc::clone(&scope), writer, context),
                        span: span.clone(),
                    }
                },

                // Array access assignment (x[2] = 5;)
                AST::Expr(Expr::ArrAccess {
                    id,
                    index,
                    span: arr_span,
                }) => {
                    SymStatement::Assignment {
                        target: build_expr(location, Rc::clone(&scope), writer, context),
                        expr: build_expr(expr, Rc::clone(&scope), writer, context),
                        span: span.clone(),
                    }
                }
                _ => panic!(
                    "Unsupported assignment target in AST at line {}, column {}: {:#?}",
                    span.sline, span.scol, statement
                ),
            }
        },

        AST::Statement(Statement::MethodCall {
            method_name,
            args,
            span,}) => {

            check_methodcall(&method_name, &args, span, scope.clone(), false, writer, context);

            SymStatement::MethodCall {
            method_name: method_name.clone(),
            args: args
                .iter()
                .map(|arg| build_expr(arg, Rc::clone(&scope), writer, context))
                .collect(),
            span: span.clone(),
            }
        },

        AST::Statement(Statement::If {
            condition,
            then_block,
            else_block,
            span,
        }) => {
            check_is_bool(condition, span, scope.clone(), writer, context);


            SymStatement::If {
                condition: build_expr(condition, Rc::clone(&scope), writer, context),
                then_block: Rc::new(build_block(then_block, Rc::clone(&scope), writer, context)),
                else_block: else_block
                    .as_ref()
                    .map(|blk| Rc::new(build_block(blk, Rc::clone(&scope), writer, context))),
                span: span.clone(),
        }
    }
    

        AST::Statement(Statement::While {
            condition,
            block,
            span,
        }) => {
            check_is_bool(condition, span, scope.clone(), writer, context);

            SymStatement::While {
                condition: build_expr(condition, Rc::clone(&scope), writer, context),
                block: Rc::new(build_block(block, Rc::clone(&scope), writer, context)),
                span: span.clone(),
            }
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

            check_is_bool(condition, span, scope.clone(), writer, context);

            let update_expr = match update.as_ref() {
                AST::Statement(Statement::Assignment { location, expr, .. }) => {

                    if let AST::Identifier { .. } = location.as_ref() {
                        build_expr(expr, Rc::clone(&scope), writer, context)
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
                init: build_expr(init, Rc::clone(&scope), writer, context),
                condition: build_expr(condition, Rc::clone(&scope), writer, context),
                update: update_expr,
                block: Rc::new(build_block(block, Rc::clone(&scope), writer, context)),
                span: span.clone(),
            }
        }

        AST::Statement(Statement::Return { expr, span }) => {
            check_return_value(expr.as_ref(), &scope.borrow(), span, writer, context);
            SymStatement::Return {
            expr: expr
                .as_ref()
                .map(|e| build_expr(e, Rc::clone(&scope), writer, context)),
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
    writer: &mut dyn std::io::Write,
    context: &mut SemanticContext
) -> SymExpr {
    match expr {
        AST::Expr(Expr::BinaryExpr {
            op,
            left,
            right,
            span,
        }) => {
            let left_expr = Rc::new(build_expr(left, Rc::clone(&scope), writer, context));
            let right_expr = Rc::new(build_expr(right, Rc::clone(&scope), writer, context));

            let left_type = infer_expr_type(left, &scope.borrow());
            let right_type = infer_expr_type(right, &scope.borrow());
            let mut result_type = Type::Unknown; // Set result type based on operator

            match *op {
                BinaryOp::Add
                | BinaryOp::Subtract
                | BinaryOp::Multiply
                | BinaryOp::Divide
                | BinaryOp::Modulo => {
                    // Rule 14: Must be (1) numeric, (2) left and right have same type
                    // check_is_numeric_and_compatible(true, left, Some(right), span, scope.clone(), writer, context);
                    result_type = left_type; // If valid, set type to operand type
                }

                // Operands must have numeric
                BinaryOp::Greater
                | BinaryOp::GreaterEqual
                | BinaryOp::Less
                | BinaryOp::LessEqual => {
                    check_is_numeric_and_compatible(false, left, Some(right), span, scope.clone(), writer, context);
                    result_type = Type::Bool;
                }

                // Equality operators always return bool
                BinaryOp::Equal
                | BinaryOp::NotEqual => {
                    check_equality_compatible(left, right, span, scope.clone(), writer, context);
                    result_type = Type::Bool;
                }

                // Logical operators (&&, ||) require bool operands and return bool
                // BinaryOp::And
                // | BinaryOp::Or => {
                //     if left_type == Type::Bool && right_type == Type::Bool {
                //         result_type = Type::Bool;
                //     } else {
                //         writeln!(
                //             writer,
                //             "{}",
                //             format_error_message(
                //                 &format!("Operands `{:#?}` and `{:#?}`", left, right),
                //                 Some(span),
                //                 "Logical operators require `bool` operands.",
                //                 context
                //             )
                //         ).expect("Failed to write error message");
                //     }
                // }

                _ => {}
            }
            SymExpr::BinaryExpr {
                op: op.clone(),
                left: left_expr,
                right: right_expr,
                typ: result_type, // Set the correct type
                span: span.clone(),
            }
        }

        AST::Expr(Expr::UnaryExpr { op, expr, span }) => {
            // Rule 14: Unary minus must have numeric type
            // match *op {
            //     UnaryOp::Neg => {check_is_numeric_and_compatible(false, expr, None, span, scope.clone(), writer, context);}
            //     _=>{}
            // }
    
            SymExpr::UnaryExpr {
                op: op.clone(),
                expr: Rc::new(build_expr(expr, Rc::clone(&scope), writer, context)),
                typ: Type::Int,
                span: span.clone(),
            }
        }

        AST::Expr(Expr::MethodCall {
            method_name,
            args,
            span,
        }) => {
            check_methodcall(&method_name, args, span, scope.clone(), true, writer, context);

            SymExpr::MethodCall {
            method_name: method_name.clone(),
            args: args
                .iter()
                .map(|arg| Rc::new(build_expr(arg, Rc::clone(&scope), writer, context)))
                .collect(),
            span: span.clone(),
            }
        },

        AST::Expr(Expr::ArrAccess { id, index, span }) => {
            if let Some(entry) = scope.borrow().lookup(id).map(|e| e.clone()) {  // ✅ Manually cloning the entry
                if let TableEntry::Variable { is_array, .. } = entry {
                    check_arraccess(is_array, index, &scope, span, writer, context);
                }
            }
            
            // Still build this on failure?
            SymExpr::ArrAccess {
                id: id.clone(),
                index: Rc::new(build_expr(index, Rc::clone(&scope), writer, context)),
                span: span.clone(),
            }
        },

        AST::Expr(Expr::Len { id, span }) => {
            if check_len_argument(id, span, &scope.borrow(), writer, context) {
                if let AST::Identifier { id, .. } = id.as_ref() {
                    SymExpr::Len {
                        id: id.clone(),
                        span: span.clone(),
                    } 
                } else {
                        SymExpr::Error { span: span.clone() }
                    }
            } else {
                // ❌ If invalid, return an error node
                SymExpr::Error { span: span.clone() }
            }
        }
        
        AST::Expr(Expr::Cast {
            target_type,
            expr,
            span,
        }) => SymExpr::Cast {
            target_type: target_type.clone(),
            expr: Rc::new(build_expr(expr, Rc::clone(&scope), writer, context)),
            span: span.clone(),
        },

        AST::Expr(Expr::Literal { lit, span }) => SymExpr::Literal {
            value: lit.clone(),
            span: span.clone(),
        },

        AST::Identifier { ref id, ref span } => {
            // RULE 2: no identifier is used before being declared
            check_used_before_decl(id, Rc::clone(&scope), span, writer, context);

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
// SEMANTIC CHECKING
// #################################################
// There are 23 semantic rules we need to follow
// Some are checked while the SymTree is being made.
// Others are checked later explicitly.

/// RULE 1
fn check_duplicate_imports(imports: &[Box<AST>],writer: &mut dyn std::io::Write, context: &mut SemanticContext) {
    let mut seen = HashSet::new();

    for import in imports {
        if !seen.insert(import) {
            match **import {
                AST::ImportDecl { ref id, ref span } => {
                    writeln!(
                        writer,
                        "{}",
                        format_error_message(id, Some(span), "duplicate import", context)
                    )
                    .expect("Failed to write output!");
                }
                _ => unreachable!(),
            }
        }
    }
}

/// RULE 2, 9
fn check_used_before_decl(
    id: &str,
    scope: Rc<RefCell<Scope>>,
    span: &Span,
    writer: &mut dyn std::io::Write,
    context: &mut SemanticContext
) {
    let scope_ref = scope.borrow();

    if scope_ref.lookup(id).is_none() {
        writeln!(
            writer,
            "{}",
            format_error_message(id, Some(span), "Use before declaration", context)
        )
        .expect("Failed to write output!");
    }
}

/// RULE 3
fn check_main_exists(methods: &Vec<Box<AST>>, writer: &mut dyn std::io::Write, context: &mut SemanticContext) {
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
        let error_msg = format_error_message("main", None, "Missing entry point:", context);
        writeln!(writer, "{}", error_msg).expect("Failed to write error message");
    }
}

/// Rules 4-6, 10
fn check_methodcall(
    method_name: &String,
    args: &Vec<Box<AST>>, // Now considering actual arguments
    span: &Span,
    scope: Rc<RefCell<Scope>>,
    is_expr: bool,
    writer: &mut dyn std::io::Write,
    context: &mut SemanticContext
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
                    &format!(
                        "Incorrect number of arguments for method. Expected {}, but got {}.",
                        params.len(),
                        args.len()
                    ),
                    context
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
                        &format!(
                            "Type mismatch for parameter `{}`. Expected `{:?}`, but got `{:?}`.",
                            param_name, expected_type, arg_type
                        ),
                        context
                    );
                    writeln!(writer, "{}", err_msg).unwrap();
                }
            }

            // Check if method returns a value when used in an expression
            if is_expr && matches!(return_type, Type::Void) {
                let err_msg = format_error_message(
                    method_name,
                    Some(&span),
                    "Method does not return a value but is used in an expression.",
                    context
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
                "Variable declaration shadows method declaration",
                context
            );
            writeln!(writer, "{}", err_msg).unwrap();

        },

        // Method is not found in the scope → Report error
        None => {
            let err_msg = format_error_message(
                method_name,
                Some(&span),
                "Call to undefined method.",
                context
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
    writer: &mut dyn std::io::Write,
    context: &mut SemanticContext,
) {
    // Ensure return statement is inside a function
    let method_name = match &scope.enclosing_method {
        Some(name) => name.clone(),
        None => {
            println!("no enclosing scope");
            let error_msg = format_error_message(
                &format!("{:#?}", ret), 
                Some(span),
                "Return statement outside of a function.",
                context
            );
            writer.write_all(error_msg.as_bytes()).expect("Failed to write error message");
            return;
        }
    };

    // Retrieve method return type from symbol table
    let method_return_type = match scope.lookup(&method_name) {
        Some(TableEntry::Method { return_type, .. }) => return_type.clone(),
        _ => {
            let error_msg = format_error_message(
                &format!("{:#} ret", method_name),
                Some(span),
                &format!("Could not find method '{}' in symbol table.", method_name),
                context
            );
            writer.write_all(error_msg.as_bytes()).expect("Failed to write error message");
            return;
        }
    };

    match (&ret, method_return_type) {
        // Rule 7: Return statement with a value in a void function (Illegal)
        (Some(expr), Type::Void) => {
            let expr_str = format!("{:#?} ret", expr);  // ✅ ID format
            let error_msg = format_error_message(
                &expr_str,
                Some(span),
                "Return statement with a value in a function declared to return void.",
                context
            );
            writeln!(writer, "{}", error_msg).expect("Failed to write output!");
        }
        
        // Rule 7: Missing return value in a function that requires one (Illegal)
        (None, return_type) if return_type != Type::Void => {
            let error_msg = format_error_message(
                "{:#} ret", 
                Some(span),
                &format!("Missing return value in function returning '{:#?}'.", return_type),
                context
            );
            writeln!(writer, "{}", error_msg).expect("Failed to write output!");
        }
        
        // Rule 8: Return type mismatch (Illegal)
        (Some(expr), expected_type) => {
            let actual_type = infer_expr_type(expr, scope);
            if actual_type != expected_type {
                let expr_str = format!("{:#?} ret", expr);  // ✅ ID format
                let error_msg = format_error_message(
                    &expr_str, 
                    Some(span),
                    &format!(
                        "Return type mismatch. Expected '{:#?}', but found '{:#?}'.",
                        expected_type, actual_type
                    ),
                    context
                );
                writeln!(writer, "{}", error_msg).expect("Failed to write output!");
            }
        }
        
        // Valid case: No return value in a void function or correct return type
        _ => {}
    }
}



/// Rule 11
fn check_arraccess(
    is_array: bool,
    array_index: &Box<AST>,
    scope: &Rc<RefCell<Scope>>,
    span: &Span,
    writer: &mut dyn std::io::Write,
    context: &mut SemanticContext,
) {
    // Rule 11(a): Ensure that the identifier is an array
    if !is_array {
        let error_msg = format_error_message(
            format!("{:#?}", array_index).as_str(),
            Some(span),
            "Identifier must be an array variable.",
            context,
        );
        writeln!(writer, "{}", error_msg).unwrap();
        return;
    }

    // Rule 11(b): Ensure that the index expression evaluates to an int
    let index_type = infer_expr_type(array_index, &scope.borrow());
    if index_type != Type::Int {
        let error_msg = format_error_message(
            format!("{:#?}", array_index).as_str(),
            Some(span),
            "Array index must be of type int.",
            context,
        );
        writeln!(writer, "{}", error_msg).unwrap();
    }
}

/// Rule 12
fn check_len_argument(id: &AST, span: &Span, scope: &Scope, writer: &mut dyn std::io::Write, context: &mut SemanticContext)-> bool{
    if let AST::Identifier { id, .. } = id {
        if let Some(entry) = scope.lookup(id) {
            match entry {
                TableEntry::Variable { is_array: true, .. } => {
                    return true
                    // ✅ Valid: `len` is used on an array
                }
                _ => {
                    // ❌ Error: `len` called on a non-array variable
                    writeln!(
                        writer,
                        "{}",
                        format_error_message(
                            id,
                            Some(span),
                            "Invalid use of `len`: argument must be an array variable.",
                            context
                        )
                    ).expect("Failed to write error message");
                }
            }
        } else {
            // ❌ Error: `len` called on an undefined variable
            writeln!(
                writer,
                "{}",
                format_error_message(
                    id,
                    Some(span),
                    "Undefined variable used in `len` expression.",
                    context
                )
            ).expect("Failed to write error message");
        }
    } else {
        // len` called with an invalid argument (must be an identifier)
        writeln!(
            writer,
            "{}",
            format_error_message(
                &format!("{:?}", id),
                Some(span),
                "Invalid argument to `len`: expected an array identifier.",
                context
            )
        ).expect("Failed to write error message");
    }
    false
}


/// Rule 13
fn check_is_bool(
    condition: &AST,
    span: &Span,
    scope: Rc<RefCell<Scope>>,
    writer: &mut dyn std::io::Write,
    context: &mut SemanticContext,
) {
    // if, while, bool expressions must evaluate to type bool
    let inferred_type = infer_expr_type(condition, &scope.borrow());

    if inferred_type != Type::Bool {
        writeln!(
            writer,
            "{}",
            format_error_message(
                format!("{:#?}", condition).as_str(),
                Some(span),
                "Condition must have type bool",
                context
            )
        )
        .expect("Failed to write error message");
    }
}


fn check_array_size(size: &str, span: &Span, writer: &mut dyn std::io::Write, context: &mut SemanticContext) {
    let is_valid = if let Some(stripped) = size.strip_prefix("0x") {
        usize::from_str_radix(stripped, 16).is_ok()
    } else {
        size.parse::<usize>().is_ok()
    };

    if !is_valid {
        writeln!(
            writer,
            "{}",
            format_error_message(size, Some(span), "Invalid array size:", context)
        )
        .expect("Failed to write error message");
    }
}

//// Rule 14
fn check_is_numeric_and_compatible(
    is_arithmetic: bool,
    left: &AST,
    right: Option<&AST>,  // Right is `None` for unary `Neg`
    span: &Span,
    scope: Rc<RefCell<Scope>>,
    writer: &mut dyn std::io::Write,
    context: &mut SemanticContext,
) {
    let left_type = infer_expr_type(left, &scope.borrow());

    // 🔥 **CHANGE 1: Special Handling for Unary Expressions**
    if right.is_none() { 
        if left_type == Type::Int || left_type == Type::Long {
            return; // ✅ Valid unary `Neg`
        } else {
            writeln!(
                writer,
                "{}",
                format_error_message(
                    &format!("Unary operand `{:#?}`", left),
                    Some(span),
                    "Unary `-` must be applied to `int` or `long`.",
                    context
                )
            ).expect("Failed to write error message");
            return;
        }
    }

    // ✅ Original check remains for binary expressions:
    let right_type = right.map(|r| infer_expr_type(r, &scope.borrow()));

    // 🔥 **CHANGE 2: Allowing `None` for Unary Cases**
    if let Some(right_type) = right_type {
        if is_arithmetic && left_type != right_type {
            writeln!(
                writer,
                "{}",
                format_error_message(
                    &format!("left `{:#?}`, right `{:#?}`", left, right),
                    Some(span),
                    "Operands of arithmetic expressions must have the same type.",
                    context
                )
            ).expect("Failed to write error message");
        }
    }
}




// Rule 15, 16: Ensure equality operators (`==`, `!=`) have compatible types
fn check_equality_compatible(
    left: &AST,
    right: &AST,
    span: &Span,
    scope: Rc<RefCell<Scope>>,
    writer: &mut dyn std::io::Write,
    context: &mut SemanticContext,
) {
    let left_type = infer_expr_type(left, &scope.borrow());
    let right_type = infer_expr_type(right, &scope.borrow());

    // Allowable types: `int`, `long`, `bool`
    if left_type != right_type {
        writeln!(
            writer,
            "{}",
            format_error_message(
                &format!("left `{:#?}`, right `{:#?}`", left, right),
                Some(span),
                "Equality operator requires both operands to have the same type.",
                context
            )
        )
        .expect("Failed to write error message");
        return;
    }

    if left_type != Type::Int && left_type != Type::Long && left_type != Type::Bool {
        writeln!(
            writer,
            "{}",
            format_error_message(
                &format!("left `{:#?}`, right `{:#?}`", left, right),
                Some(span),
                "Equality operator can only be applied to int, long, or bool.",
                context
            )
        )
        .expect("Failed to write error message");
    }
}




// #################################################
// ENTRY POINT
// #################################################


/// Perform all semantic checks not already performed
/// during SymTree construction.
pub fn check_semantics(sym_tree: SymProgram, filename: &str, writer: &mut dyn std::io::Write) {

}

/// Semantically check the given file by parsing it and 
/// turning the AST into a symbol table tree.
pub fn semcheck(file: &str, filename: &str, writer: &mut dyn std::io::Write, verbose: bool) {
    let parse_tree: AST = parse(file, filename, writer, false).expect("Parsing failed");

    // Package semantic context
    let mut context = SemanticContext {
        filename: filename.to_string(),
        error_found: false
    };

    // Build the semantic tree
    let sym_tree: SymProgram = build_symbol_table(&parse_tree, writer, &mut context);

    if verbose {
        println!("Successfully built symbol table!");
        println!("=================SYMBOL TABLE====================");
        print_symtree(&sym_tree);
    }

    // Panic if any semantic errors were found
    if context.error_found {
        panic!("Semantic check failed.");
    }
}