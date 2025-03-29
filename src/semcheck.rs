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

*/

use core::panic;
use std::cell::RefCell;
use std::collections::HashMap;
use std::collections::HashSet;
use std::rc::Rc;


use crate::ast::*;
use crate::parse::parse;
use crate::scope::{Scope, EnclosingBlock, TableEntry};
use crate::symtable::*;
use crate::token::Literal;
use crate::token::Span;
use crate::traverse::traverse_ir;
use crate::utils::print::*;


// #################################################
// HELPERS (these are vital, since so many rules are typechecking)
// #################################################

/// Formats an error message for printing to stdout.
/// Also flags that an error has occurred, so we can
/// return a non-zero exist code once checking is finished.
pub fn format_error_message(invalid_token: &str, span: Option<&Span>, msg: &str, context: &mut SemanticContext) -> String {
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

pub fn format_error_message0(span: Option<&Span>, msg: &str, context: &mut SemanticContext) -> String {
    // direct checker to panic after completing semantic checks
    context.error_found = true;

    match span {
        Some(span) => format!(
            "~~~{} (line {}, col {}): semantic error:\n|\t{}",
            context.filename, span.sline, span.scol, msg
        ),
        None => format!(
            "~~~{}: semantic error: \n|\t{}",
            context.filename, msg
        ),
    }
}

pub fn format_error_message2(invalid_token1: &str, invalid_token2: &str, span: Option<&Span>, msg: &str, context: &mut SemanticContext) -> String {
    // direct checker to panic after completing semantic checks
    context.error_found = true;

    match span {
        Some(span) => format!(
            "~~~{} (line {}, col {}): semantic error:\n|\t{}: `{}` and `{}`",
            context.filename, span.sline, span.scol, msg, invalid_token1, invalid_token2
        ),
        None => format!(
            "~~~{}: semantic error: \n|\t{}: `{}` and `{}`",
            context.filename, msg, invalid_token1, invalid_token2
        ),
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
                        span: _
                    } => {
                        for decl in decls {
                            match **decl {
                                // scalar variable declaration
                                AST::Identifier { ref id, ref span } => {
                                    if global_scope.borrow_mut().insert(
                                        id.clone(),
                                        TableEntry::Variable {
                                            name: id.clone(),
                                            typ: typ.clone(),
                                            length: None,
                                            span: span.clone(),
                                        },
                                    ).is_some() { // rule 1
                                        writeln!(
                                            writer,
                                            "{}",
                                            format_error_message(id, Some(span), "duplicate field declaration", context)
                                        )
                                        .expect("Failed to write output!");
                                    }
                                }

                                // array variable declaration
                                AST::ArrayFieldDecl {
                                    ref id,
                                    ref size,
                                    ref span,
                                } => {
                                    check_and_insert_array_field(id, size, span, typ, &global_scope, writer, context);
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
                        block: _,
                        ref span,
                    } => {
                        // Insert function into table before processing body
                        if global_scope.borrow_mut().insert(
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
                        ).is_some() { // rule 1
                            writeln!(
                                writer,
                                "{}",
                                format_error_message(name, Some(span), "duplicate method declaration", context)
                            )
                            .expect("Failed to write output!");
                        }
                        
                        // Now process the function body
                        let method: SymMethod = build_method(method, Rc::clone(&global_scope), writer, context);

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
            //   Create a new scope for this method, with `enclosing_block` set to Method
            let method_scope = Rc::new(RefCell::new(Scope::add_child(
                Rc::clone(&parent_scope),
                Some(EnclosingBlock::Method),
            )));
            
            // Process method params, and add into method's outer-most scope
            for param in params {
                match param.name.as_ref() {
                    AST::Identifier { id, span } => {
                        if method_scope.borrow_mut().insert(
                            id.clone(),
                            TableEntry::Variable {
                                name: id.clone(),
                                typ: param.typ.clone(),
                                length: None,
                                span: span.clone(),
                            },
                        ).is_some() {
                            writeln!(
                                writer,
                                "{}",
                                format_error_message(id, Some(span), "duplicate parameter name", context)
                            )
                            .expect("Failed to write output!");
                        }
                    }
                    _ => unreachable!(),
                }
            }

            // Process method body
            let method_body = match **block {
                AST::Block { .. } => build_block(block, Rc::clone(&method_scope), writer, context, Some(Rc::clone(&method_scope))),
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


/// Builds an IR representation of a block. If overwrite scope defined, will use given scope
/// instead of making a new one
pub fn build_block(
    block: &AST,
    parent_scope: Rc<RefCell<Scope>>,
    writer: &mut dyn std::io::Write,
    context: &mut SemanticContext,
    overwrite_scope: Option<Rc<RefCell<Scope>>>
) -> SymBlock {
    match block {
        AST::Block {
            field_decls,
            statements,
            span,
        } => {
            //  Detect if this block is inside a `for` or `while`
            let is_loop_body = matches!(
                parent_scope.borrow().enclosing_block,
                Some(EnclosingBlock::Loop)
            );

            let enclosing_block = if is_loop_body {
                Some(EnclosingBlock::Loop) //   Explicitly mark this block as a loop
            } else {
                parent_scope.borrow().enclosing_block.clone() //   Inherit from parent
            };

            let scope = if let Some(overwrite) = overwrite_scope {
                overwrite //   Overwrite scope if provided
            } else {
                Rc::new(RefCell::new(Scope::add_child(
                    Rc::clone(&parent_scope),
                    enclosing_block,
                )))
            };

            let mut sym_statements = Vec::new();

            // Block creates a new scope that can have its own field decls
            for field in field_decls {
                match **field {
                    AST::FieldDecl {
                        ref typ,
                        ref decls,
                        span: _
                    } => {
                        for decl in decls {
                            match **decl {
                                // Scalar field declaration
                                AST::Identifier { ref id, ref span } => {
                                    if scope.borrow_mut().insert(
                                        id.clone(),
                                        TableEntry::Variable {
                                            name: id.clone(),
                                            typ: typ.clone(),
                                            length: None,
                                            span: span.clone(),
                                        },
                                    ).is_some() {
                                        writeln!(
                                            writer,
                                            "{}",
                                            format_error_message(id, Some(span), "duplicate field declaration", context)
                                        )
                                        .expect("Failed to write output!");
                                    }

                                    sym_statements.push(Rc::new(SymStatement::VarDecl {
                                        name: id.clone(),
                                        typ: typ.clone(),
                                        length: None,
                                        span: span.clone(),
                                    }));
                                }

                                // Array field declaration
                                AST::ArrayFieldDecl {
                                    ref id,
                                    ref size,
                                    ref span,
                                } => {
                                    check_and_insert_array_field(id, size, span, typ, &scope, writer, context);

                                    sym_statements.push(Rc::new(SymStatement::VarDecl {
                                        name: id.clone(),
                                        typ: typ.clone(),
                                        length: Some(size.clone()),
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
    match statement {
        AST::Statement(Statement::Assignment {
            location,
            expr,
            op,
            span,
        }) => {
            match location.as_ref() {
                // Plain variable assignment (x = 3;)
                AST::Identifier { id, span: id_span } => {
                    check_used_before_decl(id, scope.clone(), span, writer, context);

                    if let Some(entry) = scope.borrow().lookup(id) {
                        SymStatement::Assignment {
                            target: SymExpr::Identifier {
                                entry: entry.clone(),
                                span: id_span.clone(),
                            },
                            expr: build_expr(expr, Rc::clone(&scope), writer, context),
                            span: span.clone(),
                            op: op.clone()
                        }

                    } else {
                        SymStatement::Error
                    }
                },

                // Array access assignment (x[2] = 5;)
                AST::Expr(Expr::ArrAccess {
                    id,
                    index: _,
                    span
                }) => {
                    check_used_before_decl(id, scope.clone(), span, writer, context);

                    SymStatement::Assignment {
                        target: build_expr(location, Rc::clone(&scope), writer, context),
                        expr: build_expr(expr, Rc::clone(&scope), writer, context),
                        span: span.clone(),
                        op: op.clone()
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

                check_used_before_decl(method_name, scope.clone(), span, writer, context);

                SymStatement::MethodCall {
                    method_name: method_name.clone(),
                    args: args
                        .iter()
                        .map(|arg| build_expr(arg, scope.clone(), writer, context)) //   Use `scope_clone.clone()` here
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
            SymStatement::If {
                condition: build_expr(condition, Rc::clone(&scope), writer, context),
                then_block: Rc::new(build_block(then_block, Rc::clone(&scope), writer, context, None)),
                else_block: else_block
                    .as_ref()
                    .map(|blk| Rc::new(build_block(blk, Rc::clone(&scope), writer, context, None))),
                span: span.clone(),
        }
    }
    

        AST::Statement(Statement::While {
            condition,
            block,
            span,
        }) => {
            SymStatement::While {
                condition: build_expr(condition, Rc::clone(&scope), writer, context),
                block: Rc::new(build_block(block, Rc::clone(&scope), writer, context, None)),
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
            let mut var_typ: Type = Type::Int;
            if let Some(var_entry) = scope.borrow().lookup(var){
                match var_entry {
                    TableEntry::Variable { typ, span, .. } => {
                        match typ {
                            Type::Int | Type::Long => {
                                var_typ = typ;
                            }
                            _ => {
                                writeln!(
                                    writer,
                                    "{}",
                                    format_error_message(var, Some(&span), "For loop variable must be int or long", context)
                                )
                                .expect("Failed to write output!");
                            }
                        }
                    }
                    _ => {
                        writeln!(
                            writer,
                            "{}",
                            format_error_message(var, Some(span), "For loop variable must be a table entry variable", context)
                        )
                        .expect("Failed to write output!");
                    }
                }
            } else {
                writeln!(
                    writer,
                    "{}",
                    format_error_message(var, Some(span), "For loop variable not defined in scope", context)    // TODO: Check if necessary to check here
                )
                .expect("Failed to write output!");
            }
            if scope.borrow_mut().insert(
                var.clone(),
                TableEntry::Variable {
                    name: var.clone(),
                    typ: var_typ, // TODO: Determine type dynamically
                    length: None,
                    span: span.clone(),
                },
            ).is_none() {
                writeln!(
                    writer,
                    "{}",
                    format_error_message(var, Some(span), "for loop variable not declared", context)
                )
                .expect("Failed to write output!");
            }

            let update_expr = match update.as_ref() {
                AST::Statement(Statement::Assignment { location, .. }) => {

                    if let AST::Identifier { .. } = location.as_ref() {
                        build_statement(&update, Rc::clone(&scope), writer, context)
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
                update: Box::new(update_expr),
                block: Rc::new(build_block(block, Rc::clone(&scope), writer, context, None)),
                span: span.clone(),
            }
        }

        AST::Statement(Statement::Return { expr, span }) => {
            SymStatement::Return {
            expr: expr
                .as_ref()
                .map(|e| build_expr(e, Rc::clone(&scope), writer, context)),
            span: span.clone(),
            }
        }

        AST::Statement(Statement::Break { span }) => {
            // check_in_loop(scope.clone(), span, writer, context);
            SymStatement::Break { span: span.clone() }

        }

        AST::Statement(Statement::Continue { span }) => {
            // check_in_loop(scope.clone(), span, writer, context);
            SymStatement::Continue { span: span.clone()}
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

            SymExpr::BinaryExpr {
                op: op.clone(),
                left: left_expr,
                right: right_expr,
                span: span.clone(),
            }
        }

        AST::Expr(Expr::UnaryExpr { op, expr, span }) => {        
            match (op, &**expr) {
                // Fold `-` applied to an integer or long literal
                (UnaryOp::Neg, AST::Expr(Expr::Literal { lit, .. } )) => {                    
                    return SymExpr::Literal {
                        value: match lit {
                            Literal::Int(value) => Literal::Int(("-".to_owned() + value).to_string()),
                            Literal::Long(value) => Literal::Long(("-".to_owned() + value).to_string()),
                            Literal::HexInt(value) => Literal::HexInt(("-".to_owned() + value).to_string()),
                            Literal::HexLong(value) => Literal::HexLong(("-".to_owned() + value).to_string()),
                            _ => panic!("should only match int or long")
                        },
                        span: span.clone(),
                    };
                }
        
                // Normal unary minus handling
                (UnaryOp::Neg, _) => {
                    return SymExpr::UnaryExpr {
                        op: op.clone(),
                        expr: Rc::new(build_expr(expr, Rc::clone(&scope), writer, context)),
                        span: span.clone(),
                    };
                }
        
                // Logical NOT (`!`)
                (UnaryOp::Not, _) => {
                    return SymExpr::UnaryExpr {
                        op: op.clone(),
                        expr: Rc::new(build_expr(expr, Rc::clone(&scope), writer, context)),
                        span: span.clone(),
                    };
                }
            }
        },
        

        AST::Expr(Expr::MethodCall {
            method_name,
            args,
            span,
        }) => {
            check_used_before_decl(method_name, scope.clone(), span, writer, context);

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
                // If invalid, return an error node
                SymExpr::Error { span: span.clone() }
            }
        }
        
        AST::Expr(Expr::Cast {
            target_type,
            expr,
            span,
        }) => {        
            SymExpr::Cast {
                target_type: target_type.clone(),
                expr: Rc::new(build_expr(expr, Rc::clone(&scope), writer, context)),
                span: span.clone(),
            }
        }

        AST::Expr(Expr::Literal { lit, span }) => SymExpr::Literal {
            value: lit.clone(),
            span: span.clone(),
        },
        
        AST::Identifier { ref id, ref span } => {
            // Clone scope first to avoid overlapping borrows
            let scope_clone: Rc<RefCell<Scope>> = Rc::clone(&scope);
            
            // RULE 2: no identifier is used before being declared
            check_used_before_decl(id, scope.clone(), span, writer, context); //   Immutable borrow ends here
        
            let entry = scope_clone.borrow().lookup(id).unwrap();
            SymExpr::Identifier {
                entry: entry.clone(),
                span: span.clone(),
            }
        },
        

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
        match **import {
            AST::ImportDecl { ref id, ref span } => {
                if !seen.insert(id.clone()) {
                    writeln!(
                        writer,
                        "{}",
                        format_error_message(id, Some(span), "duplicate import", context)
                    )
                    .expect("Failed to write output!");
                }
            }
            _ => unreachable!(),
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
            AST::MethodDecl { ref return_type, ref name, ref params, span, .. } => {
                if name == "main" {
                    has_main = true;

                    if *return_type != Type::Void {
                        let error_msg = format_error_message("void", Some(&span), "`main` must return", context);
                        writeln!(writer, "{}", error_msg).expect("Failed to write error message");
                    } else if !params.is_empty() {
                        let error_msg = format_error_message("main", Some(&span), "should not have any parameters: entry point", context);
                        writeln!(writer, "{}", error_msg).expect("Failed to write error message");
                    }
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

/// Rule 12
fn check_len_argument(id: &AST, span: &Span, scope: &Scope, writer: &mut dyn std::io::Write, context: &mut SemanticContext)-> bool{
    if let AST::Identifier { id, .. } = id {
        if let Some(entry) = scope.lookup(id) {
            match entry {
                TableEntry::Variable { length: Some(_), .. } => {
                    return true
                    // Valid: `len` is used on an array
                }
                _ => {
                    // Error: `len` called on a non-array variable
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
            // Error: `len` called on an undefined variable
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

// Array Size Rule
fn check_and_insert_array_field(
    id: &String,
    size: &Literal,
    span: &Span,
    typ: &Type,
    scope: &Rc<RefCell<Scope>>, // Accepts any scope
    writer: &mut dyn std::io::Write,
    context: &mut SemanticContext,
) {

    // Extract integer value from Literal
    let parsed_size = match size {
        Literal::Int(ref s) | Literal::HexInt(ref s) => {
            let parsed = s.parse::<i32>().ok();
            parsed
        }
        _ => {
            None
        }
    };

    // Check if size is <= 0 or invalid
    if let Some(size_value) = parsed_size {
        if size_value <= 0 {
            writeln!(
                writer,
                "{}",
                format_error_message(id, Some(span), "array field declaration must have size > 0", context)
            )
            .expect("Failed to write output!");
            return; // Exit early, no insertion into scope
        }
    } else {
        writeln!(
            writer,
            "{}",
            format_error_message(id, Some(span), "invalid array size type", context)
        )
        .expect("Failed to write output!");
        return;
    }

    // Insert variable into the given scope
    if scope.borrow_mut().insert(
        id.clone(),
        TableEntry::Variable {
            name: id.clone(),
            typ: typ.clone(),
            length: parsed_size,
            span: span.clone(),
        },
    ).is_some() { // rule 1
        writeln!(
            writer,
            "{}",
            format_error_message(id, Some(span), "duplicate field declaration", context)
        )
        .expect("Failed to write output!");
    }
}



// #################################################
// ENTRY POINT
// #################################################

/// Semantically check the given file by parsing it and 
/// turning the AST into a symbol table tree.
pub fn semcheck(file: &str, filename: &str, writer: &mut dyn std::io::Write, verbose: bool) -> SymProgram {
    // Parse the input file
    let parse_tree: AST = parse(file, filename, writer, false).expect("Parsing failed");

    // Package semantic context
    let mut context = SemanticContext {
        filename: filename.to_string(),
        error_found: false
    };

    // Build the semantic tree, performing initial semantic checks
    let sym_tree: SymProgram = build_symbol_table(&parse_tree, writer, &mut context);

    // Traverse tree, performing any remaining semantic checks
    traverse_ir(&sym_tree, writer, &mut context);

    if verbose {
        println!("Successfully built symbol table!");
        println!("=================SYMBOL TABLE====================");
        print_symtree(&sym_tree);
    }

    // Panic if any semantic errors were found
    if context.error_found {
        panic!("Semantic check failed.");
    }

    sym_tree
}