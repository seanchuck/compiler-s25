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


TODO: 
    - Debug / implement rules: 4, 5, 6, 7, 8, 10, 13, 14, 17, 18, 19, 23
        - After implementing, be sure to run on gradescope to ensure all 
            legal cases still pass
    - Clean up error messages
        - Don't create multiple messages for same error
        - Test that we can output multiple error messages for single pass

    - For range checking (int, long)
        - must check for literals: array declaration sizes, etc.
        - need to fold unary minus (see https://6110-sp25.github.io/phase-2)

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
                        ref span,
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
                                            is_array: false,
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

                                // array variabe declaration
                                AST::ArrayFieldDecl {
                                    ref id,
                                    ref size,
                                    ref span,
                                } => {
                                    // Insert variable into the global scope
                                    if global_scope.borrow_mut().insert(
                                        id.clone(),
                                        TableEntry::Variable {
                                            name: id.clone(),
                                            typ: typ.clone(),
                                            is_array: true,
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
            // ✅ Create a new scope for this method, with `enclosing_block` set to Method
            let method_scope = Rc::new(RefCell::new(Scope::add_child(
                Rc::clone(&parent_scope),
                Some(EnclosingBlock::Method(name.clone())),
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
                                is_array: false,
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
            // ✅ Detect if this block is inside a `for` or `while`
            let is_loop_body = matches!(
                parent_scope.borrow().enclosing_block,
                Some(EnclosingBlock::Loop)
            );

            let enclosing_block = if is_loop_body {
                Some(EnclosingBlock::Loop) // ✅ Explicitly mark this block as a loop
            } else {
                parent_scope.borrow().enclosing_block.clone() // ✅ Inherit from parent
            };

            let scope = Rc::new(RefCell::new(Scope::add_child(
                Rc::clone(&parent_scope),
                enclosing_block,
            )));

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
                                    if scope.borrow_mut().insert(
                                        id.clone(),
                                        TableEntry::Variable {
                                            name: id.clone(),
                                            typ: typ.clone(),
                                            is_array: false,
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
                                        is_array: false,
                                        span: span.clone(),
                                        size: Literal::Int("".to_string()) // dummy literal
                                    }));
                                }

                                // Array field declaration
                                AST::ArrayFieldDecl {
                                    ref id,
                                    ref size,
                                    ref span,
                                } => {
                                    if scope.borrow_mut().insert(
                                        id.clone(),
                                        TableEntry::Variable {
                                            name: id.clone(),
                                            typ: typ.clone(),
                                            is_array: true,
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
                                        is_array: true,
                                        span: span.clone(),
                                        size: size.clone()
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
            op,
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
                        op: op.clone()
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
            if scope.borrow_mut().insert(
                var.clone(),
                TableEntry::Variable {
                    name: var.clone(),
                    typ: Type::Int, // TODO: Determine type dynamically
                    is_array: false,
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
                // ❌ If invalid, return an error node
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
            // RULE 2: no identifier is used before being declared
            check_used_before_decl(id, Rc::clone(&scope), span, writer, context);

            let entry = scope.borrow_mut().lookup(id).unwrap();
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

/// Rule 12
fn check_len_argument(id: &AST, span: &Span, scope: &Scope, writer: &mut dyn std::io::Write, context: &mut SemanticContext)-> bool{
    if let AST::Identifier { id, .. } = id {
        if let Some(entry) = scope.lookup(id) {
            match entry {
                TableEntry::Variable { is_array: true, .. } => {
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

/// Rule 19
fn check_in_loop(
    scope: Rc<RefCell<Scope>>, // Keep `Rc<RefCell<Scope>>` instead of `&Scope`
    span: &Span,
    writer: &mut dyn std::io::Write,
    context: &mut SemanticContext,
) {
    if !Scope::is_inside_loop(scope.clone()) { // Correct call
        writeln!(
            writer,
            "{}",
            format_error_message(
                "loop control statement",
                Some(span),
                "Break and continue statements must be inside a loop.",
                context
            )
        )
        .expect("Failed to write error message");
    }
}


/// Rule 23
/// ✅ Ensures that assignment targets are scalars (not arrays)
fn check_scalar_assignment(
    location: &AST,
    span: &Span,
    scope: Rc<RefCell<Scope>>,
    writer: &mut dyn std::io::Write,
    context: &mut SemanticContext,
) {
    match location {
        // ✅ Look up identifier in scope
        AST::Identifier { id, .. } => {
            if let Some(TableEntry::Variable { is_array, .. }) = scope.borrow().lookup(id) {
                if is_array {
                    writeln!(
                        writer,
                        "{}",
                        format_error_message(
                            &format!("variable `{}`", id),
                            Some(span),
                            "Assignment target must be a scalar (non-array) variable.",
                            context
                        )
                    )
                    .expect("Failed to write error message");
                }
            } else {
                writeln!(
                    writer,
                    "{}",
                    format_error_message(
                        &format!("variable `{}`", id),
                        Some(span),
                        "Assignment target is not declared.",
                        context
                    )
                )
                .expect("Failed to write error message");
            }
        }

        // ✅ Array element access (e.g., a[0]) is allowed since it is scalar
        AST::Expr(Expr::ArrAccess { .. }) => {
            // No error; array elements are scalar
        }

        // ❌ Any other expression is invalid as an assignment target
        _ => {
            writeln!(
                writer,
                "{}",
                format_error_message(
                    &format!("{:?}", location),
                    Some(span),
                    "Invalid assignment target: must be a scalar variable or array element.",
                    context
                )
            )
            .expect("Failed to write error message");
        }
    }
}




// #################################################
// ENTRY POINT
// #################################################

// /// Perform all semantic checks not already performed
// /// during SymTree construction.
// pub fn check_semantics(sym_tree: SymProgram, filename: &str, writer: &mut dyn std::io::Write) {

// }

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

    // traverse tree
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
}