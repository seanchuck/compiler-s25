/**
 * Traverse the IR after it is built to perform more semantic checks
 */

use core::panic;
use std::cmp;
use nom::bytes::is_a;

use crate::ast::{AssignOp, BinaryOp, Type, UnaryOp};
use crate::symtable::*;
use crate::parse::parse;
use crate::semcheck::{format_error_message, format_error_message0, format_error_message2};
use crate::utils::print::print_symtree;
use std::rc::Rc;
use crate::token::{Span, Token, Literal};
use crate::scope::*;

#[derive(PartialEq)]
enum BlockType {
    Method(Type), // return type
    Loop,
    Other
}

pub fn traverse_ir(program: &SymProgram, writer: &mut dyn std::io::Write, context: &mut SemanticContext) {
    match program {
        SymProgram { methods, global_scope, span } => {
            for (name, body) in methods.iter() {
                check_method(name, body, writer, context);
            }
        }
        _ => panic!("expected SymProgram!"),
    }
}

fn check_method(name: &String, body: &SymMethod, writer: &mut dyn std::io::Write, context: &mut SemanticContext) {
    match body {
        SymMethod { return_type, body, .. } => check_block(&body, &BlockType::Method(return_type.clone()), writer, context),
        _ => panic!("expected SymMethod!"),
    }
}

fn check_block(block: &SymBlock, block_type: &BlockType, writer: &mut dyn std::io::Write, context: &mut SemanticContext) {
    match block {
        SymBlock { scope, statements , span} => {
            for statement in statements.iter() {
                check_statement(statement, block_type, &*scope.borrow(), span, writer, context);
            }
        }
        _ => panic!("expected SymBlock!"),
    }
}

fn check_statement(statement: &SymStatement, block_type: &BlockType, scope: &Scope, span: &Span, writer: &mut dyn std::io::Write, context: &mut SemanticContext) {
    match statement {
        SymStatement::VarDecl { name, typ, is_array, size, span} => check_var_decl(name, typ, is_array, size, scope, span, writer, context),
        SymStatement::Assignment { target, expr, op, span } => check_assignment(target, expr, op, scope, span, writer, context),
        SymStatement::MethodCall { method_name, args, span } => check_method_call(method_name, &args.into_iter().map(|expr| Rc::new(expr.clone())).collect(), scope, span, writer, context),
        SymStatement::If { condition, then_block, else_block, span } => check_if(condition, then_block, else_block, scope, span, writer, context),
        SymStatement::While { condition, block , span} => check_while(condition, block, scope, span, writer, context),
        SymStatement::For { var, init, condition, update, block, span } => check_for(var, init, condition, update, block, scope, span, writer, context),
        SymStatement::Return { expr, span } => check_return(expr, block_type, scope, span, writer, context),
        SymStatement::Break{ span} => check_break(block_type, span, writer, context),
        SymStatement::Continue{ span} => check_continue(block_type, span, writer, context),
        _ => panic!("unexpected SymStatement pattern"),
    }
}

fn check_var_decl(name: &String, typ: &Type, is_array: &bool, size: &Literal, scope: &Scope, span: &Span, writer: &mut dyn std::io::Write, context: &mut SemanticContext) {
    if *is_array {
        infer_literal_type(size, scope, span, writer, context); // must be an int, but check rule 21
    }
}

fn check_assignment(target: &SymExpr, expr: &SymExpr, op: &AssignOp, scope: &Scope, span: &Span, writer: &mut dyn std::io::Write, context: &mut SemanticContext) {
    match target {
        SymExpr::Identifier { entry, span } => {
            match entry {
                TableEntry::Variable { name, typ, is_array, span } => {
                    // rule 23
                    if *is_array {
                        writeln!(
                            writer,
                            "{}",
                            format_error_message(name, Some(&span), "cannot assign to array", context)
                        )
                        .expect("Failed to write error message");
                    } else {
                        let expr_type = infer_expr_type(expr, scope, span, writer, context);

                        match op {
                            AssignOp::Assign => {
                                // rule 17
                                if expr_type.is_some() && expr_type.clone().unwrap() != *typ {
                                    writeln!(
                                        writer,
                                        "{}",
                                        format_error_message(&format!("{}", typ), Some(&span), &format!("cannot assign type `{}` to type", expr_type.unwrap()), context)
                                    )
                                    .expect("Failed to write error message");
                                }
                            }
                            _ => {
                                // rule 18
                                if expr_type.is_some() && expr_type.clone().unwrap() != Type::Int && expr_type.clone().unwrap() != Type::Long
                                   || *typ != Type::Int && *typ != Type::Long {
                                    writeln!(
                                        writer,
                                        "{}",
                                        format_error_message2(&format!("{}", typ), &format!("{}", expr_type.unwrap()), Some(&span), "invalid types for compound assignment", context)
                                    )
                                    .expect("Failed to write error message");
                                } else if expr_type.is_some() && expr_type.clone().unwrap() != *typ {
                                    writeln!(
                                        writer,
                                        "{}",
                                        format_error_message2(&format!("{}", expr_type.unwrap()), &format!("{}", typ), Some(&span), "mismatched types in compound assignment", context)
                                    )
                                    .expect("Failed to write error message");
                                }
                            }
                        }

                        
                    }
                }
                TableEntry::Import { name, span } => {
                    writeln!(
                        writer,
                        "{}",
                        format_error_message(name, Some(&span), "cannot assign to import", context)
                    )
                    .expect("Failed to write error message");
                }
                TableEntry::Method { name, return_type, params, span } => {
                    writeln!(
                        writer,
                        "{}",
                        format_error_message(name, Some(&span), "cannot assign to method", context)
                    )
                    .expect("Failed to write error message");
                }
            }
        }
        _ => unreachable!() // target must be an identifier
    }
}

fn check_method_call(method_name: &String, args: &Vec<Rc<SymExpr>>, scope: &Scope, span: &Span, writer: &mut dyn std::io::Write, context: &mut SemanticContext) {
    infer_method_call_type(method_name, args, scope, span, writer, context);
}

fn check_if(condition: &SymExpr, then_block: &Rc<SymBlock>, else_block: &Option<Rc<SymBlock>>, scope: &Scope, span: &Span, writer: &mut dyn std::io::Write, context: &mut SemanticContext) {
    // rule 13
    let cond_type = infer_expr_type(condition, scope, span, writer, context);

    if cond_type.is_some() && cond_type.clone().unwrap() != Type::Bool {
        writeln!(
            writer,
            "{}",
            format_error_message(&format!("{}", cond_type.unwrap()), Some(&span), "if condition must have type `bool`, instead found", context)
        )
        .expect("Failed to write error message");
    }

    check_block(&then_block, &BlockType::Other, writer, context);

    if else_block.is_some() {
        check_block(&else_block.as_ref().unwrap(), &BlockType::Other, writer, context);
    }
}

fn check_while(condition: &SymExpr, block: &Rc<SymBlock>, scope: &Scope, span: &Span, writer: &mut dyn std::io::Write, context: &mut SemanticContext) {
    // rule 13
    let cond_type = infer_expr_type(condition, scope, span, writer, context);

    if cond_type.is_some() && cond_type.clone().unwrap() != Type::Bool {
        writeln!(
            writer,
            "{}",
            format_error_message(&format!("{}", cond_type.unwrap()), Some(&span), "while condition must have type `bool`, instead found", context)
        )
        .expect("Failed to write error message");
    }

    check_block(&block, &BlockType::Loop, writer, context);
}

fn check_for(var: &String, init: &SymExpr, condition: &SymExpr, update: &SymExpr, block: &Rc<SymBlock>, scope: &Scope, span: &Span, writer: &mut dyn std::io::Write, context: &mut SemanticContext) {
    let init_type = infer_expr_type(init, scope, span, writer, context);
    let cond_type = infer_expr_type(condition, scope, span, writer, context);
    let update_type = infer_expr_type(update, scope, span, writer, context);

    // rule 5
    if init_type.is_some() && init_type == Some(Type::Void) {
        writeln!(
            writer,
            "{}",
            format_error_message(&format!("{}", init_type.unwrap()), Some(&span), "method call in init expression cannot return type", context)
        )
        .expect("Failed to write error message");
    }

    if update_type.is_some() && update_type == Some(Type::Void) {
        writeln!(
            writer,
            "{}",
            format_error_message(&format!("{}", update_type.unwrap()), Some(&span), "method call in update expression cannot return type", context)
        )
        .expect("Failed to write error message");
    }

    // rule 13
    if cond_type.is_some() && cond_type != Some(Type::Bool) {
        writeln!(
            writer,
            "{}",
            format_error_message(&format!("{}", cond_type.unwrap()), Some(&span), "for condition must have type `bool`, instead found", context)
        )
        .expect("Failed to write error message");
    }

}

// rules 7 and 8
fn check_return(expr: &Option<SymExpr>, block_type: &BlockType, scope: &Scope, span: &Span, writer: &mut dyn std::io::Write, context: &mut SemanticContext) {
    match block_type {
        BlockType::Method(typ) => {
            if expr.is_some() {
                if *typ == Type::Void {
                    writeln!(
                        writer,
                        "{}",
                        format_error_message(&format!("{}", typ), Some(&span), "did not expect expression, method returns", context)
                    )
                    .expect("Failed to write error message");
                } else {
                    let expr_type = infer_expr_type(expr.as_ref().unwrap(), scope, span, writer, context);

                    if expr_type.clone().unwrap() != *typ {
                        writeln!(
                            writer,
                            "{}",
                            format_error_message(&format!("{}", expr_type.unwrap()), Some(&span), &format!("expected return type {}, instead found", typ), context)
                        )
                        .expect("Failed to write error message");
                    }
                }
            }
        }
        _ => {
            writeln!(
                writer,
                "{}",
                format_error_message0(Some(&span), "can only return from a method", context)
            )
            .expect("Failed to write error message");
        }
    }
    
    
}

// rule 19
fn check_break(block_type: &BlockType, span: &Span, writer: &mut dyn std::io::Write, context: &mut SemanticContext) {
    if *block_type != BlockType::Loop {
        writeln!(
            writer,
            "{}",
            format_error_message0(Some(&span), "can only break from a loop", context)
        )
        .expect("Failed to write error message");
    }
}

// rule 19
fn check_continue(block_type: &BlockType, span: &Span, writer: &mut dyn std::io::Write, context: &mut SemanticContext) {
    if *block_type != BlockType::Loop {
        writeln!(
            writer,
            "{}",
            format_error_message0(Some(&span), "can only continue from a loop", context)
        )
        .expect("Failed to write error message");
    }
}

fn infer_expr_type(expr: &SymExpr, scope: &Scope, span: &Span, writer: &mut dyn std::io::Write, context: &mut SemanticContext) -> Option<Type> {
    match expr {
        SymExpr::ArrAccess { id, index, span } => infer_arr_access_type(id, index, scope, span, writer, context),
        SymExpr::BinaryExpr { op, left, right, span } => infer_binary_expr_type(op, left, right, scope, span, writer, context),
        SymExpr::Cast { target_type, expr, span } => infer_cast_type(target_type, expr, scope, span, writer, context),
        SymExpr::Error { span } => None,
        SymExpr::Identifier { entry, span } => infer_id_type(entry, scope, span, writer, context),
        SymExpr::Len { id, span } => infer_len_type(id, scope, span, writer, context),
        SymExpr::Literal { value, span } => infer_literal_type(value, scope, span, writer, context),
        SymExpr::MethodCall { method_name, args, span } => infer_method_call_type(method_name, args, scope, span, writer, context),
        SymExpr::UnaryExpr { op, expr, span } => infer_unary_expr_type(op, expr, scope, span, writer, context)
    }
}

fn infer_arr_access_type(id: &String, index: &SymExpr, scope: &Scope, span: &Span, writer: &mut dyn std::io::Write, context: &mut SemanticContext) -> Option<Type> {
    let entry = scope.lookup(id);

    // rule 11
    match entry {
        Some(TableEntry::Variable { name, typ, is_array, span }) => {
            match is_array {
                true => {
                    let index_type = infer_expr_type(index, scope, &span, writer, context);

                    match index_type {
                        Some(Type::Int) => Some(typ),
                        Some(typ) => {
                            writeln!(
                                writer,
                                "{}",
                                format_error_message(&format!("{}", typ), Some(&span), "invalid index type", context)
                            )
                            .expect("Failed to write error message");

                            None
                        }
                        None => None // assume an error has already been printed
                    }
                }
                false => {
                    writeln!(
                        writer,
                        "{}",
                        format_error_message(&name, Some(&span), "cannot index into non-array", context)
                    )
                    .expect("Failed to write error message");

                    None
                }
            }
        }
        _ => unreachable!()
    }
}

fn infer_binary_expr_type(op: &BinaryOp, left: &SymExpr, right: &SymExpr, scope: &Scope, span: &Span, writer: &mut dyn std::io::Write, context: &mut SemanticContext) -> Option<Type> {
    let left_type = infer_expr_type(left, scope, span, writer, context);
    let right_type = infer_expr_type(left, scope, span, writer, context);

    match op {
        // rule 14
        BinaryOp::Add | BinaryOp::Divide | BinaryOp::Modulo | BinaryOp::Multiply | BinaryOp::Subtract => {
            match left_type {
                Some(Type::Int) => {
                    match right_type {
                        Some(Type::Int) => Some(Type::Int),
                        Some(Type::Long) => {
                            writeln!(
                                writer,
                                "{}",
                                format_error_message2(&format!("{}", left_type.unwrap()), &format!("{}", right_type.unwrap()), Some(&span), "mismatched types for arithmetic operation", context)
                            )
                            .expect("Failed to write error message");

                            None
                        },
                        Some(_) => {
                            writeln!(
                                writer,
                                "{}",
                                format_error_message2(&format!("{}", left_type.unwrap()), &format!("{}", right_type.unwrap()), Some(&span), "invalid types for arithmetic operation", context)
                            )
                            .expect("Failed to write error message");

                            None
                        }
                        None => None // assume error already printed
                    }
                }
                Some(Type::Long) => {
                    match right_type {
                        Some(Type::Long) => Some(Type::Long),
                        Some(Type::Int) => {
                            writeln!(
                                writer,
                                "{}",
                                format_error_message2(&format!("{}", left_type.unwrap()), &format!("{}", right_type.unwrap()), Some(&span), "mismatched types for arithmetic operation", context)
                            )
                            .expect("Failed to write error message");

                            None
                        },
                        Some(_) => {
                            writeln!(
                                writer,
                                "{}",
                                format_error_message2(&format!("{}", left_type.unwrap()), &format!("{}", right_type.unwrap()), Some(&span), "invalid types for arithmetic operation", context)
                            )
                            .expect("Failed to write error message");

                            None
                        }
                        None => None // assume error already printed
                    }
                }
                Some(_) => {
                    writeln!(
                        writer,
                        "{}",
                        format_error_message2(&format!("{}", left_type.unwrap()), &format!("{}", right_type.unwrap()), Some(&span), "invalid types for arithmetic operation", context)
                    )
                    .expect("Failed to write error message");

                    None
                }
                None => None // assume error already printed
            }
        }

        // rule 16
        BinaryOp::And | BinaryOp::Or => {
            match left_type {
                Some(Type::Bool) => {
                    match right_type {
                        Some(Type::Bool) => Some(Type::Bool),
                        Some(_) => {
                            writeln!(
                                writer,
                                "{}",
                                format_error_message2(&format!("{}", left_type.unwrap()), &format!("{}", right_type.unwrap()), Some(&span), "invalid types for conditional operation", context)
                            )
                            .expect("Failed to write error message");
        
                            None
                        }
                        None => None // assume error already printed
                    }
                }
                Some(_) => {
                    writeln!(
                        writer,
                        "{}",
                        format_error_message2(&format!("{}", left_type.unwrap()), &format!("{}", right_type.unwrap()), Some(&span), "invalid types for conditional operation", context)
                    )
                    .expect("Failed to write error message");

                    None
                }
                None => None // assume error already printed
            }
        }

        // rule 14
        BinaryOp::Greater | BinaryOp::GreaterEqual | BinaryOp::Less | BinaryOp::LessEqual => {
            if (left_type == Some(Type::Int) || left_type == Some(Type::Long)) && (right_type == Some(Type::Int) || right_type == Some(Type::Long)) {
                Some(Type::Bool)
            } else {
                writeln!(
                    writer,
                    "{}",
                    format_error_message2(&format!("{}", left_type.unwrap()), &format!("{}", right_type.unwrap()), Some(&span), "invalid types for relational operation", context)
                )
                .expect("Failed to write error message");

                None
            }
        }

        // rule 15
        BinaryOp::Equal | BinaryOp::NotEqual => {
            match left_type {
                Some(Type::Int) | Some(Type::Long) | Some(Type::Bool) => {
                    if left_type == right_type {
                        Some(Type::Bool)
                    } else {
                        writeln!(
                            writer,
                            "{}",
                            format_error_message2(&format!("{}", left_type.unwrap()), &format!("{}", right_type.unwrap()), Some(&span), "mismatched types for equality operation", context)
                        )
                        .expect("Failed to write error message");

                        None
                    }
                }
                Some(_) => {
                    writeln!(
                        writer,
                        "{}",
                        format_error_message2(&format!("{}", left_type.unwrap()), &format!("{}", right_type.unwrap()), Some(&span), "invalid types for equality operation", context)
                    )
                    .expect("Failed to write error message");

                    None
                }
                None => None // assume error already printed
            }
        }
    }
}

fn infer_cast_type(target_type: &Type, expr: &SymExpr, scope: &Scope, span: &Span, writer: &mut dyn std::io::Write, context: &mut SemanticContext) -> Option<Type> {
    let expr_type = infer_expr_type(expr, scope, span, writer, context);
    
    // rule 20
    match target_type {
        Type::Int | Type::Long => {
            match expr_type {
                Some(Type::Int) | Some(Type::Long) => Some(target_type.clone()),
                Some(_) => {
                    writeln!(
                        writer,
                        "{}",
                        format_error_message2(&format!("{}", target_type), &format!("{}", expr_type.unwrap()), Some(&span), "incompatible types for cast", context)
                    )
                    .expect("Failed to write error message");
    
                    None
                }
                None => None // assume error already printed
            }
        }
        _ => {
            writeln!(
                writer,
                "{}",
                format_error_message(&format!("{}", target_type), Some(&span), "cannot cast to type", context)
            )
            .expect("Failed to write error message");

            None
        }
    }
}

fn infer_id_type(entry: &TableEntry, scope: &Scope, span: &Span, writer: &mut dyn std::io::Write, context: &mut SemanticContext) -> Option<Type> {
    match entry {
        TableEntry::Variable { typ, is_array, .. } => {
            match is_array {
                true => Some(Type::Array(Box::new(typ.clone()))),
                false => Some(typ.clone())
            }
        }
        TableEntry::Method { return_type, .. } => Some(Type::Method(Box::new(return_type.clone()))),
        _ => unreachable!()
    }
}

fn infer_len_type(id: &String, scope: &Scope, span: &Span, writer: &mut dyn std::io::Write, context: &mut SemanticContext) -> Option<Type> {
    // rule 12 already checked
    Some(Type::Int)
}

// rules 21 and 22
fn infer_literal_type(value: &Literal, scope: &Scope, span: &Span, writer: &mut dyn std::io::Write, context: &mut SemanticContext) -> Option<Type> {
    match value {
        Literal::Int(val) => {
            let int_val = val.parse::<i32>();
            if int_val.is_err() {
                writeln!(
                    writer,
                    "{}",
                    format_error_message(&val, Some(span), "invalid integer size", context)
                )
                .expect("Failed to write error message");

                None
            } else {
                Some(Type::Int)
            }
        }
    
        Literal::Long(val) => {
            let long_val = val.parse::<i64>();
            if long_val.is_err() {
                writeln!(
                    writer,
                    "{}",
                    format_error_message(&val, Some(span), "invalid long size", context)
                )
                .expect("Failed to write error message");

                None
            } else {
                Some(Type::Long)
            }
        }

        Literal::HexInt(val) => {
            let int_val = i32::from_str_radix(&val, 16);

            if int_val.is_err() {
                let msg = 
                    if val.starts_with("-") {
                        format!("-0x{}", &val[1..]) // move negative sign before 0x
                    } else {
                        format!("0x{}", val)
                    };

                writeln!(
                    writer,
                    "{}",
                    format_error_message(&msg, Some(span), "invalid integer size", context)
                )
                .expect("Failed to write error message");

                None
            } else {
                Some(Type::Int)
            }
        }

        Literal::HexLong(val) => {
            let int_val = i64::from_str_radix(&val, 16);

            if int_val.is_err() {
                let msg = 
                    if val.starts_with("-") {
                        format!("-0x{}", &val[1..]) // move negative sign before 0x
                    } else {
                        format!("0x{}", val)
                    };

                writeln!(
                    writer,
                    "{}",
                    format_error_message(&msg, Some(span), "invalid long size", context)
                )
                .expect("Failed to write error message");

                None
            } else {
                Some(Type::Long)
            }
        }

        Literal::Bool(val) => Some(Type::Bool),
        Literal::Char(val) => Some(Type::Int),
        Literal::String(val) => Some(Type::String)
    }
}

// rules 4 and 10
fn infer_method_call_type(method_name: &String, args: &Vec<Rc<SymExpr>>, scope: &Scope, span: &Span, writer: &mut dyn std::io::Write, context: &mut SemanticContext) -> Option<Type> {
    let entry = scope.lookup(method_name);

    // rule 10
    match entry {
        Some(TableEntry::Import { span, .. }) => {
            // rule 5
            for arg in args {
                let arg_type = infer_expr_type(arg, scope, &span, writer, context);

                if arg_type.is_some() && arg_type == Some(Type::Void) {
                    writeln!(
                        writer,
                        "{}",
                        format_error_message0(Some(&span), "arguments cannot have type `void`", context)
                    )
                    .expect("Failed to write error message");
                }
            }

            Some(Type::Int)
        }
        
        Some(TableEntry::Method { name, return_type, params, span }) => {
            // rule 4
            if args.len() != params.len() {
                writeln!(
                    writer,
                    "{}",
                    format_error_message(&method_name, Some(&span), "incorrect number of arguments to method", context)
                )
                .expect("Failed to write error message");
            }

            for i in 0..cmp::min(args.len(), params.len()) {
                let expr_type = infer_expr_type(&args[i], scope, &span, writer, context);

                if expr_type.is_some() && expr_type.clone().unwrap() != params[i].0 {
                    writeln!(
                        writer,
                        "{}",
                        format_error_message2(&format!("{}", params[i].0), &format!("{}", expr_type.clone().unwrap()), Some(&span), &format!("mismatched types for param {}", &params[i].1), context)
                    )
                    .expect("Failed to write error message");
                }
            }

            Some(return_type)
        }
        _ => {
            writeln!(
                writer,
                "{}",
                format_error_message(&method_name, Some(span), "cannot call non-method", context)
            )
            .expect("Failed to write error message");

            None
        }
    }
}

fn infer_unary_expr_type(op: &UnaryOp, expr: &SymExpr, scope: &Scope, span: &Span, writer: &mut dyn std::io::Write, context: &mut SemanticContext) -> Option<Type> {
    match op {
        UnaryOp::Neg => {
            let expr_type = infer_expr_type(expr, scope, span, writer, context);

            // rule 14
            match expr_type {
                Some(Type::Int) => Some(Type::Int),
                Some(Type::Long) => Some(Type::Long),
                Some(typ) => {
                    writeln!(
                        writer,
                        "{}",
                        format_error_message(&format!("{}", typ), Some(span), "invalid type for operand of unary minus", context)
                    )
                    .expect("Failed to write error message");

                    None
                }
                None => None // assume an error has already been printed
            }
        }
        UnaryOp::Not => {
            let expr_type = infer_expr_type(expr, scope, span, writer, context);

            // rule 16
            match expr_type {
                Some(Type::Bool) => Some(Type::Bool),
                Some(typ) => {
                    writeln!(
                        writer,
                        "{}",
                        format_error_message(&format!("{}", typ), Some(span), "invalid type for operand of logical not", context)
                    )
                    .expect("Failed to write error message");

                    None
                }
                None => None // assume an error has already been printed
            }
        }
    }
}