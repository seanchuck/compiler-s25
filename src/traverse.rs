/**
 * Traverse the IR after it is built to perform more semantic checks
 */

use core::panic;
use crate::ast::{BinaryOp, Type, UnaryOp};
use crate::symtable::*;
use crate::parse::parse;
use crate::semcheck::format_error_message;
use crate::utils::print::print_symtree;
use std::rc::Rc;
use crate::token::{Span, Token, Literal};
use crate::scope::*;

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
        SymMethod { body, .. } => check_block(&body, writer, context),
        _ => panic!("expected SymMethod!"),
    }
}

fn check_block(block: &SymBlock, writer: &mut dyn std::io::Write, context: &mut SemanticContext) {
    match block {
        SymBlock { scope, statements , span} => {
            for statement in statements.iter() {
                check_statement(statement, &*scope.borrow(), span, writer, context);
            }
        }
        _ => panic!("expected SymBlock!"),
    }
}

fn check_statement(statement: &SymStatement, scope: &Scope, span: &Span, writer: &mut dyn std::io::Write, context: &mut SemanticContext) {
    match statement {
        SymStatement::VarDecl { name, typ, is_array , span} => check_var_decl(name, typ, is_array, scope, span, writer, context),
        SymStatement::Assignment { target, expr, span } => check_assignment(target, expr, scope, span, writer, context),
        SymStatement::MethodCall { method_name, args, span } => check_method_call(method_name, args, scope, span, writer, context),
        SymStatement::If { condition, then_block, else_block, span } => check_if(condition, then_block, else_block, scope, span, writer, context),
        SymStatement::While { condition, block , span} => check_while(condition, block, scope, span, writer, context),
        SymStatement::For { var, init, condition, update, block, span } => check_for(var, init, condition, update, block, scope, span, writer, context),
        SymStatement::Return { expr, span } => check_return(expr, scope, span, writer, context),
        SymStatement::Break{ span} => check_break(span, writer, context),
        SymStatement::Continue{ span} => check_continue(span, writer, context),
        _ => panic!("unexpected SymStatement pattern"),
    }
}

fn check_var_decl(name: &String, typ: &Type, is_array: &bool, scope: &Scope, span: &Span, writer: &mut dyn std::io::Write, context: &mut SemanticContext) {
    
}

fn check_assignment(target: &SymExpr, expr: &SymExpr, scope: &Scope, span: &Span, writer: &mut dyn std::io::Write, context: &mut SemanticContext) {
    check_expr(target, scope, writer, context);
    check_expr(expr, scope, writer, context);
}

fn check_method_call(method_name: &String, args: &Vec<SymExpr>, scope: &Scope, span: &Span, writer: &mut dyn std::io::Write, context: &mut SemanticContext) {
    
}

fn check_if(condition: &SymExpr, then_block: &Rc<SymBlock>, else_block: &Option<Rc<SymBlock>>, scope: &Scope, span: &Span, writer: &mut dyn std::io::Write, context: &mut SemanticContext) {
    check_expr(condition, scope, writer, context);
}

fn check_while(condition: &SymExpr, block: &Rc<SymBlock>, scope: &Scope, span: &Span, writer: &mut dyn std::io::Write, context: &mut SemanticContext) {
    check_expr(condition, scope, writer, context);
}

fn check_for(var: &String, init: &SymExpr, condition: &SymExpr, update: &SymExpr, block: &Rc<SymBlock>, scope: &Scope, span: &Span, writer: &mut dyn std::io::Write, context: &mut SemanticContext) {
    check_expr(init, scope, writer, context);
    check_expr(condition, scope, writer, context);
    check_expr(update, scope, writer, context);
}

fn check_return(expr: &Option<SymExpr>, scope: &Scope, span: &Span, writer: &mut dyn std::io::Write, context: &mut SemanticContext) {
    if expr.is_some() {
        check_expr(&expr.as_ref().clone().unwrap(), scope, writer, context);
    }
}

fn check_break(span: &Span, writer: &mut dyn std::io::Write, context: &mut SemanticContext) {

}

fn check_continue(span: &Span, writer: &mut dyn std::io::Write, context: &mut SemanticContext) {
    
}

fn check_expr(expr: &SymExpr, scope: &Scope, writer: &mut dyn std::io::Write, context: &mut SemanticContext) {
    match expr {
        SymExpr::ArrAccess { id, index, span } => {}
        SymExpr::BinaryExpr { op, left, right, typ, span } => {}
        SymExpr::Cast { target_type, expr, span } => {}
        SymExpr::Error { span } => {}
        SymExpr::Identifier { entry, span } => {}
        SymExpr::Len { id, span } => {}
        SymExpr::Literal { value, span } => check_literal_in_range(value, scope, span, writer, context),
        SymExpr::MethodCall { method_name, args, span } => {}
        SymExpr::UnaryExpr { op, expr, typ, span } => {}
    }
}

// rules 21 and 22
fn check_literal_in_range(lit: &Literal, scope: &Scope, span: &Span, writer: &mut dyn std::io::Write, context: &mut SemanticContext) {
    match lit {
        Literal::Int(val) => {
            let int_val = val.parse::<i32>();
            if int_val.is_err() {
                writeln!(
                    writer,
                    "{}",
                    format_error_message(&val, Some(span), "invalid integer size", context)
                )
                .expect("Failed to write error message");
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
            }
        }

        _ => {}
    }
}