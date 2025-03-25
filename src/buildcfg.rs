/**
Construct a control flow graph (CFG) from the
symbol table IR.

Who adds a basic block to the CFG?
    follow every BasicBlock::New with a cfg.add_block()
**/

use std::{cell::RefCell, collections::HashMap, rc::Rc};
use crate::ast::*;
use crate::cfg::*;
use crate::linear_ir::*;
use crate::scope::TableEntry;
use crate::semcheck::semcheck;
use crate::symtable::{SymExpr, SymMethod, SymProgram, SymStatement};
use crate::token::Literal;

// Initialize a counter for naming temps and indexing basic blocks
thread_local! {
    static TEMP_COUNTER: RefCell<usize> = RefCell::new(0);
    static BBLOCK_COUNTER: RefCell<usize> = RefCell::new(0);
}

/// Create a new temporary variable named "_t{counter}"
/// based on the number of temps currently in the program.
fn fresh_temp() -> String {
    TEMP_COUNTER.with(|counter| {
        let mut count = counter.borrow_mut();
        let temp_name = format!("_t{}", *count);
        *count += 1;
        temp_name
    })
}

/// Returns a new id for the next basic block
fn next_bblock_id() -> i32 {
    BBLOCK_COUNTER.with(|counter| {
        let mut count = counter.borrow_mut();
        let val = *count;
        *count += 1;
        val as i32
    })
}

/// Reset temp variable counter and basic block counters to 0
fn reset_counters() {
    TEMP_COUNTER.with(|counter| {
        let mut count = counter.borrow_mut();
        *count = 0;
    });
    BBLOCK_COUNTER.with(|counter| {
        let mut count = counter.borrow_mut();
        *count = 0;
    });
}

/// Helper to convert literal or identifier expressions into operands.
/// The expression's evaluation starts at the end of cur_block.
/// Returns the block at the end of expr's evaluation, and the Operand 
/// that holds its result.
fn destruct_expr(cfg: &mut CFG, expr: &SymExpr, mut cur_block_id: i32) -> (i32, Operand) {
    match expr {
        SymExpr::Identifier { entry, .. } => match entry {
            TableEntry::Variable { name, .. } => (cur_block_id, Operand::Id(name.clone())),
            TableEntry::Method { name, .. } => (cur_block_id, Operand::Id(name.clone())),
            TableEntry::Import { name, .. } => (cur_block_id, Operand::Id(name.clone())),
        },

        SymExpr::Literal { value, .. } => (cur_block_id, Operand::Const(value.clone())),

        SymExpr::ArrAccess { id, index, .. } => {
            // Load the array element into a temp and then return the temp's operand
            let temp_arr_val = Operand::Id(fresh_temp());

            // array index can be an expression
            let (next_block_id, index_operand) = destruct_expr(cfg, index, cur_block_id);

            cfg.add_instruction_to_block(next_block_id, Instruction::ArrAccess {
                array: Box::new(Operand::Id(id.clone())),
                index: Box::new(index_operand),
                dest: Box::new(temp_arr_val.clone()),
            });

            (next_block_id, temp_arr_val)
        }

        SymExpr::MethodCall {
            method_name, args, ..
        } => {
            // arguments can be expressions
            let mut arg_ops: Vec<Operand> = Vec::new();
            for arg in args {
                let arg_op;
                (cur_block_id, arg_op) = destruct_expr(cfg, expr, cur_block_id);
                arg_ops.push(arg_op);
            }

            let temp_method_result = Operand::Id(fresh_temp());

            cfg.add_instruction_to_block(cur_block_id, Instruction::MethodCall {
                name: method_name.clone(),
                args: arg_ops,
                dest: Some(Box::new(temp_method_result.clone())),
            });

            (cur_block_id, temp_method_result)
        }

        SymExpr::BinaryExpr {
            op, left, right, ..
        } => {
            match op {
                // short-circuiting (from recitation 6)
                BinaryOp::And | BinaryOp::Or => {
                    let dest = Operand::Id(fresh_temp());
                    let mut next_true_block = BasicBlock::new(next_bblock_id());
                    let mut next_false_block = BasicBlock::new(next_bblock_id());
                    let next_block = BasicBlock::new(next_bblock_id());

                    cfg.add_block(&next_true_block);
                    cfg.add_block(&next_false_block);
                    cfg.add_block(&next_block);

                    // if condition is true, set dest = 1 and jump to next_block
                    cfg.add_instruction_to_block(next_true_block.get_id(), Instruction::Assign { 
                        src: Box::new(Operand::Const(Literal::Int("1".to_string()))),
                        dest: Box::new(dest.clone()) 
                    });
                    cfg.add_instruction_to_block(next_true_block.get_id(), Instruction::UJmp { id: next_block.get_id() });

                    // if condition is false, set dest = 0 and jump to next_block
                    cfg.add_instruction_to_block(next_false_block.get_id(), Instruction::Assign { 
                        src: Box::new(Operand::Const(Literal::Int("0".to_string()))),
                        dest: Box::new(dest.clone()) 
                    });
                    cfg.add_instruction_to_block(next_false_block.get_id(), Instruction::UJmp { id: next_block.get_id() });

                    build_cond(
                        cfg, 
                        expr, 
                        cur_block_id,
                        next_true_block.get_id(),
                        next_false_block.get_id()
                    );

                    return (next_block.get_id(), dest);
                }
                _ => {
                    let left_operand: Operand;
                    let right_operand: Operand;
                    (cur_block_id, left_operand) = destruct_expr(cfg, left, cur_block_id);
                    (cur_block_id, right_operand) = destruct_expr(cfg, right, cur_block_id);

                    let result = Operand::Id(fresh_temp());

                    let instruction: Instruction = match op {
                        BinaryOp::Add => Instruction::Add {
                            left: Box::new(left_operand),
                            right: Box::new(right_operand),
                            dest: Box::new(result.clone()),
                        },
                        BinaryOp::Subtract => Instruction::Subtract {
                            left: Box::new(left_operand),
                            right: Box::new(right_operand),
                            dest: Box::new(result.clone()),
                        },
                        BinaryOp::Multiply => Instruction::Multiply {
                            left: Box::new(left_operand),
                            right: Box::new(right_operand),
                            dest: Box::new(result.clone()),
                        },
                        BinaryOp::Divide => Instruction::Divide {
                            left: Box::new(left_operand),
                            right: Box::new(right_operand),
                            dest: Box::new(result.clone()),
                        },
                        BinaryOp::Modulo => Instruction::Modulo {
                            left: Box::new(left_operand),
                            right: Box::new(right_operand),
                            dest: Box::new(result.clone()),
                        },
                        BinaryOp::Less => Instruction::Less {
                            left: Box::new(left_operand),
                            right: Box::new(right_operand),
                            dest: Box::new(result.clone()),
                        },
                        BinaryOp::Greater => Instruction::Greater {
                            left: Box::new(left_operand),
                            right: Box::new(right_operand),
                            dest: Box::new(result.clone()),
                        },
                        BinaryOp::LessEqual => Instruction::LessEqual {
                            left: Box::new(left_operand),
                            right: Box::new(right_operand),
                            dest: Box::new(result.clone()),
                        },
                        BinaryOp::GreaterEqual => Instruction::GreaterEqual {
                            left: Box::new(left_operand),
                            right: Box::new(right_operand),
                            dest: Box::new(result.clone()),
                        },
                        BinaryOp::Equal => Instruction::Equal {
                            left: Box::new(left_operand),
                            right: Box::new(right_operand),
                            dest: Box::new(result.clone()),
                        },
                        BinaryOp::NotEqual => Instruction::NotEqual {
                            left: Box::new(left_operand),
                            right: Box::new(right_operand),
                            dest: Box::new(result.clone()),
                        },
                        _ => unreachable!()
                    };

                    cfg.add_instruction_to_block(cur_block_id, instruction);

                    (cur_block_id, result)
                }
            }
        }

        SymExpr::UnaryExpr { op, expr, .. } => {
            let result = Operand::Id(fresh_temp());
            let operand: Operand;
            (cur_block_id, operand) = destruct_expr(cfg, expr, cur_block_id);

            let instruction: Instruction = match op {
                UnaryOp::Neg => Instruction::Subtract { 
                    left: Box::new(Operand::Const(Literal::Int("0".to_string()))), 
                    right: Box::new(operand), 
                    dest: Box::new(result.clone())
                },
                UnaryOp::Not => Instruction::Not {
                    expr: Box::new(operand),
                    dest: Box::new(result.clone())
                },
            };

            cfg.add_instruction_to_block(cur_block_id, instruction);

            (cur_block_id, result)
        }

        SymExpr::Cast { target_type, expr, .. } => {
            let result = Operand::Id(fresh_temp());
            let operand: Operand;
            (cur_block_id, operand) = destruct_expr(cfg, expr, cur_block_id);

            let instruction = Instruction::Cast { 
                expr: Box::new(operand), 
                dest: Box::new(result.clone()), 
                target_type: target_type.clone()
            };

            cfg.add_instruction_to_block(cur_block_id, instruction);

            (cur_block_id, result)
        }

        SymExpr::Len { id, span } => {
            let result = Operand::Id(fresh_temp());
            let operand: Operand;
            (cur_block_id, operand) = destruct_expr(cfg, expr, cur_block_id);

            let instruction = Instruction::Len { 
                expr: Box::new(operand), 
                dest: Box::new(result.clone()), 
            };

            cfg.add_instruction_to_block(cur_block_id, instruction);

            (cur_block_id, result)
        }

        _ => unreachable!()
    }
}

/// short-circuiting (from recitation 6)
/// Build the CFG for a conditional expression starting at cur_block.
/// next_true_block is the block to jump to if the condition is true
/// next_false_block is the block to jump to if the condition is false
fn build_cond(cfg: &mut CFG, expr: &SymExpr, mut cur_block_id: i32, next_true_block_id: i32, next_false_block_id: i32) {
    match expr {
        SymExpr::BinaryExpr { op, left, right, span } => {
            match op {
                BinaryOp::And => {
                    let mut left_true_block = BasicBlock::new(next_bblock_id());
                    cfg.add_block(&left_true_block);
                    build_cond(cfg, left, cur_block_id, left_true_block.get_id(), next_false_block_id);
                    // RHS is only evaluated if LHS is true
                    build_cond(cfg, right, left_true_block.get_id(), next_true_block_id, next_false_block_id);
                }
                BinaryOp::Or => {
                    let mut left_false_block = BasicBlock::new(next_bblock_id());
                    cfg.add_block(&left_false_block);
                    build_cond(cfg, left, cur_block_id, next_true_block_id, left_false_block.get_id());
                    // RHS is only evaluated if LHS is false
                    build_cond(cfg, right, left_false_block.get_id(), next_true_block_id, next_false_block_id);
                }
                _ => {
                    let dest: Operand;
                    (cur_block_id, dest) = destruct_expr(cfg, expr, cur_block_id);

                    cfg.add_instruction_to_block(cur_block_id, Instruction::Branch { 
                        condition: Box::new(dest), 
                        true_target: next_true_block_id, 
                        false_target: next_false_block_id
                    });
                }
            }
        }
        SymExpr::UnaryExpr { op, expr, span } => {
            match op {
                UnaryOp::Not => {
                    // just swap the true and false blocks
                    build_cond(cfg, expr, cur_block_id, next_false_block_id, next_true_block_id);
                }
                _ => {
                    let dest: Operand;
                    (cur_block_id, dest) = destruct_expr(cfg, expr, cur_block_id);

                    cfg.add_instruction_to_block(cur_block_id, Instruction::Branch { 
                        condition: Box::new(dest), 
                        true_target: next_true_block_id, 
                        false_target: next_false_block_id
                    });
                }
            }
        }
        _ => {
            let dest: Operand;
            (cur_block_id, dest) = destruct_expr(cfg, expr, cur_block_id);
            cfg.add_instruction_to_block(cur_block_id, Instruction::Branch { 
                condition: Box::new(dest), 
                true_target: next_true_block_id, 
                false_target: next_false_block_id
            });
        }
    }
}

#[derive(Debug, Clone)]
pub struct Loop {
    break_to: i32, // ID of basic block following the loop
    continue_to: i32 // ID of loop's header basic block
}

/// Destruct a statement starting at cur_block into basic blocks and add them to the method CFG.
/// Returns the basic block at the end of the statement.
fn destruct_statement(cfg: &mut CFG, mut cur_block_id: i32, statement: &SymStatement, cur_loop: Option<&Loop>) -> i32 {
    // For now, destruct_statement will have responsibility to add any blocks to the CFG

    match &*statement {
        SymStatement::Assignment {
            target, expr, op, ..
        } => {
            let (_, dest) = destruct_expr(cfg, target, cur_block_id); // should just be an identifer
            let rhs: Operand;
            (cur_block_id, rhs) = destruct_expr(cfg, expr, cur_block_id);

            let instr = match op {
                AssignOp::Assign => Instruction::Assign {
                    src: Box::new(rhs),
                    dest: Box::new(dest.clone()),
                },
                AssignOp::PlusAssign => Instruction::Add {
                    left: Box::new(dest.clone()),
                    right: Box::new(rhs),
                    dest: Box::new(dest.clone()),
                },
                AssignOp::MinusAssign => Instruction::Subtract {
                    left: Box::new(dest.clone()),
                    right: Box::new(rhs),
                    dest: Box::new(dest.clone()),
                },
                AssignOp::MultiplyAssign => Instruction::Multiply {
                    left: Box::new(dest.clone()),
                    right: Box::new(rhs),
                    dest: Box::new(dest.clone()),
                },
                AssignOp::DivideAssign => Instruction::Divide {
                    left: Box::new(dest.clone()),
                    right: Box::new(rhs),
                    dest: Box::new(dest.clone()),
                },
                AssignOp::ModuloAssign => Instruction::Modulo {
                    left: Box::new(dest.clone()),
                    right: Box::new(rhs),
                    dest: Box::new(dest.clone()),
                },
            };

            cfg.add_instruction_to_block(cur_block_id, instr);

            cur_block_id
        }

        SymStatement::MethodCall {
            method_name, args, ..
        } => {
            // arguments can be expressions
            let mut arg_ops: Vec<Operand> = Vec::new();
            for arg in args {
                let arg_op;
                (cur_block_id, arg_op) = destruct_expr(cfg, arg, cur_block_id);
                arg_ops.push(arg_op);
            }

            cfg.add_instruction_to_block(cur_block_id, Instruction::MethodCall {
                name: method_name.clone(),
                args: arg_ops,
                dest: None
            });

            cur_block_id
        }

        SymStatement::If {
            condition,
            then_block,
            else_block,
            ..
        } => {
            let mut body_block = &BasicBlock::new(next_bblock_id()); // the block that starts the body
            let mut body_block_id = body_block.get_id();
            cfg.add_block(&body_block);
            let next_block: BasicBlock; // the block after the whole if
            
            if else_block.is_some() {
                let mut else_body_block = &BasicBlock::new(next_bblock_id());
                let mut else_body_block_id = else_body_block.get_id();
                cfg.add_block(&else_body_block);

                next_block = BasicBlock::new(next_bblock_id());
                
                build_cond(cfg, condition, cur_block_id, body_block_id, else_body_block_id);

                for statement in &then_block.statements {
                    body_block_id = destruct_statement(cfg, body_block_id, &statement, cur_loop);
                }

                // jump to next block
                cfg.add_instruction_to_block(body_block_id, Instruction::UJmp { id: next_block.get_id() });

                // same for else body if we have one
                for statement in &else_block.clone().unwrap().statements {
                    else_body_block_id = destruct_statement(cfg, else_body_block_id, statement, cur_loop);
                }
                cfg.add_instruction_to_block(else_body_block_id, Instruction::UJmp { id: next_block.get_id() });
            } else {
                next_block = BasicBlock::new(next_bblock_id());
                
                build_cond(cfg, condition, cur_block_id, body_block_id, next_block.get_id());

                for statement in &then_block.statements {
                    body_block_id = destruct_statement(cfg, body_block_id, statement, cur_loop);
                }

                // jump to next block
                cfg.add_instruction_to_block(body_block_id, Instruction::UJmp { id: next_block.get_id() });
            }

            cfg.add_block(&next_block);
            next_block.get_id()
        },
        SymStatement::While { condition, block, span } => {
            let header_id = next_bblock_id();
            let mut header_block = BasicBlock::new(header_id); // the block that evaluates the loop condition
            let mut body_id = next_bblock_id();
            let mut body_block = &BasicBlock::new(body_id); // the block that starts the body
            let next_block = BasicBlock::new(next_bblock_id()); // the block after the whole loop
            cfg.add_block(&header_block);
            cfg.add_block(&body_block);
            cfg.add_block(&next_block);

            cfg.add_instruction_to_block(cur_block_id, Instruction::UJmp { id: header_block.get_id() });

            build_cond(cfg, condition, header_id, body_id, next_block.get_id());

            let next_loop = Loop { break_to: next_block.get_id(), continue_to: header_id };

            for statement in &block.statements {
                body_id = destruct_statement(cfg, body_id, statement, Some(&next_loop));
            }

            // jump back to header block after the body
            cfg.add_instruction_to_block(body_id, Instruction::UJmp { id: header_id });

            next_block.get_id()
        }
        SymStatement::For { var, init, condition, update, block, .. } => {
            let header_id = next_bblock_id();
            let mut header_block = BasicBlock::new(header_id); // the block that evaluates the loop condition
            let mut body_id = next_bblock_id();
            let mut body_block = &BasicBlock::new(body_id); // the block that starts the body
            let next_block = BasicBlock::new(next_bblock_id()); // the block after the whole loop
            cfg.add_block(&header_block);
            cfg.add_block(&body_block);
            cfg.add_block(&next_block);

            // evaluate the for_init
            let lhs = Operand::Id(var.clone());
            let rhs: Operand;
            (cur_block_id, rhs) = destruct_expr(cfg, init, cur_block_id);
            cfg.add_instruction_to_block(cur_block_id, Instruction::Assign {
                src: Box::new(rhs),
                dest: Box::new(lhs),
            });
            cfg.add_instruction_to_block(cur_block_id, Instruction::UJmp { id: header_block.get_id() });

            build_cond(cfg, condition, header_id, body_id, next_block.get_id());

            let next_loop = Loop { break_to: next_block.get_id(), continue_to: header_id };

            for statement in &block.statements {
                body_id = destruct_statement(cfg, body_id, statement, Some(&next_loop));
            }

            // execute the for_update
            body_id = destruct_statement(cfg, body_id, update, Some(&next_loop));

            // jump back to header block after the body
            cfg.add_instruction_to_block(body_id, Instruction::UJmp { id: header_id });

            next_block.get_id()
        }
        SymStatement::Break { .. } => {
            cfg.add_instruction_to_block(cur_block_id, Instruction::UJmp { id: cur_loop.unwrap().break_to });

            // code after the break is unreachable
            -1
        }
        SymStatement::Continue { .. } => {
            cfg.add_instruction_to_block(cur_block_id, Instruction::UJmp { id: cur_loop.clone().unwrap().continue_to });

            // code after the continue is unreachable
            -1
        }
        SymStatement::Return { expr, .. } => {
            if expr.is_some() {
                let operand: Operand;
                (cur_block_id, operand) = destruct_expr(cfg, &expr.clone().unwrap(), cur_block_id);
                cfg.add_instruction_to_block(cur_block_id, Instruction::Ret { value: Some(Box::new(operand)) });
            } else {
                cfg.add_instruction_to_block(cur_block_id, Instruction::Ret { value: None });
            }

            // code after the return is unreachable
            -1
        }
        SymStatement::VarDecl { .. } => {
            // no need to do anything; default values are undefined
            cur_block_id
        }
        _ => unreachable!()
    }
}

/// Destruct a method AST node into basic blocks
fn destruct_method(method: &Rc<SymMethod>) -> CFG {
    // reset counters for each CFG
    reset_counters();

    let mut method_cfg = CFG::new();

    let mut cur_block_id = next_bblock_id(); // should be 0
    let entry_block = BasicBlock::new(cur_block_id);
    method_cfg.add_block(&entry_block);    

    for statement in method.body.statements.clone() {
        cur_block_id = destruct_statement(&mut method_cfg, cur_block_id, &statement, None);
    }

    method_cfg
}

/// Destruct a program AST node into basic blocks
fn destruct_program(program: SymProgram) -> HashMap<String, CFG> {
    let mut method_cfgs: HashMap<String, CFG> = HashMap::new(); // Hash map to map method names to associated CFG

    for (name, method) in &program.methods {
        let cfg: CFG = destruct_method(method);
        method_cfgs.insert(name.clone(), cfg);
    }

    method_cfgs
}

/// Build the CFG from the symbol table IR.
/// The outmap is a map between (method_name, method_cfg).
/// Might need further processing to combine basic blocks.
pub fn build_cfg(
    file: &str,
    filename: &str,
    writer: &mut dyn std::io::Write,
    debug: bool,
) -> HashMap<String, CFG> {
    // Create symbol table IR
    let sym_tree: SymProgram = semcheck(file, filename, writer, debug);

    // Generate a CFG for each method
    let method_cfgs: HashMap<String, CFG> = destruct_program(sym_tree);
    method_cfgs
}
