/**
Construct a control flow graph (CFG) from the
symbol table IR.

Who adds a basic block to the CFG?
    - In general, parent destructor recursively calls another method
    and receives (begin, end). Parent should add those to the CFG,
    and everything else should be added recursively by callee.
    - Or: just follow every BasicBlock::New with a cfg.add_block()

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
/// Returns the block at the end of expr's evaluation and the Operand 
/// that holds its result.
fn destruct_expr(cfg: &mut CFG, expr: &SymExpr, mut cur_block: &mut BasicBlock) -> (BasicBlock, Operand) {
    match expr {
        SymExpr::Identifier { entry, .. } => match &*entry {
            TableEntry::Variable { name, .. } => (cur_block.clone(), Operand::Id(name.clone())),
            TableEntry::Method { name, .. } => (cur_block.clone(), Operand::Id(name.clone())),
            TableEntry::Import { name, .. } => (cur_block.clone(), Operand::Id(name.clone())),
        },

        SymExpr::Literal { value, .. } => (cur_block.clone(), Operand::Const(value.clone())),

        SymExpr::ArrAccess { id, index, .. } => {
            // Load the array element into a temp and then return the temp's operand
            let temp_arr_val = Operand::Id(fresh_temp());

            // array index can be an expression
            let (mut next_block, index_operand) = destruct_expr(cfg, index, cur_block);

            next_block.add_instruction(Instruction::ArrAccess {
                array: Box::new(Operand::Id(id.clone())),
                index: Box::new(index_operand),
                dest: Box::new(temp_arr_val.clone()),
            });

            (next_block, temp_arr_val)
        }

        SymExpr::MethodCall {
            method_name, args, ..
        } => {
            // arguments can be expressions
            let arg_ops: Vec<Operand> = args
                .iter()
                .map(|arg| {
                    let arg_op: Operand;
                    (*cur_block, arg_op) = destruct_expr(cfg, expr, cur_block);
                    arg_op
                })
                .collect();

            let temp_method_result = Operand::Id(fresh_temp());

            cur_block.add_instruction(Instruction::MethodCall {
                name: method_name.clone(),
                args: arg_ops,
                dest: Some(Box::new(temp_method_result.clone())),
            });

            (cur_block.clone(), temp_method_result)
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
                    next_true_block.add_instruction(Instruction::Assign { 
                        src: Box::new(Operand::Const(Literal::Int("1".to_string()))),
                        dest: Box::new(dest.clone()) 
                    });
                    next_true_block.add_instruction(Instruction::UJmp { id: next_block.get_id() });

                    // if condition is false, set dest = 0 and jump to next_block
                    next_false_block.add_instruction(Instruction::Assign { 
                        src: Box::new(Operand::Const(Literal::Int("0".to_string()))), 
                        dest: Box::new(dest.clone()) 
                    });
                    next_false_block.add_instruction(Instruction::UJmp { id: next_block.get_id() });

                    build_cond(
                        cfg, 
                        expr, 
                        cur_block,
                        &next_true_block,
                        &next_false_block
                    );

                    return (next_block, dest);
                }
                _ => {
                    let left_operand: Operand;
                    let right_operand: Operand;
                    (*cur_block, left_operand) = destruct_expr(cfg, left, cur_block);
                    (*cur_block, right_operand) = destruct_expr(cfg, right, cur_block);
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

                    cur_block.add_instruction(instruction);
                    (cur_block.clone(), result)
                }
            }
        }

        SymExpr::UnaryExpr { op, expr, .. } => {
            let result = Operand::Id(fresh_temp());
            let operand: Operand;
            (*cur_block, operand) = destruct_expr(cfg, expr, cur_block);

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

            cur_block.add_instruction(instruction);
            (cur_block.clone(), result)
        }

        SymExpr::Cast { target_type, expr, .. } => {
            let result = Operand::Id(fresh_temp());
            let operand: Operand;
            (*cur_block, operand) = destruct_expr(cfg, expr, cur_block);

            let instruction = Instruction::Cast { 
                expr: Box::new(operand), 
                dest: Box::new(result.clone()), 
                target_type: target_type.clone()
            };

            cur_block.add_instruction(instruction);
            (cur_block.clone(), result)
        }

        SymExpr::Len { id, span } => {
            let result = Operand::Id(fresh_temp());
            let operand: Operand;
            (*cur_block, operand) = destruct_expr(cfg, expr, cur_block);

            let instruction = Instruction::Len { 
                expr: Box::new(operand), 
                dest: Box::new(result.clone()), 
            };

            cur_block.add_instruction(instruction);
            (cur_block.clone(), result)
        }

        _ => unreachable!()
    }
}

/// short-circuiting (from recitation 6)
/// Build the CFG for a conditional expression starting at cur_block.
/// next_true_block is the block to jump to if the condition is true
/// next_false_block is the block to jump to if the condition is false
fn build_cond(cfg: &mut CFG, expr: &SymExpr, mut cur_block: &mut BasicBlock, next_true_block: &BasicBlock, next_false_block: &BasicBlock) {
    match expr {
        SymExpr::BinaryExpr { op, left, right, span } => {
            match op {
                BinaryOp::And => {
                    let mut left_true_block = BasicBlock::new(next_bblock_id());
                    cfg.add_block(&left_true_block);
                    build_cond(cfg, left, cur_block, &left_true_block, next_false_block);
                    // RHS is only evaluated if LHS is true
                    build_cond(cfg, right, &mut left_true_block, next_true_block, next_false_block);
                }
                BinaryOp::Or => {
                    let mut left_false_block = BasicBlock::new(next_bblock_id());
                    cfg.add_block(&left_false_block);
                    build_cond(cfg, left, cur_block, next_true_block, &left_false_block);
                    // RHS is only evaluated if LHS is false
                    build_cond(cfg, right, &mut left_false_block, next_true_block, next_false_block);
                }
                _ => {
                    let dest: Operand;
                    (*cur_block, dest) = destruct_expr(cfg, expr, &mut cur_block);
                    cur_block.add_instruction(Instruction::Branch { 
                        condition: Box::new(dest), 
                        true_target: next_true_block.get_id(), 
                        false_target: next_false_block.get_id()
                    });
                }
            }
        }
        SymExpr::UnaryExpr { op, expr, span } => {
            match op {
                UnaryOp::Not => {
                    // just swap the true and false blocks
                    build_cond(cfg, expr, cur_block, next_false_block, next_true_block);
                }
                _ => {
                    let dest: Operand;
                    (*cur_block, dest) = destruct_expr(cfg, expr, &mut cur_block);
                    cur_block.add_instruction(Instruction::Branch { 
                        condition: Box::new(dest), 
                        true_target: next_true_block.get_id(), 
                        false_target: next_false_block.get_id()
                    });
                }
            }
        }
        _ => {
            let dest: Operand;
            (*cur_block, dest) = destruct_expr(cfg, expr, &mut cur_block);
            cur_block.add_instruction(Instruction::Branch { 
                condition: Box::new(dest), 
                true_target: next_true_block.get_id(), 
                false_target: next_false_block.get_id()
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
fn destruct_statement(cfg: &mut CFG, mut cur_block: &mut BasicBlock, statement: Rc<SymStatement>, cur_loop: &Option<Loop>) -> BasicBlock {
    // For now, destruct_statement will have responsibility to add any blocks to the CFG

    match &*statement {
        SymStatement::Assignment {
            target, expr, op, ..
        } => {
            let (_, dest) = destruct_expr(cfg, target, &mut cur_block); // should just be an identifer
            let rhs: Operand;
            (*cur_block, rhs) = destruct_expr(cfg, expr, &mut cur_block);

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

            cur_block.add_instruction(instr);
            cfg.add_block(&cur_block);
            cur_block.clone()
        }

        SymStatement::MethodCall {
            method_name, args, ..
        } => {
            // arguments can be expressions
            let arg_ops: Vec<Operand> = args
                .iter()
                .map(|arg| {
                    let arg_op: Operand;
                    (*cur_block, arg_op) = destruct_expr(cfg, arg, &mut cur_block);
                    arg_op
                })
                .collect();

            cur_block.add_instruction(Instruction::MethodCall {
                name: method_name.clone(),
                args: arg_ops,
                dest: None
            });

            cur_block.clone()
        }

        SymStatement::If {
            condition,
            then_block,
            else_block,
            ..
        } => {
            let mut body_block = BasicBlock::new(next_bblock_id()); // the block that starts the body
            cfg.add_block(&body_block);
            let next_block: BasicBlock; // the block after the whole if
            
            if else_block.is_some() {
                let mut else_body_block = BasicBlock::new(next_bblock_id());
                next_block = BasicBlock::new(next_bblock_id());
                cfg.add_block(&else_body_block);
                cfg.add_block(&next_block);
                build_cond(cfg, condition, cur_block, &body_block, &else_body_block);

                for statement in then_block.statements.clone() {
                    body_block = destruct_statement(cfg, &mut body_block, statement, cur_loop);
                }
                // jump to next block
                body_block.add_instruction(Instruction::UJmp { id: next_block.get_id() });

                // same for else body if we have one
                for statement in else_block.clone().unwrap().statements.clone() {
                    else_body_block = destruct_statement(cfg, &mut else_body_block, statement, cur_loop);
                }
                body_block.add_instruction(Instruction::UJmp { id: next_block.get_id() });
            } else {
                next_block = BasicBlock::new(next_bblock_id());
                cfg.add_block(&next_block);
                build_cond(cfg, condition, cur_block, &body_block, &next_block);

                for statement in then_block.statements.clone() {
                    body_block = destruct_statement(cfg, &mut body_block, statement, cur_loop);
                }
                // jump to next block
                body_block.add_instruction(Instruction::UJmp { id: next_block.get_id() });
            }

            next_block
        },
        SymStatement::While { condition, block, span } => {
            let header_id = next_bblock_id();
            let mut header_block = BasicBlock::new(header_id); // the block that evaluates the loop condition
            let mut body_block = BasicBlock::new(next_bblock_id()); // the block that starts the body
            let next_block = BasicBlock::new(next_bblock_id()); // the block after the whole loop
            cfg.add_block(&header_block);
            cfg.add_block(&body_block);
            cfg.add_block(&next_block);

            cur_block.add_instruction(Instruction::UJmp { id: header_block.get_id() });

            build_cond(cfg, condition, &mut header_block, &body_block, &next_block);

            let next_loop = Loop { break_to: next_block.get_id(), continue_to: header_id };

            for statement in block.statements.clone() {
                body_block = destruct_statement(cfg, &mut body_block, statement, &Some(next_loop.clone()));
            }

            // jump back to header block after the body
            body_block.add_instruction(Instruction::UJmp { id: header_id });

            next_block
        }
        SymStatement::For { var, init, condition, update, block, .. } => {
            let header_id = next_bblock_id();
            let mut header_block = BasicBlock::new(header_id); // the block that evaluates the loop condition
            let mut body_block = BasicBlock::new(next_bblock_id()); // the block that starts the body
            let next_block = BasicBlock::new(next_bblock_id()); // the block after the whole loop
            cfg.add_block(&header_block);
            cfg.add_block(&body_block);
            cfg.add_block(&next_block);

            // evaluate the for_init
            let lhs = Operand::Id(var.clone());
            let rhs: Operand;
            (*cur_block, rhs) = destruct_expr(cfg, init, &mut cur_block);
            cur_block.add_instruction(Instruction::Assign {
                src: Box::new(rhs),
                dest: Box::new(lhs),
            });

            cur_block.add_instruction(Instruction::UJmp { id: header_block.get_id() });

            build_cond(cfg, condition, &mut header_block, &body_block, &next_block);

            let next_loop = Loop { break_to: next_block.get_id(), continue_to: header_id };

            for statement in block.statements.clone() {
                body_block = destruct_statement(cfg, &mut body_block, statement, &Some(next_loop.clone()));
            }

            // execute the for_update
            body_block = destruct_statement(cfg, &mut body_block, Rc::from(*(update.clone())), &Some(next_loop));

            // jump back to header block after the body
            body_block.add_instruction(Instruction::UJmp { id: header_id });

            next_block
        }
        SymStatement::Break { .. } => {
            cur_block.add_instruction(Instruction::UJmp { id: cur_loop.clone().unwrap().break_to });

            // all code after the break is unreachable; just return an empty basic block.
            // no need to add it to the CFG because there are no edges into it.
            BasicBlock::new(0)
        }
        SymStatement::Continue { .. } => {
            cur_block.add_instruction(Instruction::UJmp { id: cur_loop.clone().unwrap().continue_to });

            // all code after the continue is unreachable; just return an empty basic block.
            // no need to add it to the CFG because there are no edges into it.
            BasicBlock::new(0)
        }
        SymStatement::Return { expr, .. } => {
            if expr.is_some() {
                let operand: Operand;
                (*cur_block, operand) = destruct_expr(cfg, &expr.clone().unwrap(), &mut cur_block);
                cur_block.add_instruction(Instruction::Ret { value: Some(Box::new(operand)) });
            } else {
                cur_block.add_instruction(Instruction::Ret { value: None });
            }

            // all code after the return is unreachable; just return an empty basic block.
            // no need to add it to the CFG because there are no edges into it.
            BasicBlock::new(0)
        }
        SymStatement::VarDecl { .. } => {
            // no need to do anything; default values are undefined
            cur_block.clone()
        }
        _ => unreachable!()
    }
}

/// Destruct a method AST node into basic blocks
fn destruct_method(method: &Rc<SymMethod>) -> CFG {
    // reset counters for each CFG
    reset_counters();

    let mut method_cfg = CFG::new();

    let entry_block = BasicBlock::new(next_bblock_id()); // should have ID 0
    method_cfg.add_block(&entry_block);
    let mut cur_block = entry_block;

    for statement in method.body.statements.clone() {
        cur_block = destruct_statement(&mut method_cfg, &mut cur_block, statement, &None);
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
