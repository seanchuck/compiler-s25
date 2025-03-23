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
use core::panic;
use crate::ast::*;
use crate::cfg::*;
use crate::linear_ir::*;
use crate::scope::TableEntry;
use crate::semcheck::semcheck;
use crate::symtable::{SymBlock, SymExpr, SymMethod, SymProgram, SymStatement};


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

// Returns a new id for the next basic block
fn next_bblock_id() -> i32 {
    BBLOCK_COUNTER.with(|counter| {
        let mut count = counter.borrow_mut();
        *count += 1;
        *count as i32
    })
}

/// Helper to convert literal or identifier expressions into operands
fn expr_to_operand(expr: &SymExpr, block: &mut BasicBlock) -> Operand {
    match expr {
        SymExpr::Identifier { entry, .. } => match &*entry {
            TableEntry::Variable { name, .. } => Operand::Id(name.clone()),
            TableEntry::Method { name, .. } => Operand::Id(name.clone()),
            TableEntry::Import { name, .. } => Operand::Id(name.clone()),
        },

        SymExpr::Literal { value, .. } => Operand::Const(value.clone()),

        SymExpr::ArrAccess { id, index, .. } => {
            // Load the array into a temp and then return the temp's operand
            let temp_arr_val = Operand::Id(fresh_temp());

            let index_operand = match index.as_ref() {
                SymExpr::Literal { value, .. } => Operand::Const(value.clone()),
                _ => panic!("Expected literal in array index"),
            };

            block.add_instruction(Instruction::ArrAccess {
                array: Box::new(Operand::Id(id.clone())),
                index: Box::new(index_operand),
                dest: Box::new(temp_arr_val.clone()),
            });

            temp_arr_val
        }

        SymExpr::MethodCall {
            method_name, args, ..
        } => {
            let arg_ops: Vec<Operand> = args
                .iter()
                .map(|arg| match &**arg {
                    SymExpr::Identifier { entry, .. } => match &*entry {
                        TableEntry::Variable { name, .. } => Operand::Id(name.clone()),
                        _ => panic!("Expected a variable identifier in method call arguments"),
                    },
                    _ => panic!("Expected only Identifier in method call arguments"),
                })
                .collect();

            let temp_method_result = Operand::Id(fresh_temp());

            block.add_instruction(Instruction::MethodCall {
                name: method_name.clone(),
                args: arg_ops,
                dest: Some(Box::new(temp_method_result.clone())),
            });

            temp_method_result
        }

        SymExpr::BinaryExpr {
            op, left, right, ..
        } => {
            let left_operand = expr_to_operand(left, block);
            let right_operand = expr_to_operand(right, block);
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
                // Short-circuiting handled elsewhere
                BinaryOp::And => Instruction::And {
                    left: Box::new(left_operand),
                    right: Box::new(right_operand),
                    dest: Box::new(result.clone()),
                },
                BinaryOp::Or => Instruction::Or {
                    left: Box::new(left_operand),
                    right: Box::new(right_operand),
                    dest: Box::new(result.clone()),
                },
            };

            block.add_instruction(instruction);
            result
        }

        // SymExpr::UnaryExpr { op, expr, span } => {

        // }
        _ => Operand::Id(fresh_temp()),
    }
}

/// Destruct a statement into basic blocks and add them to the method CFG.
/// Returns the (begin_bblock, end_bblock), where end_bblock is some
/// only if the statement spans multiple basic blocks.
fn destruct_statement(cfg: &mut CFG, statement: Rc<SymStatement>) -> (BasicBlock, BasicBlock) {
    // For now, destruct_statement will have responsibility to add any blocks to the CFG

    match &*statement {
        SymStatement::Assignment {
            target, expr, op, ..
        } => {
            let mut bblock: BasicBlock = BasicBlock::new(next_bblock_id());
            let dest = expr_to_operand(target, &mut bblock);

            // Expr could be two things, a literal/identifier, in which its just returned as the operand.
            // If expr requires more computation and is
            let rhs_op = expr_to_operand(expr, &mut bblock);

            let instr = match op {
                AssignOp::Assign => Instruction::Assign {
                    src: Box::new(rhs_op),
                    dest: Box::new(dest.clone()),
                },
                AssignOp::PlusAssign => Instruction::Add {
                    left: Box::new(dest.clone()),
                    right: Box::new(rhs_op),
                    dest: Box::new(dest.clone()),
                },
                AssignOp::MinusAssign => Instruction::Subtract {
                    left: Box::new(dest.clone()),
                    right: Box::new(rhs_op),
                    dest: Box::new(dest.clone()),
                },
                AssignOp::MultiplyAssign => Instruction::Multiply {
                    left: Box::new(dest.clone()),
                    right: Box::new(rhs_op),
                    dest: Box::new(dest.clone()),
                },
                AssignOp::DivideAssign => Instruction::Divide {
                    left: Box::new(dest.clone()),
                    right: Box::new(rhs_op),
                    dest: Box::new(dest.clone()),
                },
                AssignOp::ModuloAssign => Instruction::Modulo {
                    left: Box::new(dest.clone()),
                    right: Box::new(rhs_op),
                    dest: Box::new(dest.clone()),
                },
            };

            bblock.add_instruction(instr);
            cfg.add_block(&bblock);
            (bblock.clone(), bblock) // TODO: verify this is ok. Since we add and index bblocks
                                     // from CFG using bblock id, I think it should be fine.
        }

        SymStatement::MethodCall {
            method_name, args, ..
        } => {
            let mut bblock: BasicBlock = BasicBlock::new(next_bblock_id());
            let arg_ops: Vec<Operand> = args
                .iter()
                .map(|arg| expr_to_operand(arg, &mut bblock))
                .collect();

            bblock.add_instruction(Instruction::MethodCall {
                name: method_name.clone(),
                args: arg_ops,
                dest: None, // or Some(Box::new(...)) if used as value
            });

            cfg.add_block(&bblock);
            (bblock.clone(), bblock)
        }

        // Must handle short-circuiting conditionals
        SymStatement::If {
            condition,
            then_block,
            else_block,
            ..
        } => {
            // TODO: hope is that recursion will add all intermediate blocks, is this true?
            let (true_begin, true_end) = destruct_block(cfg, then_block);
            let (false_begin, false_end) = match else_block {
                Some(unwrapped_else) => destruct_block(cfg, unwrapped_else),
                None => {
                    // If there is no else, it is essentially a nop
                    let mut nop_bblock = BasicBlock::new(next_bblock_id());
                    nop_bblock.add_instruction(Instruction::Nop);
                    cfg.add_block(&nop_bblock);

                    (nop_bblock.clone(), nop_bblock)
                }
            };

            return short_circuit(cfg, condition, &true_begin, &true_end, &false_begin, &false_end);

        },
        _ => { panic!("Failed to destruct statement: Unexpected statement type"); }
    }
}


// TODO: add blocks to CFG where necessary. 
fn short_circuit(
    cfg: &mut CFG,
    condition: &SymExpr,
    true_begin: &BasicBlock,
    true_end: &BasicBlock,
    false_begin: &BasicBlock,
    false_end: &BasicBlock,
) -> (BasicBlock, BasicBlock) {
    match condition {
        // No short-circuiting necessary
        SymExpr::Identifier { entry, .. } => match &*entry {
            TableEntry::Variable { name, .. } => {
                let mut bblock = BasicBlock::new(next_bblock_id());
                cfg.add_block(&bblock);
                let instr = Instruction::CJmp {
                    condition: (Box::new(Operand::Id(name.clone()))),
                    label: ("placeholder").to_string(),
                };
                bblock.add_instruction(instr);

                // Diverge
                cfg.add_edge(bblock.get_id(), true_begin.get_id());
                cfg.add_edge(bblock.get_id(), false_begin.get_id());

                // Converge
                let mut nop_bblock = BasicBlock::new(next_bblock_id());
                nop_bblock.add_instruction(Instruction::Nop);
                cfg.add_block(&nop_bblock);

                cfg.add_edge(true_end.get_id(), nop_bblock.get_id());
                cfg.add_edge(false_end.get_id(), nop_bblock.get_id());

                return (bblock, nop_bblock);
            }
            _ => { panic!("Short-circuiting undefined for Method or Import"); }
        },

        SymExpr::Literal { .. }
        | SymExpr::MethodCall { .. } 
        | SymExpr::ArrAccess { .. } => {
            let mut bblock = BasicBlock::new(next_bblock_id());
            cfg.add_block(&bblock);

            let operand = expr_to_operand(condition, &mut bblock);

            let instr = Instruction::CJmp {
                condition: Box::new(operand),
                label: ("placeholder".to_string()),
            };
            bblock.add_instruction(instr);

            // Diverge
            cfg.add_edge(bblock.get_id(), true_begin.get_id());
            cfg.add_edge(bblock.get_id(), false_begin.get_id());

            // Converge 
            let mut nop_bblock = BasicBlock::new(next_bblock_id());
            nop_bblock.add_instruction(Instruction::Nop);
            cfg.add_block(&nop_bblock);

            cfg.add_edge(true_end.get_id(), nop_bblock.get_id());
            cfg.add_edge(false_end.get_id(), nop_bblock.get_id());

            return (bblock, nop_bblock);
        },

        // Short-circuiting required
        SymExpr::UnaryExpr { op, expr, .. } => {
            match op {
                UnaryOp::Not => {
                    // Introduce "Not" by flipping true and false blocks
                    return short_circuit(cfg, expr, false_begin, false_end, true_begin, true_end);
                }
                _=> { panic!("Short-circuiting undefined for neg!"); }
            }
        },
        SymExpr::BinaryExpr {
            op, left, right, .. } => {
            let (left_begin, left_end): (BasicBlock, BasicBlock);
            let (right_begin, right_end): (BasicBlock, BasicBlock);

            match op {
                BinaryOp::And => {
                    // If left succeeds, enter right; demonic
                    (right_begin, right_end) =
                        short_circuit(cfg, &right, true_begin, true_end, false_begin, false_end);
                    (left_begin, left_end) = 
                        short_circuit(cfg, &left, &right_begin, &right_end, false_begin, false_end);
                }
                BinaryOp::Or => {
                    // If left fails, enter right; angelic
                    (right_begin, right_end) =
                        short_circuit(cfg, &right, true_begin, true_end, false_begin, false_end);
                    (left_begin, left_end) = 
                        short_circuit(cfg, &left, true_begin, true_end, &right_begin, &right_end);
                }
                _ => { panic!("Short-circuiting only allowed on BinaryOp::AND or BinaryOp::OR") }
            }

            // Converge short-circuiting with a nop block
            let mut nop_bblock = BasicBlock::new(next_bblock_id());
            nop_bblock.add_instruction(Instruction::Nop);
            cfg.add_block(&nop_bblock);

            cfg.add_edge(left_end.get_id(), nop_bblock.get_id());
            cfg.add_edge(right_end.get_id(), nop_bblock.get_id());

            return (left_begin, nop_bblock);
        },
        _ => { panic!("Short-circuiting not defined for expression {:?}", condition); }
    }
}


fn destruct_block(cfg: &mut CFG, block: &Rc<SymBlock>) -> (BasicBlock, BasicBlock) {
    let mut begin_bblock: Option<BasicBlock> = None;
    let mut end_bblock: Option<BasicBlock> = None;

    // Iteratively destruct statements, linking them as you go
    for statement in block.statements.clone() {
        let (begin_stmt, end_stmt) = destruct_statement(cfg, statement);

        if begin_bblock.is_none() {
            // Assumes these blocks were already added to CFG by destruct_statement
            begin_bblock = Some(begin_stmt.clone());
            end_bblock = Some(begin_stmt);
        }

        // Link blocks: end of prev is parent to start of next
        cfg.add_edge(end_bblock.unwrap().get_id(), end_stmt.get_id());

        // Update end_bblock of the block we are destructing
        end_bblock = Some(end_stmt);
    }

    if begin_bblock.is_some() {
        (begin_bblock.unwrap(), end_bblock.unwrap())
    } else {
        // Weird case where block has no statements inside of it,
        // so we return nop block
        let bblock_id = next_bblock_id();
        let mut nop_bblock = BasicBlock::new(bblock_id);
        cfg.add_block(&nop_bblock);

        nop_bblock.add_instruction(Instruction::Nop);
        (nop_bblock.clone(), nop_bblock)
    }
}

/// Destruct a method AST node into basic blocks
fn destruct_method(method: &Rc<SymMethod>) -> CFG {
    let mut method_cfg = CFG::new();

    for statement in method.body.statements.clone() {
        destruct_statement(&mut method_cfg, statement);
        // TODO: link the blocks together
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
