/*
Linearize the Parse Tree to create a linear 
IR that can be used for code generation.

The linear IR is an ordered vector of instructions,
which follow similarly to the three-address code format.
*/

use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::scope::TableEntry;
use crate::symtable::{SymMethod, SymProgram, SymStatement, SymExpr};
use crate::linear_ir::*;
use crate::semcheck::semcheck;
use crate::cfg::*;
use crate::ast::*;
use crate::token::Literal;


thread_local! {
    static TEMP_COUNTER: RefCell<usize> = RefCell::new(0);
}

fn fresh_temp() -> String {
    TEMP_COUNTER.with(|counter| {
        let mut count = counter.borrow_mut();
        let temp_name = format!("_t{}", *count);
        *count += 1;
        temp_name
    })
}

// Helper to convert literal or identifier expressions into operands
fn expr_to_operand(expr: &SymExpr, block: &mut BasicBlock) -> Operand {
    match expr {
        SymExpr::Identifier { entry, .. } => {
            match &*entry {
                TableEntry::Variable { name, .. } => Operand::Id(name.clone()),
                TableEntry::Method { name, .. } => Operand::Id(name.clone()),
                TableEntry::Import { name, .. } => Operand::Id(name.clone()),
            }
        }

        SymExpr::Literal {value, ..} => Operand::Const(value.clone()),

        SymExpr::ArrAccess { id, index, span } => {
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

        SymExpr::MethodCall { method_name, args, .. } => {
            let arg_ops: Vec<Operand> = args.iter()
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

        SymExpr::BinaryExpr { op, left, right, .. } => {
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
                BinaryOp::And => Instruction::And { // TODO: Short circuit for conditionals like And and Or
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

        SymExpr::UnaryExpr { op, expr, .. } => {
            let result = Operand::Id(fresh_temp());
            let right_operand = expr_to_operand(expr, block);

            let instruction: Instruction = match op {
                UnaryOp::Neg => Instruction::Subtract { 
                    left: Box::new(Operand::Const(Literal::Int("0".to_string()))), 
                    right: Box::new(right_operand), 
                    dest: Box::new(result.clone()) 
                },
                UnaryOp::Not => todo!(),
            };

            block.add_instruction(instruction);
            result
        }

        // SynExpr::Cast
        _ => Operand::Id(fresh_temp()),
    }
}


fn destruct_statement(cfg: &mut CFG, statement: Rc<SymStatement>) {
    let mut block: BasicBlock = BasicBlock::new();

    match &*statement {
        SymStatement::Assignment { target, expr, op, .. } => {
            let dest = expr_to_operand(target, &mut block);

            // Expr could be two things, a literal/identifier, in which its just returned as the operand.
            // If expr requires more computation and is 
            let rhs_op = expr_to_operand(expr, &mut block); 

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

            block.add_instruction(instr);
        }

        SymStatement::MethodCall { method_name, args, .. } => {
            let arg_ops: Vec<Operand> = args.iter().map(|arg| expr_to_operand(arg, &mut block)).collect();

            block.add_instruction(Instruction::MethodCall {
                name: method_name.clone(),
                args: arg_ops,
                dest: None, // or Some(Box::new(...)) if used as value
            });
        }

        _ => {
            println!("Statement not handled in destruct_statement: {:?}", statement);
        }
    }

    // For now just use block ID 0 (you'll likely want to generate these dynamically) // TODO!!!
    cfg.add_block(0, block);
    // TODO ADD EDGE FOr the block
}


fn destruct_method(method: &Rc<SymMethod>) -> CFG {
    let mut method_cfg = CFG::new();

    for statement in method.body.statements.clone() {
        destruct_statement(&mut method_cfg, statement);
    }

    method_cfg
}

fn destruct_program(program: SymProgram) -> HashMap<String, CFG> {
    let mut method_cfgs: HashMap<String, CFG> = HashMap::new();     // Hash map to map method names to associated CFG

    for (name, method) in &program.methods {
        let cfg: CFG = destruct_method(method);
        method_cfgs.insert(name.clone(), cfg);
    }

    method_cfgs
}


pub fn assemble(
    file: &str,
    filename: &str,
    writer: &mut dyn std::io::Write,
    debug: bool
) {
    // Create symbol table IR
    let sym_tree: SymProgram = semcheck(file, filename, writer, debug);

    let method_cfgs: HashMap<String, CFG> = destruct_program(sym_tree);

    // Use the CFGS to linearize all blocks starting from main 
    
    // Convert linear blocks into x86 Assembly
}
