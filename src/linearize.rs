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
fn expr_to_operand(expr: &SymExpr) -> Operand {
    match expr {
        SymExpr::Identifier { entry, .. } => {
            match &*entry {
                TableEntry::Variable { name, .. } => Operand::Var(name.clone()),
                TableEntry::Method { name, .. } => Operand::Var(name.clone()),
                TableEntry::Import { name, .. } => Operand::Var(name.clone()),
            }
        }
        SymExpr::Literal {value, ..} => Operand::Const, // TODO encode actual constant value here
        _ => Operand::Temp(fresh_temp()),
    }
}


fn destruct_statement(mut cfg: CFG, statement: Rc<SymStatement>) {
    let mut block = BasicBlock::new();

    match &*statement {
        SymStatement::Assignment { target, expr, op, .. } => {
            let lhs_op = expr_to_operand(target);
            let rhs_op = expr_to_operand(expr);
            let dest = match &lhs_op {
                Operand::Temp(_) => Operand::Temp(fresh_temp()),
                _ => lhs_op.clone(),
            };

            let instr = match op {
                AssignOp::Assign => Instruction::Assign {
                    src: Box::new(rhs_op),
                    dest: Box::new(dest.clone()),
                },
                AssignOp::PlusAssign => Instruction::Add {
                    left: Box::new(lhs_op),
                    right: Box::new(rhs_op),
                    dest: Box::new(dest.clone()),
                },
                AssignOp::MinusAssign => Instruction::Subtract {
                    left: Box::new(lhs_op),
                    right: Box::new(rhs_op),
                    dest: Box::new(dest.clone()),
                },
                AssignOp::MultiplyAssign => Instruction::Multiply {
                    left: Box::new(lhs_op),
                    right: Box::new(rhs_op),
                    dest: Box::new(dest.clone()),
                },
                AssignOp::DivideAssign => Instruction::Divide {
                    left: Box::new(lhs_op),
                    right: Box::new(rhs_op),
                    dest: Box::new(dest.clone()),
                },
                AssignOp::ModuloAssign => Instruction::Modulo {
                    left: Box::new(lhs_op),
                    right: Box::new(rhs_op),
                    dest: Box::new(dest.clone()),
                },
            };

            block.add_instruction(instr);
        }

        SymStatement::MethodCall { method_name, args, .. } => {
            let arg_ops: Vec<Operand> = args.iter().map(|arg| expr_to_operand(arg)).collect();

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
}


fn destruct_method(method: &Rc<SymMethod>) -> CFG {
    let mut method_cfg = CFG::new();

    for statement in method.body.statements.clone() {
        destruct_statement(method_cfg.clone(), statement);
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
