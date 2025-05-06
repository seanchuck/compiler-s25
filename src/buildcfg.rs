/**
Construct a control flow graph (CFG) from the
symbol table IR.
**/
use crate::{
    ast::*,
    cfg::*,
    scope::{Scope, TableEntry},
    semcheck::semcheck,
    symtable::{SemanticContext, SymExpr, SymMethod, SymProgram, SymStatement},
    tac::*,
    token::{Literal, Span},
    traverse::infer_expr_type,
};
use std::{collections::BTreeMap, io};
use std::{cell::RefCell, collections::HashMap, rc::Rc};

const UNREACHABLE_BLOCK: i32 = -1; // id for an unreachable block

// Initialize a counter for naming temps and indexing basic blocks
thread_local! {
    static TEMP_COUNTER: RefCell<usize> = RefCell::new(0);
    static BBLOCK_COUNTER: RefCell<usize> = RefCell::new(0);
    static VAR_COUNTERS: RefCell<HashMap<String, usize>> = RefCell::new(HashMap::new());
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

/// Create a new unique version of a named variable like "b" -> "_b1"
fn fresh_var(base: &str) -> String {
    VAR_COUNTERS.with(|map| {
        let mut counters = map.borrow_mut();
        let count = counters.entry(base.to_string()).or_insert(0);
        let var_name: String;
        if *count as i32 > 0 {
            var_name = format!("_{}{}", base, count);
        } else {
            var_name = format!("{}", base);
        }
        *count += 1;
        var_name
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
fn destruct_expr(
    cfg: &mut CFG,
    expr: &SymExpr,
    mut cur_block_id: i32,
    cfg_scope: &CFGScope,
    sym_scope: &Rc<RefCell<Scope>>,
    strings: &mut Vec<String>,
) -> (i32, Operand) {
    match expr {
        SymExpr::Literal { value, .. } => match value {
            Literal::String(val) => {
                let temp = fresh_temp();
                let temp_op = Operand::LocalVar { name: temp.clone(), typ: Type::String, reg: None};
                cfg.add_temp_var(temp, Type::String, None);

                cfg.add_instruction_to_block(
                    cur_block_id,
                    Instruction::LoadString {
                        src: Operand::String { id: strings.len().try_into().unwrap(), typ: Type::String, reg: None },
                        dest: temp_op.clone(),
                    },
                );
                strings.push(val.to_string());
                (cur_block_id, temp_op)
            }
            Literal::Bool(val) => match val {
                true => (cur_block_id, Operand::Const { value: 1, typ: Type::Bool, reg: None}),
                false => (cur_block_id, Operand::Const { value: 0, typ: Type::Bool, reg: None})
            },
            Literal::Int(val) => {
                let temp = fresh_temp();
                let temp_op = Operand::LocalVar { name: temp.clone(), typ: Type::Int, reg: None };
                cfg.add_temp_var(temp, Type::Int, None);

                cfg.add_instruction_to_block(
                    cur_block_id,
                    Instruction::LoadConst {
                        src: val.parse::<i64>().unwrap(),
                        dest: temp_op.clone(),
                        typ: Type::Int,
                    },
                );

                (cur_block_id, temp_op)
            }

            Literal::HexInt(val) => {
                let temp = fresh_temp();
                let temp_op = Operand::LocalVar { name: temp.clone(), typ: Type::Int, reg: None };
                cfg.add_temp_var(temp, Type::Int, None);

                cfg.add_instruction_to_block(
                    cur_block_id,
                    Instruction::LoadConst {
                        src: i64::from_str_radix(&val, 16).unwrap(),
                        dest: temp_op.clone(),
                        typ: Type::Int,
                    },
                );

                (cur_block_id, temp_op)
            }
            Literal::Long(val) => {
                let temp = fresh_temp();
                let temp_op = Operand::LocalVar { name: temp.clone(), typ: Type::Long, reg: None };
                cfg.add_temp_var(temp, Type::Long, None);

                cfg.add_instruction_to_block(
                    cur_block_id,
                    Instruction::LoadConst {
                        src: val.parse::<i64>().unwrap(),
                        dest: temp_op.clone(),
                        typ: Type::Long,
                    },
                );

                (cur_block_id, temp_op)
            }

            Literal::HexLong(val) => {
                let temp = fresh_temp();
                let temp_op = Operand::LocalVar { name: temp.clone(), typ: Type::Long, reg: None };
                cfg.add_temp_var(temp, Type::Long, None);

                cfg.add_instruction_to_block(
                    cur_block_id,
                    Instruction::LoadConst {
                        src: i64::from_str_radix(&val, 16).unwrap(),
                        dest: temp_op.clone(),
                        typ: Type::Long,
                    },
                );

                (cur_block_id, temp_op)
            }
            Literal::Char(val) => (cur_block_id, Operand::Const { value: *val as i64, typ: Type::Long, reg: None }),  //TODO what shoudl char type be?
        },

        SymExpr::Identifier { entry, .. } => match entry {
            TableEntry::Variable { name, typ, length: _, span: _ } => {
                (cur_block_id, cfg_scope.lookup_var(name.to_string(), typ.clone(), None))
            }
            _ => unreachable!(),
        },

        SymExpr::ArrAccess { id, index, .. } => {
            let arr_entry = sym_scope.borrow().lookup(id).expect("Array not found in scope!");
            let arr_typ = if let TableEntry::Variable { typ, .. } = arr_entry {
                typ
            } else {
                panic!("Expected a variable, found something else!");
            };

            // Load array element into a temp
            let temp = fresh_temp();
            let array_element = Operand::LocalVar { name: temp.to_string(), typ: arr_typ.clone(), reg: None };

            let table_entry = sym_scope
                .borrow()
                .lookup(id)
                .expect("Array not found in scope!");
            let TableEntry::Variable { typ, .. } = table_entry else {
                panic!("Expected a variable, found something else!");
            };

            cfg.add_temp_var(temp, typ.clone(), None);

            // array index can be an expression
            let (next_block_id, index_operand) =
                destruct_expr(cfg, index, cur_block_id, cfg_scope, sym_scope, strings);

            cfg.add_instruction_to_block(
                next_block_id,
                Instruction::Assign {
                    typ: typ,
                    src: cfg_scope.lookup_arr(id.to_string(), index_operand, sym_scope, arr_typ.clone()),
                    dest: array_element.clone(),
                },
            );

            (next_block_id, array_element)
        }

        SymExpr::MethodCall {
            method_name, args, ..
        } => {
            // arguments can be expressions
            let mut arg_ops: Vec<Operand> = Vec::new();
            for arg in args {
                let arg_op;
                (cur_block_id, arg_op) =
                    destruct_expr(cfg, arg, cur_block_id, cfg_scope, sym_scope, strings);
                arg_ops.push(arg_op);
            }

            let method_entry = sym_scope.borrow().lookup(method_name).expect("Couldnt find called method");
            let return_type = match method_entry {
                TableEntry::Method { name: _, return_type, .. } => return_type,
                TableEntry::Import { .. } => Type::Int, //All external functions are treated as if they return int
                _ => panic!("cannot call non method or import")
            };

            let temp = fresh_temp();
            let temp_method_result = Operand::LocalVar { name: temp.to_string(), typ: return_type.clone(), reg: None };

            cfg.add_temp_var(temp, return_type.clone(), None);

            cfg.add_instruction_to_block(
                cur_block_id,
                Instruction::MethodCall {
                    name: method_name.clone(),
                    args: arg_ops,
                    dest: Some(temp_method_result.clone()),
                    return_type,
                },
            );

            (cur_block_id, temp_method_result)
        }

        SymExpr::BinaryExpr {
            op, left, right, ..
        } => {
            match op {
                // short-circuiting (from recitation 6)
                BinaryOp::And | BinaryOp::Or => {
                    let temp = fresh_temp();
                    let dest = Operand::LocalVar { name: temp.to_string(), typ: Type::Bool, reg: None };
                    cfg.add_temp_var(temp, Type::Bool, None);
                    let next_true_block_id = next_bblock_id();
                    let next_true_block = BasicBlock::new(next_true_block_id);
                    let next_false_block_id = next_bblock_id();
                    let next_false_block = BasicBlock::new(next_false_block_id);
                    let next_block_id = next_bblock_id();
                    let next_block: BasicBlock = BasicBlock::new(next_block_id);

                    cfg.add_block(&next_true_block);
                    cfg.add_block(&next_false_block);
                    cfg.add_block(&next_block);

                    // if condition is true, set dest = 1 and jump to next_block
                    cfg.add_instruction_to_block(
                        next_true_block_id,
                        Instruction::Assign {
                            typ: Type::Bool,
                            src: Operand::Const { value: 1, typ: Type::Bool, reg: None },
                            dest: dest.clone(),
                        },
                    );
                    cfg.add_instruction_to_block(
                        next_true_block_id,
                        Instruction::UJmp {
                            name: cfg.name.clone(),
                            id: next_block_id,
                        },
                    );

                    // if condition is false, set dest = 0 and jump to next_block
                    cfg.add_instruction_to_block(
                        next_false_block_id,
                        Instruction::Assign {
                            typ: Type::Bool,
                            src: Operand::Const { value: 0, typ: Type::Bool, reg: None },
                            dest: dest.clone(),
                        },
                    );
                    cfg.add_instruction_to_block(
                        next_false_block_id,
                        Instruction::UJmp {
                            name: cfg.name.clone(),
                            id: next_block_id,
                        },
                    );

                    build_cond(
                        cfg,
                        expr,
                        cur_block_id,
                        next_true_block_id,
                        next_false_block_id,
                        cfg_scope,
                        sym_scope,
                        strings,
                    );

                    // add these blocks after the condition blocks are added
                    cfg.add_block_order(next_true_block_id);
                    cfg.add_block_order(next_false_block_id);
                    cfg.add_block_order(next_block_id);

                    cfg.add_edge(next_true_block_id, next_block_id, EdgeType::Unconditional);
                    cfg.add_edge(next_false_block_id, next_block_id, EdgeType::Unconditional);

                    return (next_block_id, dest);
                }
                _ => {
                    let left_operand: Operand;
                    let right_operand: Operand;
                    (cur_block_id, left_operand) =
                        destruct_expr(cfg, left, cur_block_id, cfg_scope, sym_scope, strings);
                    (cur_block_id, right_operand) =
                        destruct_expr(cfg, right, cur_block_id, cfg_scope, sym_scope, strings);

                    // Infer type of result
                    let mut dummy_context: SemanticContext = SemanticContext {
                        filename: "dummy".to_string(),
                        error_found: false,
                    };
                    let mut dummy_writer = io::sink();
                    let left_type: Type = infer_expr_type(
                        left,
                        &sym_scope.borrow(),
                        &mut dummy_writer,
                        &mut dummy_context,
                    )
                    .expect("Left type not defined");
                    let right_type: Type = infer_expr_type(
                        right,
                        &sym_scope.borrow(),
                        &mut dummy_writer,
                        &mut dummy_context,
                    )
                    .expect("Right type not defined");
                    assert_eq!(left_type, right_type, "Left and right types must match");

                    let temp = fresh_temp();
                    let result = Operand::LocalVar { name: temp.to_string(), typ: left_type.clone(), reg: None };

                    cfg.add_temp_var(temp, left_type.clone(), None);

                    let instruction: Instruction = match op {
                        BinaryOp::Add => Instruction::Add {
                            typ: left_type,
                            left: left_operand,
                            right: right_operand,
                            dest: result.clone(),
                        },
                        BinaryOp::Subtract => Instruction::Subtract {
                            typ: left_type,
                            left: left_operand,
                            right: right_operand,
                            dest: result.clone(),
                        },
                        BinaryOp::Multiply => Instruction::Multiply {
                            typ: left_type,
                            left: left_operand,
                            right: right_operand,
                            dest: result.clone(),
                        },
                        BinaryOp::Divide => Instruction::Divide {
                            typ: left_type,
                            left: left_operand,
                            right: right_operand,
                            dest: result.clone(),
                        },
                        BinaryOp::Modulo => Instruction::Modulo {
                            typ: left_type,
                            left: left_operand,
                            right: right_operand,
                            dest: result.clone(),
                        },
                        BinaryOp::Less => Instruction::Less {
                            left: left_operand,
                            right: right_operand,
                            dest: result.clone(),
                        },
                        BinaryOp::Greater => Instruction::Greater {
                            left: left_operand,
                            right: right_operand,
                            dest: result.clone(),
                        },
                        BinaryOp::LessEqual => Instruction::LessEqual {
                            left: left_operand,
                            right: right_operand,
                            dest: result.clone(),
                        },
                        BinaryOp::GreaterEqual => Instruction::GreaterEqual {
                            left: left_operand,
                            right: right_operand,
                            dest: result.clone(),
                        },
                        BinaryOp::Equal => Instruction::Equal {
                            left: left_operand,
                            right: right_operand,
                            dest: result.clone(),
                        },
                        BinaryOp::NotEqual => Instruction::NotEqual {
                            left: left_operand,
                            right: right_operand,
                            dest: result.clone(),
                        },
                        _ => unreachable!(),
                    };

                    cfg.add_instruction_to_block(cur_block_id, instruction);

                    (cur_block_id, result)
                }
            }
        }

        SymExpr::UnaryExpr { op, expr, .. } => {
            // Infer type of result
            let mut dummy_context: SemanticContext = SemanticContext {
                filename: "dummy".to_string(),
                error_found: false,
            };
            let mut dummy_writer = io::sink();
            let inferred_type: Type = infer_expr_type(
                expr,
                &sym_scope.borrow(),
                &mut dummy_writer,
                &mut dummy_context,
            )
            .expect("type not defined");

            let temp = fresh_temp();
            let result = Operand::LocalVar { name: temp.to_string(), typ: inferred_type.clone(), reg: None };

            cfg.add_temp_var(temp, inferred_type.clone(), None);
            let operand: Operand;
            (cur_block_id, operand) =
                destruct_expr(cfg, expr, cur_block_id, cfg_scope, sym_scope, strings);

            let instruction: Instruction = match op {
                UnaryOp::Neg => Instruction::Subtract {
                    typ: inferred_type.clone(),
                    left: Operand::Const { value: 0, typ: inferred_type.clone(), reg: None },
                    right: operand,
                    dest: result.clone(),
                },
                UnaryOp::Not => Instruction::Not {
                    expr: operand,
                    dest: result.clone(),
                },
            };

            cfg.add_instruction_to_block(cur_block_id, instruction);

            (cur_block_id, result)
        }

        SymExpr::Cast {
            target_type, expr, ..
        } => {
            let temp = fresh_temp();
            let result = Operand::LocalVar { name: temp.to_string(), typ: target_type.clone(), reg: None };
            cfg.add_temp_var(temp, target_type.clone(), None);

            let operand: Operand;
            (cur_block_id, operand) =
                destruct_expr(cfg, expr, cur_block_id, cfg_scope, sym_scope, strings);

            let instruction = Instruction::Cast {
                expr: operand,
                dest: result.clone(),
                target_type: target_type.clone(),
            };

            cfg.add_instruction_to_block(cur_block_id, instruction);

            (cur_block_id, result)
        }

        SymExpr::Len { id, .. } => {
            // determine if operand array is type int or long
            let table_entry = sym_scope
                .borrow()
                .lookup(id)
                .expect("Array not found in scope!");
            let TableEntry::Variable { typ, .. } = table_entry else {
                panic!("Expected a variable, found something else!");
            };

            let temp = fresh_temp();
            let result = Operand::LocalVar { name: temp.to_string(), typ: Type::Int, reg: None };

            //allocate only for int for the temp to hold length
            cfg.add_temp_var(temp, Type::Int, None);
            let operand: Operand = cfg_scope.lookup_var(id.to_string(), typ.clone(), None);

            let instruction = Instruction::Len {
                typ: Type::Int,
                expr: operand,
                dest: result.clone(),
            };

            cfg.add_instruction_to_block(cur_block_id, instruction);

            (cur_block_id, result)
        }

        _ => unreachable!(),
    }
}

/// Build the CFG for a conditional expression starting at cur_block.
/// Implements short-circuiting.
/// next_true_block is the block to jump to if the condition is true
/// next_false_block is the block to jump to if the condition is false
fn build_cond(
    cfg: &mut CFG,
    expr: &SymExpr,
    mut cur_block_id: i32,
    next_true_block_id: i32,
    next_false_block_id: i32,
    cfg_scope: &CFGScope,
    sym_scope: &Rc<RefCell<Scope>>,
    strings: &mut Vec<String>,
) {
    match expr {
        SymExpr::BinaryExpr {
            op, left, right, ..
        } => {
            match op {
                BinaryOp::And => {
                    let left_true_block_id = next_bblock_id();
                    let left_true_block = BasicBlock::new(left_true_block_id);
                    cfg.add_block(&left_true_block);
                    build_cond(
                        cfg,
                        left,
                        cur_block_id,
                        left_true_block_id,
                        next_false_block_id,
                        cfg_scope,
                        sym_scope,
                        strings,
                    );
                    cfg.add_block_order(left_true_block_id); // add this block after the left condition blocks are added, but before the right
                    // RHS is only evaluated if LHS is true
                    build_cond(
                        cfg,
                        right,
                        left_true_block_id,
                        next_true_block_id,
                        next_false_block_id,
                        cfg_scope,
                        sym_scope,
                        strings,
                    );
                }

                BinaryOp::Or => {
                    let left_false_block_id = next_bblock_id();
                    let left_false_block = BasicBlock::new(left_false_block_id);
                    cfg.add_block(&left_false_block);
                    build_cond(
                        cfg,
                        left,
                        cur_block_id,
                        next_true_block_id,
                        left_false_block_id,
                        cfg_scope,
                        sym_scope,
                        strings,
                    );
                    cfg.add_block_order(left_false_block_id); // add this block after the left condition blocks are added, but before the right
                    // RHS is only evaluated if LHS is false
                    build_cond(
                        cfg,
                        right,
                        left_false_block_id,
                        next_true_block_id,
                        next_false_block_id,
                        cfg_scope,
                        sym_scope,
                        strings,
                    );
                }

                // No short-circuiting necessary
                _ => {
                    let dest: Operand;
                    (cur_block_id, dest) =
                        destruct_expr(cfg, expr, cur_block_id, cfg_scope, sym_scope, strings);

                    cfg.add_instruction_to_block(
                        cur_block_id,
                        Instruction::CJmp {
                            name: cfg.name.clone(),
                            condition: dest,
                            id: next_true_block_id
                        },
                    );
                    cfg.add_edge(
                        cur_block_id,
                        next_true_block_id,
                        EdgeType::True,
                    );

                    cfg.add_instruction_to_block(
                        cur_block_id,
                        Instruction::UJmp {
                            name: cfg.name.clone(),
                            id: next_false_block_id
                        },
                    );
                    cfg.add_edge(
                        cur_block_id,
                        next_false_block_id,
                        EdgeType::False,
                    );
                }
            }
        }

        SymExpr::UnaryExpr { op, expr, .. } => {
            match op {
                UnaryOp::Not => {
                    // just swap the true and false blocks
                    build_cond(
                        cfg,
                        expr,
                        cur_block_id,
                        next_false_block_id,
                        next_true_block_id,
                        cfg_scope,
                        sym_scope,
                        strings,
                    );
                }

                _ => {
                    let dest: Operand;
                    (cur_block_id, dest) =
                        destruct_expr(cfg, expr, cur_block_id, cfg_scope, sym_scope, strings);

                    cfg.add_instruction_to_block(
                        cur_block_id,
                        Instruction::CJmp {
                            name: cfg.name.clone(),
                            condition: dest,
                            id: next_true_block_id
                        },
                    );
                    cfg.add_edge(cur_block_id, next_true_block_id, EdgeType::True);

                    cfg.add_instruction_to_block(
                        cur_block_id,
                        Instruction::UJmp {
                            name: cfg.name.clone(),
                            id: next_false_block_id
                        },
                    );
                    cfg.add_edge(cur_block_id, next_false_block_id, EdgeType::False);
                }
            }
        }

        _ => {
            let dest: Operand;
            (cur_block_id, dest) =
                destruct_expr(cfg, expr, cur_block_id, cfg_scope, sym_scope, strings);

            cfg.add_instruction_to_block(
                cur_block_id,
                Instruction::CJmp {
                    name: cfg.name.clone(),
                    condition: dest,
                    id: next_true_block_id
                },
            );
            cfg.add_edge(cur_block_id, next_true_block_id, EdgeType::True);  

            cfg.add_instruction_to_block(
                cur_block_id,
                Instruction::UJmp {
                    name: cfg.name.clone(),
                    id: next_false_block_id
                },
            );
            cfg.add_edge(cur_block_id, next_false_block_id, EdgeType::False);
        }
    }
}

/// Destruct a statement starting at cur_block into basic blocks and add them to the method CFG.
/// Returns the basic block at the end of the statement.
fn destruct_statement(
    cfg: &mut CFG,
    mut cur_block_id: i32,
    statement: &SymStatement,
    cur_loop: Option<&Loop>,
    cfg_scope: &mut CFGScope,
    sym_scope: &Rc<RefCell<Scope>>,
    strings: &mut Vec<String>,
) -> i32 {
    match &*statement {
        SymStatement::Assignment {
            target, expr, op, ..
        } => {
            let rhs: Operand;
            (cur_block_id, rhs) =   // In this case, rhs is the operand which is the name fo the temp 
                destruct_expr(cfg, expr, cur_block_id, cfg_scope, sym_scope, strings);
            
            let mut dummy_context: SemanticContext = SemanticContext {
                filename: "dummy".to_string(),
                error_found: false,
            };
            let mut dummy_writer = io::sink();

            let target_typ = infer_expr_type(&target, &sym_scope.borrow(), &mut dummy_writer, &mut dummy_context).expect("expected type for target");

            // dest can either be an identifier or array element
            match target {
                SymExpr::ArrAccess { id, index, .. } => {
                    let index_op: Operand;
                    (cur_block_id, index_op) =
                        destruct_expr(cfg, index, cur_block_id, cfg_scope, sym_scope, strings);

                    match op {
                        AssignOp::Assign => {
                            cfg.add_instruction_to_block(
                                cur_block_id,
                                Instruction::Assign {
                                    typ: target_typ.clone(),
                                    src: rhs,
                                    dest: cfg_scope.lookup_arr(id.to_string(), index_op, sym_scope, target_typ.clone()),
                                },
                            );
                        }
                        AssignOp::PlusAssign => {
                            let array_element =
                                cfg_scope.lookup_arr(id.to_string(), index_op, sym_scope, target_typ.clone());

                            // load array element into a new temp
                            let array_temp = fresh_temp();
                            let array_element_temp = Operand::LocalVar { name: array_temp.to_string(), typ: target_typ.clone(), reg: None };

                            let table_entry = sym_scope
                                .borrow()
                                .lookup(id)
                                .expect("Array not found in scope!");
                            let TableEntry::Variable { typ, .. } = table_entry else {
                                panic!("Expected a variable, found something else!");
                            };

                            cfg.add_temp_var(array_temp, typ.clone(), None);
                            cfg.add_instruction_to_block(
                                cur_block_id,
                                Instruction::Assign {
                                    typ: target_typ.clone(),
                                    src: array_element.clone(),
                                    dest: array_element_temp.clone(),
                                },
                            );

                            // add array element to rhs
                            let rhs_temp = fresh_temp();
                            let new_rhs = Operand::LocalVar { name: rhs_temp.to_string(), typ: target_typ.clone(), reg: None };
                            cfg.add_temp_var(rhs_temp, typ.clone(), None);
                            cfg.add_instruction_to_block(
                                cur_block_id,
                                Instruction::Add {
                                    typ: typ.clone(),
                                    left: array_element_temp,
                                    right: rhs,
                                    dest: new_rhs.clone(),
                                },
                            );

                            // store back into array element
                            cfg.add_instruction_to_block(
                                cur_block_id,
                                Instruction::Assign {
                                    typ: target_typ.clone(),
                                    src: new_rhs,
                                    dest: array_element,
                                },
                            );
                        }
                        AssignOp::MinusAssign => {
                            let array_element =
                                cfg_scope.lookup_arr(id.to_string(), index_op, sym_scope, target_typ.clone());

                            // load array element into a new temp
                            let array_temp = fresh_temp();
                            let array_element_temp = Operand::LocalVar { name: array_temp.to_string(), typ: target_typ.clone(), reg: None };
                            let table_entry = sym_scope
                                .borrow()
                                .lookup(id)
                                .expect("Array not found in scope!");
                            let TableEntry::Variable { typ, .. } = table_entry else {
                                panic!("Expected a variable, found something else!");
                            };

                            cfg.add_temp_var(array_temp, typ.clone(), None);
                            cfg.add_instruction_to_block(
                                cur_block_id,
                                Instruction::Assign {
                                    typ: target_typ.clone(),
                                    src: array_element.clone(),
                                    dest: array_element_temp.clone(),
                                },
                            );

                            // subtract rhs from array element
                            let rhs_temp = fresh_temp();
                            let new_rhs = Operand::LocalVar { name: rhs_temp.to_string(), typ: target_typ.clone(), reg: None };
                            cfg.add_temp_var(rhs_temp, typ.clone(), None);
                            cfg.add_instruction_to_block(
                                cur_block_id,
                                Instruction::Subtract {
                                    typ: typ.clone(),
                                    left: array_element_temp,
                                    right: rhs,
                                    dest: new_rhs.clone(),
                                },
                            );

                            // store back into array element
                            cfg.add_instruction_to_block(
                                cur_block_id,
                                Instruction::Assign {
                                    typ: target_typ.clone(),
                                    src: new_rhs,
                                    dest: array_element,
                                },
                            );
                        }
                        AssignOp::MultiplyAssign => {
                            let array_element =
                                cfg_scope.lookup_arr(id.to_string(), index_op, sym_scope, target_typ.clone());

                            // load array element into a new temp
                            let array_temp = fresh_temp();
                            let array_element_temp = Operand::LocalVar { name: array_temp.to_string(), typ: target_typ.clone(), reg: None };
                            let table_entry = sym_scope
                                .borrow()
                                .lookup(id)
                                .expect("Array not found in scope!");
                            let TableEntry::Variable { typ, .. } = table_entry else {
                                panic!("Expected a variable, found something else!");
                            };

                            cfg.add_temp_var(array_temp, typ.clone(), None);
                            cfg.add_instruction_to_block(
                                cur_block_id,
                                Instruction::Assign {
                                    typ: target_typ.clone(),
                                    src: array_element.clone(),
                                    dest: array_element_temp.clone(),
                                },
                            );

                            // multiply array element by rhs
                            let rhs_temp = fresh_temp();
                            let new_rhs = Operand::LocalVar { name: rhs_temp.to_string(), typ: target_typ.clone(), reg: None };
                            cfg.add_temp_var(rhs_temp, typ.clone(), None);
                            cfg.add_instruction_to_block(
                                cur_block_id,
                                Instruction::Multiply {
                                    typ: typ.clone(),
                                    left: array_element_temp,
                                    right: rhs,
                                    dest: new_rhs.clone(),
                                },
                            );

                            // store back into array element
                            cfg.add_instruction_to_block(
                                cur_block_id,
                                Instruction::Assign {
                                    typ: target_typ.clone(),
                                    src: new_rhs,
                                    dest: array_element,
                                },
                            );
                        }
                        AssignOp::DivideAssign => {
                            let array_element =
                                cfg_scope.lookup_arr(id.to_string(), index_op, sym_scope, target_typ.clone());

                            // load array element into a new temp
                            let array_temp = fresh_temp();
                            let array_element_temp = Operand::LocalVar { name: array_temp.to_string(), typ: target_typ.clone(), reg: None };
                            let table_entry = sym_scope
                                .borrow()
                                .lookup(id)
                                .expect("Array not found in scope!");
                            let TableEntry::Variable { typ, .. } = table_entry else {
                                panic!("Expected a variable, found something else!");
                            };

                            cfg.add_temp_var(array_temp, typ.clone(), None);
                            cfg.add_instruction_to_block(
                                cur_block_id,
                                Instruction::Assign {
                                    typ: target_typ.clone(),
                                    src: array_element.clone(),
                                    dest: array_element_temp.clone(),
                                },
                            );

                            // divide array element by rhs
                            let rhs_temp = fresh_temp();
                            let new_rhs = Operand::LocalVar { name: rhs_temp.to_string(), typ: target_typ.clone(), reg: None };
                            cfg.add_temp_var(rhs_temp, typ.clone(), None);
                            cfg.add_instruction_to_block(
                                cur_block_id,
                                Instruction::Divide {
                                    typ: typ.clone(),
                                    left: array_element_temp,
                                    right: rhs,
                                    dest: new_rhs.clone(),
                                },
                            );

                            // store back into array element
                            cfg.add_instruction_to_block(
                                cur_block_id,
                                Instruction::Assign {
                                    typ: target_typ.clone(),
                                    src: new_rhs,
                                    dest: array_element,
                                },
                            );
                        }
                        AssignOp::ModuloAssign => {
                            let array_element =
                                cfg_scope.lookup_arr(id.to_string(), index_op, sym_scope, target_typ.clone());

                            // load array element into a new temp
                            let array_temp = fresh_temp();
                            let array_element_temp = Operand::LocalVar { name: array_temp.to_string(), typ: target_typ.clone(), reg: None };
                            let table_entry = sym_scope
                                .borrow()
                                .lookup(id)
                                .expect("Array not found in scope!");
                            let TableEntry::Variable { typ, .. } = table_entry else {
                                panic!("Expected a variable, found something else!");
                            };

                            cfg.add_temp_var(array_temp.clone(), typ.clone(), None);

                            cfg.add_instruction_to_block(
                                cur_block_id,
                                Instruction::Assign {
                                    typ: target_typ.clone(),
                                    src: array_element.clone(),
                                    dest: array_element_temp.clone(),
                                },
                            );

                            // modulo array element by rhs
                            let rhs_temp = fresh_temp();
                            let new_rhs = Operand::LocalVar { name: array_temp.to_string(), typ: target_typ.clone(), reg: None };
                            cfg.add_temp_var(rhs_temp, typ.clone(), None);
                            cfg.add_instruction_to_block(
                                cur_block_id,
                                Instruction::Modulo {
                                    typ: typ.clone(),
                                    left: array_element_temp,
                                    right: rhs,
                                    dest: new_rhs.clone(),
                                },
                            );

                            // store back into array element
                            cfg.add_instruction_to_block(
                                cur_block_id,
                                Instruction::Assign {
                                    typ: target_typ.clone(),
                                    src: new_rhs,
                                    dest: array_element,
                                },
                            );
                        }
                    }
                }
                SymExpr::Identifier { entry, .. } => {
                    let (_, dest) =
                        destruct_expr(cfg, target, cur_block_id, cfg_scope, sym_scope, strings);
                    let TableEntry::Variable { typ, .. } = entry else {
                        panic!("Expected a variable, found something else!");
                    };

                    let instr = match op {
                        AssignOp::Assign => Instruction::Assign {
                            typ: typ.clone(),
                            src: rhs,
                            dest: dest.clone(),
                        },
                        AssignOp::PlusAssign => Instruction::Add {
                            typ: typ.clone(),
                            left: dest.clone(),
                            right: rhs,
                            dest: dest.clone(),
                        },
                        AssignOp::MinusAssign => Instruction::Subtract {
                            typ: typ.clone(),
                            left: dest.clone(),
                            right: rhs,
                            dest: dest.clone(),
                        },
                        AssignOp::MultiplyAssign => Instruction::Multiply {
                            typ: typ.clone(),
                            left: dest.clone(),
                            right: rhs,
                            dest: dest.clone(),
                        },
                        AssignOp::DivideAssign => Instruction::Divide {
                            typ: typ.clone(),
                            left: dest.clone(),
                            right: rhs,
                            dest: dest.clone(),
                        },
                        AssignOp::ModuloAssign => Instruction::Modulo {
                            typ: typ.clone(),
                            left: dest.clone(),
                            right: rhs,
                            dest: dest.clone(),
                        },
                    };

                    cfg.add_instruction_to_block(cur_block_id, instr);
                }
                _ => unreachable!(),
            }

            cur_block_id
        }

        SymStatement::MethodCall {
            method_name, args, ..
        } => {
            // arguments can be expressions
            let mut arg_ops: Vec<Operand> = Vec::new();
            for arg in args {
                let arg_op;
                (cur_block_id, arg_op) =
                    destruct_expr(cfg, arg, cur_block_id, cfg_scope, sym_scope, strings);
                arg_ops.push(arg_op);
            }

            let table_entry = sym_scope
                .borrow()
                .lookup(method_name)
                .expect("Method not defined in scope");
            let return_type = match table_entry {
                TableEntry::Method { return_type, .. } => return_type,
                TableEntry::Import { .. } => Type::Long,
                _ => panic!(
                    "Expected a method or import, found something else! {:#?}",
                    table_entry
                ),
            };

            cfg.add_instruction_to_block(
                cur_block_id,
                Instruction::MethodCall {
                    name: method_name.clone(),
                    args: arg_ops,
                    return_type,
                    dest: None,
                },
            );

            cur_block_id
        }

        SymStatement::If {
            condition,
            then_block,
            else_block,
            ..
        } => {
            let mut body_block_id = next_bblock_id();
            let body_block = &BasicBlock::new(body_block_id); // the block that starts the body
            
            cfg.add_block(&body_block);

            let next_block_id: i32;
            let next_block: BasicBlock; // the block after the whole if

            let mut if_scope = CFGScope {
                parent: Some(Box::new(cfg_scope.clone())),
                local_to_temp: HashMap::new(),
            };

            if else_block.is_some() {
                let else_id = next_bblock_id();
                let else_body_block = &BasicBlock::new(else_id);
                let mut else_body_block_id = else_id;
                cfg.add_block(&else_body_block);

                next_block_id = next_bblock_id();
                next_block = BasicBlock::new(next_block_id);

                let mut else_scope = CFGScope {
                    parent: Some(Box::new(cfg_scope.clone())),
                    local_to_temp: HashMap::new(),
                };

                build_cond(
                    cfg,
                    condition,
                    cur_block_id,
                    body_block_id,
                    else_body_block_id,
                    cfg_scope,
                    sym_scope,
                    strings,
                );

                cfg.add_block_order(body_block_id); // add this block after the condition blocks

                // Note: use then_block sym_scope
                for statement in &then_block.statements {
                    body_block_id = destruct_statement(
                        cfg,
                        body_block_id,
                        &statement,
                        cur_loop,
                        &mut if_scope,
                        &then_block.scope,
                        strings,
                    );
                }

                // jump to next block
                cfg.add_instruction_to_block(
                    body_block_id,
                    Instruction::UJmp {
                        name: cfg.name.clone(),
                        id: next_block_id,
                    },
                );

                cfg.add_block_order(else_body_block_id); // add this block after blocks from statements in the if body

                // same for else body if we have one
                for statement in &else_block.clone().unwrap().statements {
                    else_body_block_id = destruct_statement(
                        cfg,
                        else_body_block_id,
                        statement,
                        cur_loop,
                        &mut else_scope,
                        &else_block.clone().unwrap().scope,
                        strings,
                    );
                }
                cfg.add_instruction_to_block(
                    else_body_block_id,
                    Instruction::UJmp {
                        name: cfg.name.clone(),
                        id: next_block_id,
                    },
                );

                cfg.add_block_order(next_block_id); // add this block after blocks from statements in the else body

                cfg.add_edge(body_block_id, next_block_id, EdgeType::Unconditional);
                cfg.add_edge(else_body_block_id, next_block_id, EdgeType::Unconditional);
            } else { // no else block
                next_block_id = next_bblock_id();
                next_block = BasicBlock::new(next_block_id);

                build_cond(
                    cfg,
                    condition,
                    cur_block_id,
                    body_block_id,
                    next_block_id,
                    cfg_scope,
                    sym_scope,
                    strings,
                );

                cfg.add_block_order(body_block_id); // add this block after the condition blocks

                for statement in &then_block.statements {
                    body_block_id = destruct_statement(
                        cfg,
                        body_block_id,
                        statement,
                        cur_loop,
                        &mut if_scope,
                        &then_block.scope,
                        strings,
                    );
                }

                // jump to next block
                cfg.add_instruction_to_block(
                    body_block_id,
                    Instruction::UJmp {
                        name: cfg.name.clone(),
                        id: next_block_id,
                    },
                );
                cfg.add_block_order(next_block_id); // add this block after blocks from statements in the if body

                cfg.add_edge(body_block_id, next_block_id, EdgeType::Unconditional);
            }

            cfg.add_block(&next_block);
            next_block_id
        }
        SymStatement::While {
            condition, block, ..
        } => {
            let header_id = next_bblock_id();
            let header_block = BasicBlock::new(header_id); // the block that evaluates the loop condition
            let mut body_id = next_bblock_id();
            let body_block = &BasicBlock::new(body_id); // the block that starts the body
            let next_id = next_bblock_id();
            let next_block = BasicBlock::new(next_id); // the block after the whole loop
            cfg.add_block(&header_block);
            cfg.add_block(&body_block);
            cfg.add_block(&next_block);
            // Two edges from header to body and next are handled by buildconditional

            cfg.add_instruction_to_block(
                cur_block_id,
                Instruction::UJmp {
                    name: cfg.name.clone(),
                    id: header_id,
                },
            );
            cfg.add_edge(cur_block_id, header_id, EdgeType::Unconditional);

            cfg.add_block_order(header_id); // add this block before the condition blocks

            build_cond(
                cfg,
                condition,
                header_id,
                body_id,
                next_id,
                cfg_scope,
                sym_scope,
                strings,
            );

            let next_loop = Loop {
                break_to: next_id,
                continue_to: header_id,
            };

            let mut new_scope = CFGScope {
                parent: Some(Box::new(cfg_scope.clone())),
                local_to_temp: HashMap::new(),
            };

            cfg.add_block_order(body_id); // add this block before the blocks from statements in the loop body

            for statement in &block.statements {
                body_id = destruct_statement(
                    cfg,
                    body_id,
                    statement,
                    Some(&next_loop),
                    &mut new_scope,
                    &block.scope,
                    strings,
                );
            }

            // jump back to header block after the body
            cfg.add_instruction_to_block(
                body_id,
                Instruction::UJmp {
                    name: cfg.name.clone(),
                    id: header_id,
                },
            );

            cfg.add_block_order(next_id); // add this block after the blocks from statements in the loop body

            cfg.add_edge(body_id, header_id, EdgeType::Unconditional);

            next_id
        }
        SymStatement::For {
            var,
            init,
            condition,
            update,
            block,
            ..
        } => {
            let header_id = next_bblock_id();
            let header_block = BasicBlock::new(header_id); // condition evaluation
            let mut body_id = next_bblock_id();
            let body_block = &BasicBlock::new(body_id); // body of the loop
            let update_block_id = next_bblock_id();
            let update_block = BasicBlock::new(update_block_id); // update block
            let next_id = next_bblock_id();
            let next_block = BasicBlock::new(next_id); // block after loop

            cfg.add_block(&header_block);
            cfg.add_block(&body_block);
            cfg.add_block(&update_block);
            cfg.add_block(&next_block);

            let var_entry = sym_scope.borrow().lookup(&var).expect(format!("Did not find for loop variable {}", var).as_str());
            let var_typ = if let TableEntry::Variable { typ, .. } = var_entry {
                typ.clone()
            } else {
                panic!("Expected a variable, found something else!");
            };

            let lhs = cfg_scope.lookup_var(var.to_string(), var_typ.clone(), None);
            let rhs: Operand;
            (cur_block_id, rhs) =
                destruct_expr(cfg, init, cur_block_id, cfg_scope, sym_scope, strings);
            cfg.add_instruction_to_block(
                cur_block_id,
                Instruction::Assign {
                    typ: var_typ,
                    src: rhs,
                    dest: lhs,
                },
            );

            cfg.add_block_order(header_id); // add this block after the init, before the condition

            // jump to loop condition check
            cfg.add_instruction_to_block(
                cur_block_id,
                Instruction::UJmp {
                    name: cfg.name.clone(),
                    id: header_id,
                },
            );
            cfg.add_edge(cur_block_id, header_id, EdgeType::Unconditional);
        
            // build condition check
            build_cond(
                cfg,
                condition,
                header_id,
                body_id,
                next_id,
                cfg_scope,
                sym_scope,
                strings,
            );

            cfg.add_block_order(body_id); // add this block after the condition

            // loop struct
            let next_loop = Loop {
                break_to: next_id,
                continue_to: update_block_id, // ← fix: continue should go to update
            };

            // scoped body
            let mut new_scope = CFGScope {
                parent: Some(Box::new(cfg_scope.clone())),
                local_to_temp: HashMap::new(),
            };

            // destruct loop body
            for statement in &block.statements {
                body_id = destruct_statement(
                    cfg,
                    body_id,
                    statement,
                    Some(&next_loop),
                    &mut new_scope,
                    &block.scope,
                    strings,
                );
            }

            // jump to update block after body
            cfg.add_instruction_to_block(
                body_id,
                Instruction::UJmp {
                    name: cfg.name.clone(),
                    id: update_block_id,
                },
            );
            cfg.add_edge(body_id, update_block_id, EdgeType::Unconditional);

            cfg.add_block_order(update_block_id); // add this block before destructing the update
        
            // update block executes update expression
            let mut update_id = update_block_id;
            update_id = destruct_statement(
                cfg,
                update_id,
                update,
                Some(&next_loop),
                cfg_scope,
                sym_scope,
                strings,
            );

            // jump back to condition after update
            cfg.add_instruction_to_block(
                update_id,
                Instruction::UJmp {
                    name: cfg.name.clone(),
                    id: header_id,
                },
            );
            cfg.add_edge(update_id, header_id, EdgeType::Unconditional);

            cfg.add_block_order(next_id); // add this after all for loop blocks
        
            next_id
        }

        SymStatement::Break { .. } => {
            cfg.add_instruction_to_block(
                cur_block_id,
                Instruction::UJmp {
                    name: cfg.name.clone(),
                    id: cur_loop.unwrap().break_to,
                },
            );
            cfg.add_edge(cur_block_id, cur_loop.unwrap().break_to, EdgeType::Unconditional);

            // code after the break within this statement is unreachable
            UNREACHABLE_BLOCK
        }
        SymStatement::Continue { .. } => {
            cfg.add_instruction_to_block(
                cur_block_id,
                Instruction::UJmp {
                    name: cfg.name.clone(),
                    id: cur_loop.clone().unwrap().continue_to,
                },
            );
            cfg.add_edge(cur_block_id, cur_loop.unwrap().continue_to, EdgeType::Unconditional);

            // code after the continue within this statement is unreachable
            UNREACHABLE_BLOCK
        }
        SymStatement::Return { expr, .. } => {
            if expr.is_some() {
                let operand: Operand;
                (cur_block_id, operand) = destruct_expr(
                    cfg,
                    &expr.clone().unwrap(),
                    cur_block_id,
                    cfg_scope,
                    sym_scope,
                    strings,
                );

                let mut dummy_context: SemanticContext = SemanticContext {
                    filename: "dummy".to_string(),
                    error_found: false,
                };
                let mut dummy_writer = io::sink();
                let typ = infer_expr_type(
                    expr.as_ref().unwrap(),
                    &sym_scope.borrow(),
                    &mut dummy_writer,
                    &mut dummy_context,
                )
                .expect("Function has undefined return type");

                cfg.add_instruction_to_block(
                    cur_block_id,
                    Instruction::Ret {
                        typ,
                        value: Some(operand),
                    },
                );
            } else {
                cfg.add_instruction_to_block(
                    cur_block_id,
                    Instruction::Ret {
                        typ: Type::Void,
                        value: None,
                    },
                );
            }

            // code after the return within this statement is unreachable
            UNREACHABLE_BLOCK
        }
        SymStatement::VarDecl {
            name, typ, length, ..
        } => {
            // create new temp variable for this local variable, and add it to the scope
            let temp = fresh_var(name);
            cfg_scope.add_local(name.to_string(), temp.to_string());

            if length.is_some() {
                // array
                let int_val;
                match length.as_ref().unwrap() {
                    Literal::Int(val) => {
                        int_val = val.parse::<i64>();
                        cfg.add_temp_var(temp.clone(), typ.clone(), Some(int_val.clone().unwrap()));
                    }
                    Literal::HexInt(val) => {
                        int_val = i64::from_str_radix(&val, 16);
                        cfg.add_temp_var(temp.clone(), typ.clone(), Some(int_val.clone().unwrap()));
                    }
                    _ => unreachable!(),
                }

                cfg.add_instruction_to_block(
                    cur_block_id,
                    Instruction::Assign {
                        typ: typ.clone(),
                        src: Operand::Const { value: int_val.unwrap(), typ: Type::Int, reg: None },
                        dest: Operand::LocalVar { name: temp, typ: typ.clone(), reg: None }, // IS this right that the local var type is Int? 
                        
                    },
                ); // set array length to first element of array
            } else {
                // variable
                cfg.add_temp_var(temp, typ.clone(), None);
            }

            cur_block_id
        }
        SymStatement::Error => {
            cfg.add_instruction_to_block(cur_block_id, Instruction::Exit { exit_code: -1 });
            cur_block_id
        }
    }
}

/// Destruct a method AST node into basic blocks
fn destruct_method(method: &Rc<SymMethod>, strings: &mut Vec<String>) -> CFG {
    // reset counters for each CFG
    reset_counters();

    let mut method_cfg = CFG::new(method.name.clone());

    let mut scope: CFGScope = CFGScope {
        parent: None,
        local_to_temp: HashMap::new(),
    };

    let mut cur_block_id = next_bblock_id(); // should be 0
    let entry_block = BasicBlock::new(cur_block_id);
    method_cfg.add_block(&entry_block);
    method_cfg.add_block_order(cur_block_id);

    // add parameters to method scope
    for (pos, (typ, param_name, ..)) in method.params.iter().enumerate() {
        let temp = fresh_temp();
        method_cfg.add_temp_var(temp.clone(), typ.clone(), None);
    
        // Add param name → temp to local scope
        scope.add_local(param_name.to_string(), temp.clone());
        method_cfg.param_to_temp.insert(pos as i32, Temp { name: temp.clone() });
    
        // Load argument from caller into local temp
        method_cfg.add_instruction_to_block(
            cur_block_id,
            Instruction::Assign {
                typ: typ.clone(),
                src: Operand::Argument { position: pos as i32, typ: typ.clone(), reg: None },
                dest: Operand::LocalVar { name: temp, typ: typ.clone(), reg: None },
            },
        );
    }
    

    for statement in method.body.statements.clone() {
        cur_block_id = destruct_statement(
            &mut method_cfg,
            cur_block_id,
            &statement,
            None,
            &mut scope,
            &method.scope,
            strings,
        );
    }

    if method.return_type != Type::Void {
        cur_block_id = destruct_statement(
            &mut method_cfg,
            cur_block_id,
            &SymStatement::Error,
            None,
            &mut scope,
            &method.scope,
            strings,
        );
    }

    // If we get to end of main, make sure that we have a return zero (put zero in rax to signify success)
    if method.name.clone() == "main" {
        cur_block_id = destruct_statement(
            &mut method_cfg,
            cur_block_id,
            &SymStatement::Return {
                expr: Some(SymExpr::Literal {
                    value: Literal::Int("0".to_string()),
                    span: Span {
                        sline: 0,
                        scol: 0,
                        eline: 0,
                        ecol: 0,
                    },
                }),
                span: Span {
                    sline: 0,
                    scol: 0,
                    eline: 0,
                    ecol: 0,
                },
            },
            None,
            &mut scope,
            &method.scope,
            strings,
        );
    }

    // Reserve extra stack space for the the method call with the largest
    // number of stack args, when num args > 6
    let mut max_stack_args = 0;
    for block in method_cfg.blocks.values() {
        for instr in &block.instructions {
            if let Instruction::MethodCall { args, .. } = instr {
                let stack_arg_count = args.len().saturating_sub(6);
                max_stack_args = max_stack_args.max(stack_arg_count);
            }
        }
    }
    if max_stack_args > 0 {
        method_cfg.stack_size += max_stack_args as i64 * 8;
    }

    method_cfg.exit = cur_block_id;

    method_cfg
}

/// Destruct a program AST node into basic blocks
fn destruct_program(program: &SymProgram, strings: &mut Vec<String>) -> BTreeMap<String, CFG> {
    let mut method_cfgs: BTreeMap<String, CFG> = BTreeMap::new(); // Hash map to map method names to associated CFG

    for (name, method) in &program.methods {
        let cfg: CFG = destruct_method(method, strings);
        method_cfgs.insert(name.clone(), cfg);
    }

    method_cfgs
}

/// Build the CFG from the symbol table IR.
/// Returns a map of (method_name, method_cfg), a vector of global variables,
/// and a vector of string constants
pub fn build_cfg(
    file: &str,
    filename: &str,
    writer: &mut dyn std::io::Write,
    debug: bool,
) -> (BTreeMap<String, CFG>, BTreeMap<String, Global>, Vec<String>) {
    // Create symbol table IR
    let sym_tree: SymProgram = semcheck(file, filename, writer, debug);

    // global variables
    let mut globals: BTreeMap<String, Global> = BTreeMap::new();
    let scope = &sym_tree.global_scope.borrow().table;
    
    for (_, entry) in scope {
        if let TableEntry::Variable {
            name,
            typ,
            length,
            ..
        } = entry
        {
            globals.insert(
                name.to_string(),
                Global {
                    name: name.to_string(),
                    length: *length,
                    typ: typ.clone(),
                },
            );
        }
    }

    let mut strings: Vec<String> = Vec::new();

    // Generate a CFG for each method
    let method_cfgs: BTreeMap<String, CFG> = destruct_program(&sym_tree, &mut strings);
    (method_cfgs, globals, strings)
}
