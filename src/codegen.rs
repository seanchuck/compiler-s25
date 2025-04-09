/**
 Generate x86 code from the Control flow graph.
 **/
 use crate::{
    ast::Type,
    buildcfg::build_cfg,
    cfg::{CFG, INT_SIZE, LONG_SIZE, Global},
    tac::*,
    utils::print::print_cfg,
    x86::*,
};
use core::panic;
use std::collections::HashMap;

fn is_immediate_operand(op: &X86Operand) -> bool {
    matches!(op, X86Operand::Constant(_))
}

fn is_memory_operand(op: &X86Operand) -> bool {
    matches!(
        op,
        X86Operand::RegInt(..)
            | X86Operand::RegLabel(..)
            | X86Operand::Address(..)
            | X86Operand::Global(_) // assuming globals refer to memory-located data
    )
}

/// Returns the x86 operand corresponding to operand
fn map_operand(
    method_cfg: &CFG,
    operand: &Operand,
    x86_instructions: &mut Vec<X86Insn>,
) -> X86Operand {
    match operand {
        Operand::Const(val) => X86Operand::Constant(*val),
        Operand::LocalVar(temp) => {
            X86Operand::RegInt(Register::Rbp, method_cfg.get_stack_offset(temp))
        }
        Operand::GlobalVar(val) => X86Operand::Global(val.to_string()),
        Operand::LocalArrElement(arr, idx) => {
            let array_typ: Type = method_cfg.locals.get(arr).expect("expected array entry").typ.clone();
            let idx_op = map_operand(method_cfg, idx, x86_instructions);
        
            // Load base address of array into R11
            x86_instructions.push(X86Insn::Lea(
                X86Operand::RegInt(Register::Rbp, method_cfg.get_stack_offset(arr)),
                X86Operand::Reg(Register::R11),
            ));
        
            // Move index into R10 (64-bit regardless of int or long)
            x86_instructions.push(X86Insn::Mov(idx_op, X86Operand::Reg(Register::R10), array_typ.clone()));
        
            x86_instructions.push(X86Insn::Add(
                X86Operand::Constant(1),
                X86Operand::Reg(Register::R10),
                Type::Long, // safe default for 64-bit add
            ));
        
            // type only affects address offsets
            match array_typ {
                Type::Int => {
                    X86Operand::Address(None, Some(Register::R11), Register::R10, INT_SIZE)
                }
                Type::Long => {
                    X86Operand::Address(None, Some(Register::R11), Register::R10, LONG_SIZE)
                }
                _ => panic!("Array element must be numeric type"),
            }
        }
        
        Operand::GlobalArrElement(arr, idx, typ) => {
            let idx_op = map_operand(method_cfg, idx, x86_instructions);
        
            // Always use 64-bit index register
            x86_instructions.push(X86Insn::Mov(idx_op, X86Operand::Reg(Register::R10), Type::Long));
            x86_instructions.push(X86Insn::Add(X86Operand::Constant(1), X86Operand::Reg(Register::R10), Type::Long)); // Safe to always use 64-bit add
        
            match typ {
                Type::Int => {
                    X86Operand::Address(Some(arr.to_string()), None, Register::R10, INT_SIZE)
                }
                Type::Long => {
                    X86Operand::Address(Some(arr.to_string()), None, Register::R10, LONG_SIZE)
                }
                _ => panic!("array elements must be numeric types!")
            }
        }
        
        Operand::Argument(pos) => {
            match pos {
                0 => X86Operand::Reg(Register::Rdi),
                1 => X86Operand::Reg(Register::Rsi),
                2 => X86Operand::Reg(Register::Rdx),
                3 => X86Operand::Reg(Register::Rcx),
                4 => X86Operand::Reg(Register::R8),
                5 => X86Operand::Reg(Register::R9),
                n => {
                    // Only used for callee retrieving operand, caller pushes without using map_operand!
                    // 16 byte offset because: old_rbp and ra stored betweeen new_rbp and args

                    // TODO: update based on size of function arg
                    let offset = 16 + ((n - 6) as i64 * 8);
                    X86Operand::RegInt(Register::Rbp, offset)
                }
            }
        }
        Operand::String(idx) => X86Operand::RegLabel(Register::Rip, format!("str{idx}")),
    }
}

/// Adds the x86 instructions corresponding to insn to x86_instructions
fn add_instruction(method_cfg: &CFG, insn: &Instruction, x86_instructions: &mut Vec<X86Insn>) {
    match insn {
        Instruction::LoadConst{ src, dest , typ} => {
            // x86_instructions.push(X86Insn::Mov(X86Operand::Constant(((src.clone() as u64) & 0xFFFFFFFF) as i64), X86Operand::Reg(Register::Rbx), typ.clone())); // lower 32 bits
            // x86_instructions.push(X86Insn::Mov(X86Operand::Constant(((src.clone() as u64) >> 32) as i64), X86Operand::Reg(Register::Rax), typ.clone())); // upper 32 bits
            // x86_instructions.push(X86Insn::Shl(X86Operand::Constant(32), X86Operand::Reg(Register::Rax)));
            // x86_instructions.push(X86Insn::Or(X86Operand::Reg(Register::Rax), X86Operand::Reg(Register::Rbx)));
            // let op = map_operand(method_cfg, dest, x86_instructions);
            // x86_instructions.push(X86Insn::Mov(X86Operand::Reg(Register::Rbx), op));
            match typ {
                Type::Int => {
                    x86_instructions.push(X86Insn::Mov(
                        X86Operand::Constant(src.clone()),
                        X86Operand::Reg(Register::Ebx),  // 32-bit!
                        typ.clone()
                    ));
                }
                _ => {
                    x86_instructions.push(X86Insn::Mov(
                        X86Operand::Constant(src.clone() as i64),
                        X86Operand::Reg(Register::Rbx),
                        typ.clone()
                    )); 
                }
            }
           
        }
        Instruction::Add { left, right, dest, typ } => {
            let left_op = map_operand(method_cfg, left, x86_instructions);
            let right_op = map_operand(method_cfg, right, x86_instructions);
            let dest_op = map_operand(method_cfg, dest, x86_instructions);

            match typ {
                Type::Int => {
                    x86_instructions.push(X86Insn::Mov(left_op, X86Operand::Reg(Register::Eax), typ.clone()));
                    x86_instructions.push(X86Insn::Add(right_op, X86Operand::Reg(Register::Eax), typ.clone()));
                    x86_instructions.push(X86Insn::Mov(X86Operand::Reg(Register::Eax), dest_op, typ.clone()));
                }
                Type::Long => {
                    x86_instructions.push(X86Insn::Mov(left_op, X86Operand::Reg(Register::Rax), typ.clone()));
                    x86_instructions.push(X86Insn::Add(right_op, X86Operand::Reg(Register::Rax), typ.clone()));
                    x86_instructions.push(X86Insn::Mov(X86Operand::Reg(Register::Rax), dest_op, typ.clone()));
                }
                _ => panic!("Add only works on numeric types")
            }
        }
        Instruction::Subtract { left, right, dest, typ } => {
            let left_op = map_operand(method_cfg, left, x86_instructions);
            let right_op = map_operand(method_cfg, right, x86_instructions);
            let dest_op = map_operand(method_cfg, dest, x86_instructions);

            match typ {
                Type::Int => {
                    x86_instructions.push(X86Insn::Mov(left_op, X86Operand::Reg(Register::Eax), typ.clone()));
                    x86_instructions.push(X86Insn::Sub(right_op, X86Operand::Reg(Register::Eax), typ.clone()));
                    x86_instructions.push(X86Insn::Mov(X86Operand::Reg(Register::Eax), dest_op, typ.clone()));
                }
                Type::Long => {
                    x86_instructions.push(X86Insn::Mov(left_op, X86Operand::Reg(Register::Rax), typ.clone()));
                    x86_instructions.push(X86Insn::Sub(right_op, X86Operand::Reg(Register::Rax), typ.clone()));
                    x86_instructions.push(X86Insn::Mov(X86Operand::Reg(Register::Rax), dest_op, typ.clone()));
                }
                _ => panic!("Sub only works on numeric types")
            }
        }
        Instruction::Assign { src, dest } => {
            let dest_op = map_operand(method_cfg, dest, x86_instructions);
            let src_op = map_operand(method_cfg, src, x86_instructions);

            x86_instructions.push(X86Insn::Mov(src_op, X86Operand::Reg(Register::Rax), Type::Long));
            x86_instructions.push(X86Insn::Mov(X86Operand::Reg(Register::Rax), dest_op, Type::Long));
        }
        Instruction::LoadString { src, dest } => {
            // Pointers must always be 64 bits
            let dest_op = map_operand(method_cfg, dest, x86_instructions);
            let src_op = map_operand(method_cfg, src, x86_instructions);

            // rax as working register
            x86_instructions.push(X86Insn::Lea(src_op, X86Operand::Reg(Register::Rax)));
            x86_instructions.push(X86Insn::Mov(X86Operand::Reg(Register::Rax), dest_op, Type::Long));
        }
        Instruction::MethodCall { name, args, dest, return_type } => {
            // Determine the destination operand
            let dest_op = match dest {
                Some(d) => map_operand(method_cfg, d, x86_instructions),
                None => X86Operand::Reg(Register::Rax),
            };

            // Setup arguments
            // First 6 args go in registers
            for (i, arg) in args.iter().take(6).enumerate() {
                let arg_reg =
                    map_operand(method_cfg, &Operand::Argument(i as i32), x86_instructions);
                let arg_val = map_operand(method_cfg, arg, x86_instructions);
                x86_instructions.push(X86Insn::Mov(arg_val, arg_reg, Type::Long));
            }

            // Arguments {7...n} go on stack, with last args going first; assume stack 16-aligned before call
            // TODO: change to support 32-bit args
            let mut sp_offset = 0;
            for arg in args.iter().skip(6) {
                let arg_val = map_operand(method_cfg, arg, x86_instructions);
                x86_instructions.push(X86Insn::Mov(
                    arg_val.clone(),
                    X86Operand::Reg(Register::Rax),
                    Type::Long
                ));
                x86_instructions.push(X86Insn::Mov(
                    X86Operand::Reg(Register::Rax),
                    X86Operand::RegInt(Register::Rsp, sp_offset),
                    Type::Long
                ));

                sp_offset += 8;
            }

            // Zero rax before call (implicitly zero-extends full 64 bits)
            x86_instructions.push(X86Insn::Mov(X86Operand::Constant(0), X86Operand::Reg(Register::Eax), Type::Int));

            // Make the call
            x86_instructions.push(X86Insn::Call(name.to_string()));

            // Move the result into `dest` if necessary
            match dest_op {
                X86Operand::Reg(Register::Rax) => {}
                X86Operand::Reg(Register::Eax) => {}
                _ => {
                    match return_type {
                        Type::Int
                        | Type::Bool => x86_instructions.push(X86Insn::Mov(X86Operand::Reg(Register::Eax), dest_op, Type::Int)),
                        Type::Long => x86_instructions.push(X86Insn::Mov(X86Operand::Reg(Register::Rax), dest_op, Type::Long)),
                        _=> panic!("Methods can only return int, long, bool")
                    }
                }
            }
        }
        Instruction::Ret { value, typ } => {
            if let Some(value) = value {
                let value_reg = map_operand(method_cfg, value, x86_instructions);
                x86_instructions.push(X86Insn::Mov(value_reg, X86Operand::Reg(Register::Rax), typ.clone()));
            }
            x86_instructions.push(X86Insn::Mov(
                X86Operand::Reg(Register::Rbp),
                X86Operand::Reg(Register::Rsp),
                typ.clone()
            ));
            x86_instructions.push(X86Insn::Pop(X86Operand::Reg(Register::Rbp)));
            x86_instructions.push(X86Insn::Ret);
        }
        Instruction::Multiply { left, right, dest , typ} => {
            let left_op = map_operand(method_cfg, left, x86_instructions);
            let right_op = map_operand(method_cfg, right, x86_instructions);
            let dest_op = map_operand(method_cfg, dest, x86_instructions);

            match typ {
                Type::Int => {
                    x86_instructions.push(X86Insn::Mov(left_op, X86Operand::Reg(Register::Eax), typ.clone()));
                    x86_instructions.push(X86Insn::Mul(right_op, X86Operand::Reg(Register::Eax), typ.clone()));
                    x86_instructions.push(X86Insn::Mov(X86Operand::Reg(Register::Eax), dest_op, typ.clone()));
                }
                Type::Long => {
                    x86_instructions.push(X86Insn::Mov(left_op, X86Operand::Reg(Register::Rax), typ.clone()));
                    x86_instructions.push(X86Insn::Mul(right_op, X86Operand::Reg(Register::Rax), typ.clone()));
                    x86_instructions.push(X86Insn::Mov(X86Operand::Reg(Register::Rax), dest_op, typ.clone()));
                }
                _ => panic!("Mul only works on numeric types")
            }
        }
        Instruction::Divide { left, right, dest, typ} => {
            let left_op = map_operand(method_cfg, left, x86_instructions);
            let right_op = map_operand(method_cfg, right, x86_instructions);
            let dest_op = map_operand(method_cfg, dest, x86_instructions);

            match typ {
                Type::Int => {
                    // Signed division in x86:
                    // Dividend in RAX, sign-extended into RDX using CQO
                    x86_instructions.push(X86Insn::Mov(left_op, X86Operand::Reg(Register::Eax), typ.clone()));
                    x86_instructions.push(X86Insn::Cqto); // Sign extend rax into Rdx
                    x86_instructions.push(X86Insn::Mov(right_op, X86Operand::Reg(Register::Ecx), typ.clone())); // Division cannot work on immediate, move to scratch reg
                    x86_instructions.push(X86Insn::Div(X86Operand::Reg(Register::Ecx), typ.clone())); // Signed divide RDX:RAX by right_op
                    x86_instructions.push(X86Insn::Mov(X86Operand::Reg(Register::Eax), dest_op, typ.clone()));
                }
                Type::Long => {
                    // Signed division in x86:
                    // Dividend in RAX, sign-extended into RDX using CQO
                    x86_instructions.push(X86Insn::Mov(left_op, X86Operand::Reg(Register::Rax), typ.clone()));
                    x86_instructions.push(X86Insn::Cqto); // Sign extend rax into Rdx
                    x86_instructions.push(X86Insn::Mov(right_op, X86Operand::Reg(Register::Rcx), typ.clone())); // Division cannot work on immediate, move to scratch reg
                    x86_instructions.push(X86Insn::Div(X86Operand::Reg(Register::Rcx), typ.clone())); // Signed divide RDX:RAX by right_op
                    x86_instructions.push(X86Insn::Mov(X86Operand::Reg(Register::Rax), dest_op, typ.clone()));

                }
                _=> panic!("divide only specified for numeric types")
            }

        }
        Instruction::Modulo { left, right, dest , typ} => {
            let left_op: X86Operand = map_operand(method_cfg, left, x86_instructions);
            let right_op = map_operand(method_cfg, right, x86_instructions);
            let dest_op = map_operand(method_cfg, dest, x86_instructions);

            match typ {
                Type::Int => {
                    // Signed modulo using XOR to zero out %rdx
                    x86_instructions.push(X86Insn::Mov(left_op, X86Operand::Reg(Register::Eax), typ.clone())); // RAX = left
                    x86_instructions.push(X86Insn::Xor(
                        X86Operand::Reg(Register::Edx),
                        X86Operand::Reg(Register::Edx),
                    )); // zero RDX
                    x86_instructions.push(X86Insn::Mov(right_op, X86Operand::Reg(Register::Ecx), typ.clone())); // Division cannot work on immediate, move to scratch reg
                    x86_instructions.push(X86Insn::Div(X86Operand::Reg(Register::Ecx), typ.clone())); // Signed divide RDX:RAX by right_op
                    x86_instructions.push(X86Insn::Mov(X86Operand::Reg(Register::Edx), dest_op, typ.clone()));
                }
                Type::Long => {
                    // Signed modulo using XOR to zero out %rdx
                    x86_instructions.push(X86Insn::Mov(left_op, X86Operand::Reg(Register::Rax), typ.clone())); // RAX = left
                    x86_instructions.push(X86Insn::Xor(
                        X86Operand::Reg(Register::Rdx),
                        X86Operand::Reg(Register::Rdx),
                    )); // zero RDX
                    x86_instructions.push(X86Insn::Mov(right_op, X86Operand::Reg(Register::Rcx), typ.clone())); // Division cannot work on immediate, move to scratch reg
                    x86_instructions.push(X86Insn::Div(X86Operand::Reg(Register::Rcx), typ.clone())); // Signed divide RDX:RAX by right_op
                    x86_instructions.push(X86Insn::Mov(X86Operand::Reg(Register::Rdx), dest_op, typ.clone()));
                }
                _=> panic!("Modulo only defined for numeric types.")
            }
        }
        Instruction::Not { expr, dest } => {
            let expr_op = map_operand(method_cfg, expr, x86_instructions);
            let dest_op = map_operand(method_cfg, dest, x86_instructions);

            // Move expr to RAX (or any scratch reg), xor with 1
            x86_instructions.push(X86Insn::Mov(expr_op, X86Operand::Reg(Register::Eax), Type::Int));
            x86_instructions.push(X86Insn::Xor(
                X86Operand::Constant(1),
                X86Operand::Reg(Register::Eax),
            ));
            // Bool types only require lower 32-bit registers
            x86_instructions.push(X86Insn::Mov(X86Operand::Reg(Register::Eax), dest_op, Type::Int));
        }
        Instruction::Cast {
                expr,
                dest,
                target_type,
            } => {
            let expr_op = map_operand(method_cfg, expr, x86_instructions);
            let dest_op = map_operand(method_cfg, dest, x86_instructions);

            match target_type {
                crate::ast::Type::Int => {
                    x86_instructions.push(X86Insn::Mov(expr_op, X86Operand::Reg(Register::Rax), Type::Int));
                    x86_instructions.push(X86Insn::Mov(X86Operand::Reg(Register::Rax), dest_op, Type::Int));
                }
                // cannot load 64-bit value into 32 bits of memory! Should be fine since we don't in-place cast
                crate::ast::Type::Long => {
                    x86_instructions.push(X86Insn::Mov(expr_op, X86Operand::Reg(Register::Rax), Type::Long));
                    x86_instructions.push(X86Insn::Mov(X86Operand::Reg(Register::Rax), dest_op, Type::Long));
                }
                _ => panic!("Shouldnt get here, cannot cast non int or long value"),
            }
        }
        Instruction::Len { expr, dest, typ} => {
            let expr_op = map_operand(method_cfg, expr, x86_instructions);
            let dest_op = map_operand(method_cfg, dest, x86_instructions);

            // Len is stored in the first value of the array
            x86_instructions.push(X86Insn::Mov(expr_op, X86Operand::Reg(Register::Rax), typ.clone()));
            x86_instructions.push(X86Insn::Mov(X86Operand::Reg(Register::Rax), dest_op, typ.clone()));
        }
        Instruction::Greater { left, right, dest }
        | Instruction::Less { left, right, dest }
        | Instruction::LessEqual { left, right, dest }
        | Instruction::GreaterEqual { left, right, dest }
        | Instruction::Equal { left, right, dest }
        | Instruction::NotEqual { left, right, dest } => {
            let mut left_op = map_operand(method_cfg, left, x86_instructions);
            let mut right_op = map_operand(method_cfg, right, x86_instructions);
            let dest_op = map_operand(method_cfg, dest, x86_instructions);
            let mut swapped = false;

            // Any boolean values operate exclusively on 32-bit values
            // handle illegal cmp: (mem, mem)
            if is_memory_operand(&left_op) && is_memory_operand(&right_op) {
                // move value into a register
                x86_instructions.push(X86Insn::Mov(left_op.clone(), X86Operand::Reg(Register::Eax), Type::Int));
                left_op = X86Operand::Reg(Register::Eax);
            }

            // handle illegal cmp: (imm, imm) by loading a val into a reg
            if is_immediate_operand(&left_op) && is_immediate_operand(&right_op) {
                x86_instructions.push(X86Insn::Mov(left_op.clone(), X86Operand::Reg(Register::Eax), Type::Int));
                left_op = X86Operand::Reg(Register::Eax);
            }

            // handle illegal cmp: (mem, imm), (reg, imm) --> imm must come first
            if is_immediate_operand(&left_op) && !is_immediate_operand(&right_op) {
                // immediate must come first → flip
                std::mem::swap(&mut left_op, &mut right_op);
                swapped = true; // must swap the boolean operator since swapped operands!
            }

            // x86 swaps right and left for cmp
            x86_instructions.push(X86Insn::Cmp(right_op, left_op));

            let set_instr = match insn {
                Instruction::Greater { .. } => {
                    if swapped {
                        X86Insn::Setl(X86Operand::Reg(Register::Al))
                    } else {
                        X86Insn::Setg(X86Operand::Reg(Register::Al))
                    }
                }
                Instruction::Less { .. } => {
                    if swapped {
                        X86Insn::Setg(X86Operand::Reg(Register::Al))
                    } else {
                        X86Insn::Setl(X86Operand::Reg(Register::Al))
                    }
                }
                Instruction::GreaterEqual { .. } => {
                    if swapped {
                        X86Insn::Setle(X86Operand::Reg(Register::Al))
                    } else {
                        X86Insn::Setge(X86Operand::Reg(Register::Al))
                    }
                }
                Instruction::LessEqual { .. } => {
                    if swapped {
                        X86Insn::Setge(X86Operand::Reg(Register::Al))
                    } else {
                        X86Insn::Setle(X86Operand::Reg(Register::Al))
                    }
                }
                Instruction::Equal { .. } => X86Insn::Sete(X86Operand::Reg(Register::Al)),
                Instruction::NotEqual { .. } => X86Insn::Setne(X86Operand::Reg(Register::Al)),
                _ => unreachable!(),
            };

            x86_instructions.push(set_instr);
            x86_instructions.push(X86Insn::Movzbq(X86Operand::Reg(Register::Al), X86Operand::Reg(Register::Eax)));
            x86_instructions.push(X86Insn::Mov(X86Operand::Reg(Register::Eax), dest_op, Type::Int));
        }
        Instruction::UJmp { name, id } => {
            let label = format!("{}{}", name, id);
            x86_instructions.push(X86Insn::Jmp(label));
        }
        Instruction::CJmp {
                name,
                condition,
                id,
            } => {
            let label = format!("{}{}", name, id);
            let cond_op = map_operand(method_cfg, condition, x86_instructions);

            // cmp condition, 0 → is condition true?
            x86_instructions.push(X86Insn::Mov(cond_op, X86Operand::Reg(Register::Eax), Type::Int));
            x86_instructions.push(X86Insn::Cmp(X86Operand::Constant(0), X86Operand::Reg(Register::Eax)));
            x86_instructions.push(X86Insn::Jne(label)); // jump if condition != 0
        }
        Instruction::Exit { exit_code } => {
            x86_instructions.push(X86Insn::Mov(X86Operand::Constant(*exit_code), X86Operand::Reg(Register::Eax), Type::Int));
            x86_instructions.push(X86Insn::Exit);
        },
    }
}


/// Emit x86 code corresponding to the given CFG
/// Returns a vector of strings of x86 instructions.
fn generate_method_x86(method_name: &String, method_cfg: &mut CFG, globals: &Vec<Global>) -> Vec<X86Insn> {
    let mut x86_instructions: Vec<X86Insn> = Vec::new();

    if method_name == "main" {
        x86_instructions.push(X86Insn::Global(method_name.to_string()));
    }

    x86_instructions.push(X86Insn::Label(method_name.to_string()));

    // method prologue
    x86_instructions.push(X86Insn::Push(X86Operand::Reg(Register::Rbp))); // push base pointer onto stack
    x86_instructions.push(X86Insn::Mov(
        X86Operand::Reg(Register::Rsp),
        X86Operand::Reg(Register::Rbp),
        Type::Long
    )); // copy stack pointer to base pointer

    // Round up to the next 16-byte alignment
    let aligned_stack_size = (method_cfg.stack_size + 15) / 16 * 16;

    // round up method_cfg.stack_size to be 16-byte aligned
    x86_instructions.push(X86Insn::Sub(
        X86Operand::Constant(aligned_stack_size),
        X86Operand::Reg(Register::Rsp),
        Type::Long
    )); // decrease stack pointer to allocate space on the stack

    // global array lengths
    if method_name == "main" {
        for global in globals {
            if global.length.is_some() {
                // move array length into first array slot
                x86_instructions.push(X86Insn::Mov(X86Operand::Constant(i64::from(global.length.unwrap())), X86Operand::Reg(Register::Rax), global.typ.clone()));
                x86_instructions.push(X86Insn::Mov(X86Operand::Reg(Register::Rax), X86Operand::Global(global.name.to_string()), global.typ.clone()));
            }
        }
    }

    for (id, block) in method_cfg.get_blocks() {
        x86_instructions.push(X86Insn::Label(method_name.to_string() + &id.to_string()));

        for insn in block.get_instructions() {
            add_instruction(method_cfg, &insn, &mut x86_instructions);
        }

        if *id == method_cfg.exit {
            // method epilogue
            x86_instructions.push(X86Insn::Mov(
                X86Operand::Reg(Register::Rbp),
                X86Operand::Reg(Register::Rsp),
                Type::Long,
            )); // move base pointer to stack pointer
            x86_instructions.push(X86Insn::Pop(X86Operand::Reg(Register::Rbp))); // pop base pointer off stack
            x86_instructions.push(X86Insn::Ret); // return to where function was called
        }
    }

    x86_instructions
}



/// Generate x86 assembly code from the CFG/
pub fn generate_assembly(file: &str, filename: &str, writer: &mut dyn std::io::Write, debug: bool) {
    // Generate the method CFGS
    let (method_cfgs, globals, strings) = build_cfg(file, filename, writer, debug);
    if debug {
        print_cfg(&method_cfgs);
        println!("\n========== X86 Code ==========\n");
    }

    let mut global_code: Vec<X86Insn> = Vec::new();

    // global variables
    for global in &globals {
        let element_size = match global.typ {
            Type::Int
            | Type::Bool => INT_SIZE,
            Type::Long
            | Type::String
            | _ => LONG_SIZE,
        };
        if global.length.is_some() {
            // allocate an extra element's worth of space to store the length of the array
            // 8 byte alignment for now...
            global_code.push(X86Insn::Comm(
                global.name.clone(),
                element_size * i64::from(global.length.unwrap() + 1),
                element_size,
            ));
        } else {
            global_code.push(X86Insn::Comm(global.name.to_string(), element_size, element_size));
        }
    }

    // strings
    for (idx, string) in strings.iter().enumerate() {
        global_code.push(X86Insn::Label(format!("str{idx}")));
        global_code.push(X86Insn::String(string.to_string()));
    }

    writeln!(writer).expect("Failed to write newline after globals!");

    // Generate a vector of x86 for each method
    let mut code: HashMap<String, Vec<X86Insn>> = HashMap::new();
    for (method_name, mut method_cfg) in method_cfgs {
        let method_code = generate_method_x86(&method_name, &mut method_cfg, &globals);
        code.insert(method_name.clone(), method_code);
    }

    // Emit the final code
    for insn in &global_code {
        writeln!(writer, "{}", insn).expect("Failed to write instruction!");
    }

    for (_, method_code) in &code {
        for instr in method_code {
            writeln!(writer, "{}", instr).expect("Failed to write instruction!");
        }

        writeln!(writer).expect("Failed to write newline between methods!");
    }
}