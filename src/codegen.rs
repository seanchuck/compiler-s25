/**
 Generate x86 code from the Control flow graph.
 **/
use crate::cfg::{Global, Local};
use crate::cfg::{INT_SIZE, LONG_SIZE};
use crate::ast::Type;
use crate::dataflow::optimize_dataflow;
use crate::{tac::*};
use crate::utils::cli::Optimization;
use crate::utils::print::{html_cfgs, print_cfg};
use crate::x86::*;
use crate::{buildcfg::build_cfg, cfg::CFG};
use core::panic;
use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};


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

/// Gets the correct size scratch register for the given type
fn reg_for_type(reg: Register, typ: &Type) -> Register {
    match (reg.clone(), typ) {
        (_, Type::Long) => reg,
        (_, Type::String) => reg,
        (Register::Rax, _) => Register::Eax,
        (Register::R10, _) => Register::R10d,
        (Register::R11, _) => Register::R11d,
        (Register::R12, _) => Register::R12d,
        (Register::R13, _) => Register::R13d,
        (Register::R14, _) => Register::R14d,
        (Register::R15, _) => Register::R15d,
        (Register::R8, _) => Register::R8d,
        (Register::R9, _) => Register::R9d,
        (Register::Rcx, _) => Register::Ecx,
        (Register::Rdx, _) => Register::Edx,
        (Register::Rsi, _) => Register::Esi,
        (Register::Rdi, _) => Register::Edi,
        (Register::Rbx, _) => Register::Ebx,
        (Register::Rbp, _) => Register::Ebp,
        (Register::Rsp, _) => Register::Esp,
        _ => reg,
    }
}


/// Returns the x86 operand corresponding to operand
fn map_operand(
    method_cfg: &CFG,
    operand: &Operand,
    x86_instructions: &mut Vec<X86Insn>,
    globals: &BTreeMap<String, Global>
) -> X86Operand {
    if let Some(opt_reg) = operand.get_reg() {
        if let X86Operand::Reg(reg) = opt_reg {
            let replacement_reg = reg_for_type(reg, &operand.get_type());
            return X86Operand::Reg(replacement_reg);
        }
    }
    match operand {
        Operand::Const { value, .. } => X86Operand::Constant(*value),

        Operand::LocalVar { name, typ, reg } => {
            let typ = method_cfg.locals.get(name).expect("missing temp in scope").typ.clone();
            X86Operand::RegInt(Register::Rbp, method_cfg.get_stack_offset(name), typ)
            // X86Operand::RegInt(Register::Rbp, method_cfg.get_stack_offset(temp), Type::Long)
        }

        Operand::GlobalVar { name, .. } => X86Operand::Global(name.to_string()),

        Operand::LocalArrElement { name, index, typ, reg } => {
            let array_typ: Type = method_cfg.locals.get(name).expect("expected array entry").typ.clone();
            // println!("Matching operand for {:?}", index);
            let idx_op = map_operand(method_cfg, index, x86_instructions, globals);
            // println!("index operand is  {:?}", idx_op);

            let index_reg: Register = reg_for_type(Register::R10, &Type::Int);

            // Load base address of array into R11
            x86_instructions.push(X86Insn::Lea(
                X86Operand::RegInt(Register::Rbp, method_cfg.get_stack_offset(name), Type::Long),
                X86Operand::Reg(Register::R11),
            ));

            // Move index into R10 (64-bit regardless of int or long)
            x86_instructions.push(X86Insn::Mov(idx_op, X86Operand::Reg(index_reg.clone()), Type::Int));

            // add one to index because first element is length
            x86_instructions.push(X86Insn::Add(
                X86Operand::Constant(1),
                X86Operand::Reg(index_reg.clone()),
                Type::Int,
            ));

            let element_size = if array_typ == Type::Long {
                LONG_SIZE
            } else {
                INT_SIZE
            };


            // must use 64 bit version of index reg for addressing purposes
            X86Operand::Address(None, Some(Register::R11), Register::R10, element_size, array_typ)

        }

        Operand::GlobalArrElement { name, index, typ, reg } => {
            let array_typ: Type = globals.get(name).expect("Global array not defined!").typ.clone();
            let idx_op = map_operand(method_cfg, index, x86_instructions, globals);

            let index_reg: Register = reg_for_type(Register::R10, &Type::Int);

            x86_instructions.push(X86Insn::Mov(idx_op, X86Operand::Reg(index_reg.clone()), Type::Int)); // store index in r10d
           
            x86_instructions.push(X86Insn::Add(
                X86Operand::Constant(1),
                X86Operand::Reg(index_reg.clone()),
                Type::Int
            )); // add one to index because first element is length

            let element_size = if array_typ == Type::Long {
                LONG_SIZE
            } else {
                INT_SIZE
            };

            X86Operand::Address(Some(name.to_string()), None, Register::R10, element_size, typ.clone())
        }

        Operand::Argument { position, typ, reg } => {
            match position {
                0 => X86Operand::Reg(reg_for_type(Register::Rdi, typ)),
                1 => X86Operand::Reg(reg_for_type(Register::Rsi, typ)),
                2 => X86Operand::Reg(reg_for_type(Register::Rdx, typ)),
                3 => X86Operand::Reg(reg_for_type(Register::Rcx, typ)),
                4 => X86Operand::Reg(reg_for_type(Register::R8, typ)),
                5 => X86Operand::Reg(reg_for_type(Register::R9, typ)),
                _ => {
                    // Offset must now include space for 4 saved registers: R12–R15
                    // Layout:
                    //   RBP+0  = old RBP
                    //   RBP+8  = RA
                    //   RBP+16 = R12
                    //   RBP+24 = R13
                    //   RBP+32 = R14
                    //   RBP+40 = R15
                    //   RBP+48 = stack arg 0 (position 6)
        
                    let temp_name = method_cfg
                        .param_to_temp
                        .get(position)
                        .expect("Param mapping does not exist")
                        .name
                        .clone();
        
                    let typ = method_cfg
                        .locals
                        .get(&temp_name)
                        .expect("Expected a local param")
                        .typ
                        .clone();
        
                    let saved_regs_size = 8 * 4; // R12, R13, R14, R15
                    let offset = 16 + saved_regs_size + ((position - 6) as i64 * 8);
        
                    X86Operand::RegInt(Register::Rbp, offset, typ)
                }
            }
        }        
        Operand::String { id, .. } => X86Operand::RegLabel(Register::Rip, format!("str{id}")),
    }
}


// Adds the x86 instructions corresponding to insn to x86_instructions
fn add_instruction(method_cfg: &CFG,  insn: &Instruction, x86_instructions: &mut Vec<X86Insn>, globals: &BTreeMap<String, Global>) {
    // println!("Adding instruction: {:?}", insn);
    match insn {
        Instruction::LoadConst { src, dest, typ } => {
            match typ {
                Type::Int => {
                    // println!("Loading Const to {:?}", dest);
                    let dest_location = map_operand(method_cfg, dest, x86_instructions, globals);
                    x86_instructions.push(X86Insn::Mov(
                        X86Operand::Constant(src.clone()),
                        dest_location,
                        Type::Int
                    ));
                }
                Type::Long => {
                    // Must load uppper and lower 32 bits separately since assembler doesn't 
                    // support loading a 64-bit constant directly
                    x86_instructions.push(X86Insn::Mov(
                        X86Operand::Constant(((src.clone() as u64) & 0xFFFFFFFF) as i64),
                        X86Operand::Reg(Register::Rbx),
                        Type::Long
                    )); // lower 32 bits
                    x86_instructions.push(X86Insn::Mov(
                        X86Operand::Constant(((src.clone() as u64) >> 32) as i64),
                        X86Operand::Reg(Register::Rax),
                        Type::Long
                    )); // upper 32 bits
                    x86_instructions.push(X86Insn::Shl(
                        X86Operand::Constant(32),
                        X86Operand::Reg(Register::Rax),
                    ));
                    x86_instructions.push(X86Insn::Or(
                        X86Operand::Reg(Register::Rax),
                        X86Operand::Reg(Register::Rbx),
                    ));
                    let dest_location = map_operand(method_cfg, dest, x86_instructions, globals);
                    x86_instructions.push(X86Insn::Mov(X86Operand::Reg(Register::Rbx), dest_location, Type::Long));
                }
                _=> panic!("Load const only defined for numeric types")
            }
        }

        Instruction::Add { left, right, dest, typ } => {
            let left_op = map_operand(method_cfg, left, x86_instructions, globals);
            let right_op = map_operand(method_cfg, right, x86_instructions, globals);
            let dest_op = map_operand(method_cfg, dest, x86_instructions, globals);

            let reg = reg_for_type(Register::Rax, &typ);
            x86_instructions.push(X86Insn::Mov(left_op, X86Operand::Reg(reg.clone()), typ.clone()));
            x86_instructions.push(X86Insn::Add(right_op, X86Operand::Reg(reg.clone()), typ.clone()));
            x86_instructions.push(X86Insn::Mov(X86Operand::Reg(reg.clone()), dest_op, typ.clone()));
        }

        Instruction::Subtract { left, right, dest, typ } => {
            let left_op = map_operand(method_cfg, left, x86_instructions, globals);
            let right_op = map_operand(method_cfg, right, x86_instructions, globals);
            let dest_op = map_operand(method_cfg, dest, x86_instructions, globals);

            let reg = reg_for_type(Register::Rax, &typ);
            x86_instructions.push(X86Insn::Mov(left_op, X86Operand::Reg(reg.clone()), typ.clone()));
            x86_instructions.push(X86Insn::Sub(right_op, X86Operand::Reg(reg.clone()), typ.clone()));
            x86_instructions.push(X86Insn::Mov(X86Operand::Reg(reg.clone()), dest_op, typ.clone()));
        }

        Instruction::Assign { src, dest, ..} => {
            let dest_op = map_operand(method_cfg, dest, x86_instructions, globals);
            let src_op = map_operand(method_cfg, src, x86_instructions, globals);

            // println!("Assigning {:?} to {:?}", dest, dest_op);

            // // Reason that this matters is because we might have src as an array and we are using full
            // // 64 bits for the length if its long array, so we must move using src typ then only take
            // // The necessary dest bits for the final move
            let src_typ = src.get_type();
            let dest_typ = dest.get_type();

            let src_reg = reg_for_type(Register::Rax, &src_typ);
            let dst_reg = reg_for_type(Register::Rax, &dest_typ);

            x86_instructions.push(X86Insn::Mov(src_op, X86Operand::Reg(src_reg.clone()), src_typ.clone()));
            x86_instructions.push(X86Insn::Mov(X86Operand::Reg(dst_reg), dest_op, dest_typ.clone()));

        }
        
        Instruction::LoadString { src, dest } => {
            let dest_op = map_operand(method_cfg, dest, x86_instructions, globals);
            let src_op = map_operand(method_cfg, src, x86_instructions, globals);

            // string is always the full 64 bits
            x86_instructions.push(X86Insn::Lea(src_op, X86Operand::Reg(Register::Rax)));
            x86_instructions.push(X86Insn::Mov(X86Operand::Reg(Register::Rax), dest_op, Type::Long));
        }

        Instruction::MethodCall { name, args, dest, return_type } => {
            // Determine the destination operand
            let dest_op = match dest {
                Some(d) => map_operand(method_cfg, d, x86_instructions, globals),
                None => X86Operand::Reg(Register::Rax),
            };

            // Setup arguments
            // First 6 args go in registers
            for (i, arg) in args.iter().take(6).enumerate() {
                let arg_typ = arg.get_type();
                let arg_reg = 
                    map_operand(method_cfg, &Operand::Argument { position: i as i32, typ: arg_typ.clone(), reg: None }, x86_instructions, globals);
                let arg_val = map_operand(method_cfg, arg, x86_instructions, globals);
                x86_instructions.push(X86Insn::Mov(arg_val, arg_reg, arg_typ));
            }

            // Arguments {7...n} go on stack, with last args going first; assume stack 16-aligned before call
            let mut sp_offset = 0;
            for arg in args.iter().skip(6) {
                let arg_typ = arg.get_type();
                let arg_val = map_operand(method_cfg, arg, x86_instructions, globals);

                let reg = reg_for_type(Register::Rax, &arg_typ);
                x86_instructions.push(X86Insn::Mov(
                    arg_val.clone(),
                    X86Operand::Reg(reg),
                    arg_typ
                ));
                x86_instructions.push(X86Insn::Mov(
                    X86Operand::Reg(Register::Rax),
                    X86Operand::RegInt(Register::Rsp, sp_offset, Type::Long),
                    Type::Long
                ));

                sp_offset += 8;
            }

            // Zero rax before call
            x86_instructions.push(X86Insn::Mov(
                X86Operand::Constant(0),
                X86Operand::Reg(Register::Rax),
                Type::Long
            ));

            // Make the call
            x86_instructions.push(X86Insn::Call(name.to_string()));

            let return_reg = reg_for_type(Register::Rax, return_type);

            // Move the result into `dest` if necessary
            match dest_op {
                X86Operand::Reg(Register::Rax) => {}
                _ => x86_instructions.push(X86Insn::Mov(X86Operand::Reg(return_reg), dest_op, return_type.clone())),
            }
        }
        Instruction::Ret { value, typ } => {
            // TODO: uncomment
            let return_reg = reg_for_type(Register::Rax, &typ);
            // let return_reg = Register::Rax;
            if let Some(value) = value {
                let value_reg = map_operand(method_cfg, value, x86_instructions, globals);
                x86_instructions.push(X86Insn::Mov(value_reg, X86Operand::Reg(return_reg), typ.clone()));
            }

            // Function prologue
            x86_instructions.push(X86Insn::Mov(
                X86Operand::Reg(Register::Rbp),
                X86Operand::Reg(Register::Rsp),
                Type::Long
            ));

            x86_instructions.push(X86Insn::Pop(X86Operand::Reg(Register::R15)));
            x86_instructions.push(X86Insn::Pop(X86Operand::Reg(Register::R14)));
            x86_instructions.push(X86Insn::Pop(X86Operand::Reg(Register::R13)));
            x86_instructions.push(X86Insn::Pop(X86Operand::Reg(Register::R12)));
            x86_instructions.push(X86Insn::Pop(X86Operand::Reg(Register::Rbp)));
            x86_instructions.push(X86Insn::Ret);
        }
        Instruction::Multiply { left, right, dest, typ } => {
            let left_op = map_operand(method_cfg, left, x86_instructions, globals);
            let right_op = map_operand(method_cfg, right, x86_instructions, globals);
            let dest_op = map_operand(method_cfg, dest, x86_instructions, globals);

            let reg = reg_for_type(Register::Rax, &typ);
            x86_instructions.push(X86Insn::Mov(left_op, X86Operand::Reg(reg.clone()), typ.clone()));
            x86_instructions.push(X86Insn::Mul(right_op, X86Operand::Reg(reg.clone())));
            x86_instructions.push(X86Insn::Mov(X86Operand::Reg(reg.clone()), dest_op, typ.clone()));
        }
        Instruction::Divide { left, right, dest, typ } => {
            let left_op = map_operand(method_cfg, left, x86_instructions, globals);
            let right_op = map_operand(method_cfg, right, x86_instructions, globals);
            let dest_op = map_operand(method_cfg, dest, x86_instructions, globals);

            // Signed division in x86:
            match typ {
                Type::Int => {
                    // 32-bit signed division:
                    // Dividend in EAX, sign-extended into EDX using CDQ
                    x86_instructions.push(X86Insn::Mov(left_op, X86Operand::Reg(Register::Eax), Type::Int));
                    x86_instructions.push(X86Insn::Cdq); // Sign-extend EAX into EDX
                    x86_instructions.push(X86Insn::Mov(right_op, X86Operand::Reg(Register::Ecx), Type::Int));
                    x86_instructions.push(X86Insn::Div(X86Operand::Reg(Register::Ecx), Type::Int)); // Divide EDX:EAX by ECX
                    x86_instructions.push(X86Insn::Mov(X86Operand::Reg(Register::Eax), dest_op, Type::Int));
                }
                Type::Long => {
                    // 64-bit signed division:
                    // Dividend in RAX, sign-extended into RDX using CQO
                    x86_instructions.push(X86Insn::Mov(left_op, X86Operand::Reg(Register::Rax), Type::Long));
                    x86_instructions.push(X86Insn::Cqto); // Sign-extend RAX into RDX
                    x86_instructions.push(X86Insn::Mov(right_op, X86Operand::Reg(Register::Rcx), Type::Long));
                    x86_instructions.push(X86Insn::Div(X86Operand::Reg(Register::Rcx), Type::Long)); // Divide RDX:RAX by RCX
                    x86_instructions.push(X86Insn::Mov(X86Operand::Reg(Register::Rax), dest_op, Type::Long));
                }
                _ => panic!("Divide only supported for int or long types"),
            }
        }
        Instruction::Modulo { left, right, dest, typ } => {
            let left_op: X86Operand = map_operand(method_cfg, left, x86_instructions, globals);
            let right_op = map_operand(method_cfg, right, x86_instructions, globals);
            let dest_op = map_operand(method_cfg, dest, x86_instructions, globals);
        
            match typ {
                Type::Int => {
                    // Signed modulo in x86 (32-bit): dividend in EAX, sign-extended into EDX using CDQ
                    x86_instructions.push(X86Insn::Mov(left_op, X86Operand::Reg(Register::Eax), Type::Int));
                    x86_instructions.push(X86Insn::Cdq); // Sign-extend EAX into EDX
                    x86_instructions.push(X86Insn::Mov(right_op, X86Operand::Reg(Register::Ecx), Type::Int));
                    x86_instructions.push(X86Insn::Div(X86Operand::Reg(Register::Ecx), Type::Int));
                    x86_instructions.push(X86Insn::Mov(X86Operand::Reg(Register::Edx), dest_op, Type::Int));
                }
                Type::Long => {
                    // Signed modulo in x86 (64-bit): dividend in RAX, sign-extended into RDX using CQO
                    x86_instructions.push(X86Insn::Mov(left_op, X86Operand::Reg(Register::Rax), Type::Long));
                    x86_instructions.push(X86Insn::Cqto); // Sign-extend RAX into RDX
                    x86_instructions.push(X86Insn::Mov(right_op, X86Operand::Reg(Register::Rcx), Type::Long));
                    x86_instructions.push(X86Insn::Div(X86Operand::Reg(Register::Rcx), Type::Long));
                    x86_instructions.push(X86Insn::Mov(X86Operand::Reg(Register::Rdx), dest_op, Type::Long));
                }
                _ => panic!("Modulo only supported for int or long types"),
            }
        }        
        Instruction::Not { expr, dest } => {
            let expr_op = map_operand(method_cfg, expr, x86_instructions, globals);
            let dest_op = map_operand(method_cfg, dest, x86_instructions, globals);

            let reg = reg_for_type(Register::Rax, &Type::Bool);
            // Move expr to RAX (or any scratch reg), xor with 1
            x86_instructions.push(X86Insn::Mov(expr_op, X86Operand::Reg(reg.clone()), Type::Bool));
            x86_instructions.push(X86Insn::Xor(
                X86Operand::Constant(1),
                X86Operand::Reg(reg.clone()),
            ));
            x86_instructions.push(X86Insn::Mov(X86Operand::Reg(reg.clone()), dest_op, Type::Bool));
        }
        Instruction::Cast {
            expr,
            dest,
            target_type,
        } => {
            let dest_op: X86Operand = map_operand(method_cfg, dest, x86_instructions, globals);

            if let Some(expr_reg) = expr.get_reg(){
                if let X86Operand::Reg(register) = expr_reg {
                    let sized_reg = reg_for_type(register, target_type);
                    x86_instructions.push(X86Insn::Mov(X86Operand::Reg(sized_reg), dest_op, target_type.clone()));
                    return;
                }
            }
            let expr_op = map_operand(method_cfg, expr, x86_instructions, globals);

            let expr_typ = expr.get_type();
            let expr_reg = reg_for_type(Register::Rax, &expr_typ);

            match target_type {
                crate::ast::Type::Int => {
                    x86_instructions.push(X86Insn::Mov(expr_op, X86Operand::Reg(Register::Eax), Type::Int));
                    x86_instructions.push(X86Insn::Mov(X86Operand::Reg(Register::Eax), dest_op, Type::Int));
                }
                crate::ast::Type::Long => {
                    x86_instructions.push(X86Insn::Mov(expr_op, X86Operand::Reg(expr_reg.clone()), expr_typ.clone()));
                    if expr_typ == Type::Int {  // only sign extend if its an int
                        x86_instructions.push(X86Insn::Movsxd(X86Operand::Reg(expr_reg.clone()), X86Operand::Reg(Register::Rax)));
                    } else {                    // Otherwise do basic move
                        x86_instructions.push(X86Insn::Mov(X86Operand::Reg(expr_reg.clone()), X86Operand::Reg(Register::Rax), expr_typ.clone()));
                    }
                    x86_instructions.push(X86Insn::Mov(X86Operand::Reg(Register::Rax), dest_op, Type::Long));
                }
                _ => panic!("Shouldnt get here, cannot cast non int or long value"),
            }
        }
        Instruction::Len { expr, dest, .. } => {
                let expr_typ = expr.get_type();
                let dest_typ = dest.get_type();

                let expr_reg = reg_for_type(Register::Rax, &expr_typ);
                let dest_reg = reg_for_type(Register::Rax, &dest_typ);

                let expr_op = map_operand(method_cfg, expr, x86_instructions, globals);
                let dest_op = map_operand(method_cfg, dest, x86_instructions, globals);

                // First move is dependent on entry sizes for the array. second will always produce an integer
                x86_instructions.push(X86Insn::Mov(expr_op, X86Operand::Reg(expr_reg), expr_typ.clone()));
                x86_instructions.push(X86Insn::Mov(X86Operand::Reg(dest_reg), dest_op, dest_typ));
        }
        Instruction::Greater { left, right, dest }
        | Instruction::Less { left, right, dest }
        | Instruction::LessEqual { left, right, dest }
        | Instruction::GreaterEqual { left, right, dest }
        | Instruction::Equal { left, right, dest }
        | Instruction::NotEqual { left, right, dest } => {
            let mut left_op = map_operand(method_cfg, left, x86_instructions, globals);
            let mut right_op = map_operand(method_cfg, right, x86_instructions, globals);
            let dest_op = map_operand(method_cfg, dest, x86_instructions, globals);
            let mut swapped = false;

            let left_typ = left.get_type();
            let left_reg = reg_for_type(Register::Rax, &left_typ);

            // handle illegal cmp: (mem, mem)
            if is_memory_operand(&left_op) && is_memory_operand(&right_op) {
                // move value into a register
                x86_instructions.push(X86Insn::Mov(
                    left_op.clone(),
                    X86Operand::Reg(left_reg.clone()),
                    left_typ.clone()
                ));
                left_op = X86Operand::Reg(left_reg.clone());
            }

            // handle illegal cmp: (imm, imm),
            if is_immediate_operand(&left_op) && is_immediate_operand(&right_op) {
                x86_instructions.push(X86Insn::Mov(
                    left_op.clone(),
                    X86Operand::Reg(left_reg.clone()),
                    left_typ.clone()
                ));
                left_op = X86Operand::Reg(left_reg);
            }

            // handle illegal cmp: (mem, imm), (reg, imm) --> imm must come first
            if is_immediate_operand(&left_op) && !is_immediate_operand(&right_op) {
                // immediate must come first → flip
                std::mem::swap(&mut left_op, &mut right_op);
                swapped = true; // must swap the boolean operator since swapped operands!
            }

            // x86 swaps right and left for cmp
            x86_instructions.push(X86Insn::Cmp(right_op, left_op, left_typ));

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
            x86_instructions.push(X86Insn::Movzbq(
                X86Operand::Reg(Register::Al),
                X86Operand::Reg(Register::Rax),
            ));
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
            let cond_op = map_operand(method_cfg, condition, x86_instructions, globals);

            // cmp condition, 0 → is condition true?
            x86_instructions.push(X86Insn::Mov(cond_op, X86Operand::Reg(Register::Eax), Type::Int));
            x86_instructions.push(X86Insn::Cmp(
                X86Operand::Constant(0),
                X86Operand::Reg(Register::Eax),
                Type::Bool
            ));
            x86_instructions.push(X86Insn::Jne(label)); // jump if condition != 0
        }
        Instruction::Exit { exit_code } => {
            x86_instructions.push(X86Insn::Mov(
                X86Operand::Constant(*exit_code),
                X86Operand::Reg(Register::Rax),
                Type::Long
            ));
            x86_instructions.push(X86Insn::Exit);
        }
    }
}

/// Emit x86 code corresponding to the given CFG
/// Returns a vector of strings of x86 instructions.
fn generate_method_x86(
    method_name: &String,
    method_cfg: &mut CFG,
    globals: &BTreeMap<String, Global>
) -> Vec<X86Insn> {
    let mut x86_instructions: Vec<X86Insn> = Vec::new();

    if method_name == "main" {
        x86_instructions.push(X86Insn::Global(method_name.to_string()));
    }

    x86_instructions.push(X86Insn::Label(method_name.to_string()));

    // method prologue
    x86_instructions.push(X86Insn::Push(X86Operand::Reg(Register::Rbp))); // push base pointer onto stack
    x86_instructions.push(X86Insn::Push(X86Operand::Reg(Register::R12)));
    x86_instructions.push(X86Insn::Push(X86Operand::Reg(Register::R13)));
    x86_instructions.push(X86Insn::Push(X86Operand::Reg(Register::R14)));
    x86_instructions.push(X86Insn::Push(X86Operand::Reg(Register::R15)));

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
        for global in globals.values() {
            if global.length.is_some() {
                // rax as working register
                x86_instructions.push(X86Insn::Mov(
                    X86Operand::Constant(i64::from(global.length.unwrap())),
                    X86Operand::Reg(Register::Rax),
                    Type::Long,
                ));
                x86_instructions.push(X86Insn::Mov(
                    X86Operand::Reg(Register::Rax),
                    X86Operand::Global(global.name.clone()),
                    Type::Long,
                ));
            }
        }
    }    

    for (id, block) in method_cfg.get_blocks() {
        x86_instructions.push(X86Insn::Label(method_name.to_string() + &id.to_string()));

        for insn in block.get_instructions() {
            add_instruction(method_cfg, &insn, &mut x86_instructions, globals);
        }

        if *id == method_cfg.exit {
            // method epilogue
            x86_instructions.push(X86Insn::Mov(
                X86Operand::Reg(Register::Rbp),
                X86Operand::Reg(Register::Rsp),
                Type::Long
            )); // move base pointer to stack pointer

            x86_instructions.push(X86Insn::Pop(X86Operand::Reg(Register::R15)));
            x86_instructions.push(X86Insn::Pop(X86Operand::Reg(Register::R14)));
            x86_instructions.push(X86Insn::Pop(X86Operand::Reg(Register::R13)));
            x86_instructions.push(X86Insn::Pop(X86Operand::Reg(Register::R12)));
            x86_instructions.push(X86Insn::Pop(X86Operand::Reg(Register::Rbp))); // pop base pointer off stack

            x86_instructions.push(X86Insn::Ret); // return to where function was called
        }
    }

    x86_instructions
}

/// Generate x86 assembly code from the CFG/
pub fn generate_assembly(file: &str, filename: &str, optimizations: BTreeSet<Optimization>, writer: &mut dyn std::io::Write, debug: bool) {
    // Generate the method CFGS
    let (mut method_cfgs, globals, strings) = build_cfg(file, filename, writer, debug);

    // if debug {
    //     html_cfgs(&method_cfgs, "no-opt.html".to_string());
    //     println!("\n========== X86 Code ==========\n");
    // }

    // Perform dataflow optimizations (includes register allocation)
    optimize_dataflow(&mut method_cfgs, &optimizations, debug);
    
    if debug {
        print_cfg(&method_cfgs);
        html_cfgs(&method_cfgs, "opt.html".to_string());
        println!("\n========== X86 Code ==========\n");
    }


    let mut global_code: Vec<X86Insn> = Vec::new();

    // global variables
    for global in globals.values() {
        let type_length = match global.typ {
            Type::Int => INT_SIZE,
            Type::Long => LONG_SIZE,
            Type::Bool => INT_SIZE,
            _ => panic!("Should not have had this type global")
        };

        if global.length.is_some() {
            // allocate an extra element's worth of space to store the length of the array
            global_code.push(X86Insn::Comm(
                global.name.clone(),
                type_length * i64::from(global.length.unwrap() + 1),
                type_length,
            ));
        } else {
            global_code.push(X86Insn::Comm(
                global.name.clone(),
                type_length,
                type_length,
            ));
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
    for (method_name, method_cfg) in &method_cfgs {
        let mut method_cfg = method_cfg.clone();
        let method_code = generate_method_x86(method_name, &mut method_cfg, &globals);
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
