use crate::ast::Type;
/**
Generate x86 code from the Control flow graph.
**/
use crate::cfg::{Global, Local};
use crate::cfg::{INT_SIZE, LONG_SIZE};
use crate::dataflow::optimize_dataflow;
use crate::tac::*;
use crate::utils::cli::Optimization;
use crate::utils::print::{html_cfgs, print_cfg};
use crate::x86::*;
use crate::{buildcfg::build_cfg, cfg::CFG};
use core::panic;
use std::collections::{BTreeMap, BTreeSet, HashMap};

pub const CALLEE_SAVED_REGISTERS: [Register; 5] = [
    Register::Rbx,
    Register::R12,
    Register::R13,
    Register::R14,
    Register::R15,
];

pub const ARGUMENT_REGISTERS: [X86Operand; 6] = [
    X86Operand::Reg(Register::Rdi),
    X86Operand::Reg(Register::Rsi),
    X86Operand::Reg(Register::Rdx),
    X86Operand::Reg(Register::Rcx),
    X86Operand::Reg(Register::R8),
    X86Operand::Reg(Register::R9),
];

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
    globals: &BTreeMap<String, Global>,
) -> X86Operand {
    if let Some(opt_reg) = operand.get_reg() {
        if let X86Operand::Reg(reg) = opt_reg {
            let replacement_reg = reg_for_type(reg, &operand.get_type());
            return X86Operand::Reg(replacement_reg);
        }
    }
    match operand {
        Operand::Const { value, .. } => X86Operand::Constant(*value),

        Operand::LocalVar { name, .. } => {
            let typ = method_cfg.locals.get(name).expect("missing temp in scope").typ.clone();
            X86Operand::RegInt(Register::Rbp, method_cfg.get_stack_offset(name), typ)
            // X86Operand::RegInt(Register::Rbp, method_cfg.get_stack_offset(temp), Type::Long)
        }

        Operand::GlobalVar { name, .. } => X86Operand::Global(name.to_string()),

        Operand::LocalArrElement { name, index, .. } => {
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
            x86_instructions.push(X86Insn::Mov(
                idx_op,
                X86Operand::Reg(index_reg.clone()),
                Type::Int,
            ));

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
            X86Operand::Address(
                None,
                Some(Register::R11),
                Register::R10,
                element_size,
                array_typ,
            )
        }

        Operand::GlobalArrElement { name, index, typ, .. } => {
            let array_typ: Type = globals.get(name).expect("Global array not defined!").typ.clone();
            let idx_op = map_operand(method_cfg, index, x86_instructions, globals);

            let index_reg: Register = reg_for_type(Register::R10, &Type::Int);

            x86_instructions.push(X86Insn::Mov(
                idx_op,
                X86Operand::Reg(index_reg.clone()),
                Type::Int,
            )); // store index in r10d

            x86_instructions.push(X86Insn::Add(
                X86Operand::Constant(1),
                X86Operand::Reg(index_reg.clone()),
                Type::Int,
            )); // add one to index because first element is length

            let element_size = if array_typ == Type::Long {
                LONG_SIZE
            } else {
                INT_SIZE
            };

            X86Operand::Address(
                Some(name.to_string()),
                None,
                Register::R10,
                element_size,
                typ.clone(),
            )
        }

        Operand::Argument { position, typ, .. } => {
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

                    let num_callee_saved_used = CALLEE_SAVED_REGISTERS
                        .iter()
                        .filter(|r| {
                            method_cfg
                                .get_reg_allocs()
                                .contains(&X86Operand::Reg((**r).clone()))
                        })
                        .count() as i64;
                    let saved_regs_size = 8 * num_callee_saved_used;
                    let offset = 16 + saved_regs_size + ((position - 6) as i64 * 8);

                    X86Operand::RegInt(Register::Rbp, offset, typ)
                }
            }
        }
        Operand::String { id, .. } => X86Operand::RegLabel(Register::Rip, format!("str{id}")),
    }
}

fn magic_number_signed(divisor: i64, is_64: bool) -> (i64, u8) {
    let w: u32 = if is_64 { 64 } else { 32 };
    // 2^(w-1)
    let two_w_1: u128 = 1u128 << (w - 1);
    let ad: u128 = divisor.abs() as u128;
    // sign_bit = 1 if divisor < 0, else 0
    let sign_bit: u128 = ((divisor as u128) >> (w - 1)) & 1;
    // t = 2^(w-1) + sign_bit
    let t: u128 = two_w_1 + sign_bit;
    // anc = t - 1 - (t mod ad)
    let anc: u128 = t - 1 - (t % ad);

    // initialize p, q1/r1 for |nc|, and q2/r2 for |d|
    let mut p: u32 = w - 1;
    let mut q1: u128 = two_w_1 / anc;
    let mut r1: u128 = two_w_1 - q1 * anc;
    let mut q2: u128 = two_w_1 / ad;
    let mut r2: u128 = two_w_1 - q2 * ad;

    // loop until q1 ≥ (ad - r2), with the extra check for equality/r1==0
    loop {
        p += 1;
        // update quotient/remainder for |nc|
        q1 <<= 1;
        r1 <<= 1;
        if r1 >= anc {
            q1 += 1;
            r1 -= anc;
        }
        // update quotient/remainder for |d|
        q2 <<= 1;
        r2 <<= 1;
        if r2 >= ad {
            q2 += 1;
            r2 -= ad;
        }

        let delta = ad - r2;
        // break when q1 > delta or (q1==delta && r1!=0)
        if q1 > delta || (q1 == delta && r1 != 0) {
            break;
        }
    }

    // magic = q2 + 1, with sign adjustment
    let mut m = (q2 + 1) as i128;
    if divisor < 0 {
        m = -m;
    }
    // s = p - w
    let s = (p - w) as u8;
    (m as i64, s)
}



// Adds the x86 instructions corresponding to insn to x86_instructions
fn add_instruction(
    method_cfg: &CFG,
    insn: &Instruction,
    x86_instructions: &mut Vec<X86Insn>,
    globals: &BTreeMap<String, Global>,
    reg_alloc: bool,
) {
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
                        Type::Int,
                    ));
                }
                Type::Long => {
                    let dest_location = map_operand(method_cfg, dest, x86_instructions, globals);
                    if dest.get_reg().is_some() {
                        x86_instructions.push(X86Insn::Loadlong(*src, dest_location));
                    } else {
                        x86_instructions.push(X86Insn::Loadlong(*src, X86Operand::Reg(Register::Rax)));
                        x86_instructions.push(X86Insn::Mov(X86Operand::Reg(Register::Rax), dest_location, Type::Long));
                    }
                }
                _ => panic!("Load const only defined for numeric types"),
            }
        }

        Instruction::Add {
            left,
            right,
            dest,
            typ,
        } => {
            let left_op = map_operand(method_cfg, left, x86_instructions, globals);
            let right_op = map_operand(method_cfg, right, x86_instructions, globals);
            let dest_op = map_operand(method_cfg, dest, x86_instructions, globals);

            if let Some(_) = dest.get_reg() {
                if dest_op != right_op {
                    x86_instructions.push(X86Insn::Mov(
                        left_op.clone(),
                        dest_op.clone(),
                        typ.clone(),
                    ));
                    x86_instructions.push(X86Insn::Add(
                        right_op.clone(),
                        dest_op.clone(),
                        typ.clone(),
                    ));
                    return;
                } else if dest_op != left_op {
                    x86_instructions.push(X86Insn::Mov(
                        right_op.clone(),
                        dest_op.clone(),
                        typ.clone(),
                    ));
                    x86_instructions.push(X86Insn::Add(
                        left_op.clone(),
                        dest_op.clone(),
                        typ.clone(),
                    ));
                    return;
                }
            }

            let reg = reg_for_type(Register::Rax, &typ);
            x86_instructions.push(X86Insn::Mov(
                left_op,
                X86Operand::Reg(reg.clone()),
                typ.clone(),
            ));
            x86_instructions.push(X86Insn::Add(
                right_op,
                X86Operand::Reg(reg.clone()),
                typ.clone(),
            ));
            x86_instructions.push(X86Insn::Mov(
                X86Operand::Reg(reg.clone()),
                dest_op,
                typ.clone(),
            ));
        }

        Instruction::Subtract {
            left,
            right,
            dest,
            typ,
        } => {
            let left_op = map_operand(method_cfg, left, x86_instructions, globals);
            let right_op = map_operand(method_cfg, right, x86_instructions, globals);
            let dest_op = map_operand(method_cfg, dest, x86_instructions, globals);

            if let Some(_) = dest.get_reg() {
                if dest_op != right_op {
                    x86_instructions.push(X86Insn::Mov(
                        left_op.clone(),
                        dest_op.clone(),
                        typ.clone(),
                    ));
                    x86_instructions.push(X86Insn::Sub(
                        right_op.clone(),
                        dest_op.clone(),
                        typ.clone(),
                    ));
                    return;
                }
            }

            let reg = reg_for_type(Register::Rax, &typ);
            x86_instructions.push(X86Insn::Mov(
                left_op,
                X86Operand::Reg(reg.clone()),
                typ.clone(),
            ));
            x86_instructions.push(X86Insn::Sub(
                right_op,
                X86Operand::Reg(reg.clone()),
                typ.clone(),
            ));
            x86_instructions.push(X86Insn::Mov(
                X86Operand::Reg(reg.clone()),
                dest_op,
                typ.clone(),
            ));
        }

        Instruction::Assign { src, dest, .. } => {
            // Do not waste time moving args in if they are the first 6 bc of precolor for regalloc
            if reg_alloc {
                if let Operand::Argument { position, .. } = src {
                    if *position <= 5 {
                        return;
                    }
                }
            }

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

            // If either src or dest is a register, just do the move
            if src.get_reg().is_some() || dest.get_reg().is_some() {
                x86_instructions.push(X86Insn::Mov(
                    src_op.clone(),
                    dest_op.clone(),
                    dest_typ.clone(),
                ));
                return;
            }

            x86_instructions.push(X86Insn::Mov(
                src_op,
                X86Operand::Reg(src_reg.clone()),
                src_typ.clone(),
            ));
            x86_instructions.push(X86Insn::Mov(
                X86Operand::Reg(dst_reg),
                dest_op,
                dest_typ.clone(),
            ));
        }

        Instruction::LoadString { src, dest } => {
            let dest_op = map_operand(method_cfg, dest, x86_instructions, globals);
            let src_op = map_operand(method_cfg, src, x86_instructions, globals);

            // string is always the full 64 bits
            x86_instructions.push(X86Insn::Lea(src_op, X86Operand::Reg(Register::Rax)));
            x86_instructions.push(X86Insn::Mov(
                X86Operand::Reg(Register::Rax),
                dest_op,
                Type::Long,
            ));
        }

        Instruction::MethodCall {
            name,
            args,
            dest,
            return_type,
        } => {
            // Determine the destination operand
            let dest_op = match dest {
                Some(d) => map_operand(method_cfg, d, x86_instructions, globals),
                None => X86Operand::Reg(Register::Rax),
            };

            // 1) Spill extra args to [rsp + 0 ... rsp + 8*(extra_args-1)]
            let mut sp_offset = 0;
            for arg in args.iter().skip(6) {
                let arg_typ = arg.get_type();
                let arg_val = map_operand(method_cfg, arg, x86_instructions, globals);
                let tmp_reg = reg_for_type(Register::Rax, &arg_typ);
                // move the argument into RAX
                x86_instructions.push(X86Insn::Mov(
                    arg_val.clone(),
                    X86Operand::Reg(tmp_reg.clone()),
                    arg_typ.clone(),
                ));

                // store all 8 bytes of RAX into our [rsp+sp_offset] slot
                x86_instructions.push(X86Insn::Mov(
                    X86Operand::Reg(Register::Rax),
                    X86Operand::RegInt(Register::Rsp, sp_offset, Type::Long),
                    Type::Long,
                ));
                sp_offset += 8;
            }

            // 2) Save caller‑saved ARGUMENT_REGISTERS into the *next* slots:
            //    slot_base = 8 * (number of extra args)
            //    then each register gets its own 8‑byte slot in order.
            let mut reg_to_slot: HashMap<Register, i64> = HashMap::new();
            let mut saved = Vec::new();
            let slot_base = sp_offset;
            for (i, reg_op) in ARGUMENT_REGISTERS.iter().enumerate() {
                if method_cfg.get_reg_allocs().contains(reg_op) {
                    if let X86Operand::Reg(r) = reg_op {
                        let slot = slot_base + (i as i64) * LONG_SIZE;
                        // movq r -> [rsp + slot]
                        x86_instructions.push(X86Insn::Mov(
                            X86Operand::Reg(r.clone()),
                            X86Operand::RegInt(Register::Rsp, slot, Type::Long),
                            Type::Long,
                        ));
                        reg_to_slot.insert(r.clone(), slot);
                        saved.push((r.clone(), slot));
                    }
                }
            }

            // -- 3) Setup arguments
            // First 6 args go in registers
            for (i, arg) in args.iter().take(6).enumerate() {
                let arg_typ = arg.get_type();
                let arg_reg = map_operand(
                    method_cfg,
                    &Operand::Argument {
                        position: i as i32,
                        typ: arg_typ.clone(),
                        reg: None,
                    },
                    x86_instructions,
                    globals,
                );
                let mut arg_val = map_operand(method_cfg, arg, x86_instructions, globals);

                // If the argument is usually in reg saved on stack, use its location is the stack location instead
                if let X86Operand::Reg(reg) = arg_val.clone() {
                    if let Some(slot) = reg_to_slot.get(&reg.reg_to_64()) {
                        arg_val = X86Operand::RegInt(Register::Rsp, *slot, arg_typ.clone());
                    }
                }

                x86_instructions.push(X86Insn::Mov(arg_val, arg_reg, arg_typ));
            }

            // -- 4) Zero RAX before call (varargs ABI requirement) --
            x86_instructions.push(X86Insn::Xor(
                X86Operand::Reg(Register::Rax),
                X86Operand::Reg(Register::Rax),
                Type::Long
            ));

            // -- 5) Call! --
            x86_instructions.push(X86Insn::Call(name.to_string()));

            // -- 6) Restore saved registers from their stack slots --
            for (r, slot) in saved.iter() {
                // movq [rsp + slot] -> r
                x86_instructions.push(X86Insn::Mov(
                    X86Operand::RegInt(Register::Rsp, *slot, Type::Long),
                    X86Operand::Reg(r.clone()),
                    Type::Long,
                ));
            }

            // -- 7) Move return value into dest if needed --
            let return_reg = reg_for_type(Register::Rax, return_type);
            match dest_op {
                X86Operand::Reg(Register::Rax) => { /* already there */ }
                _ => x86_instructions.push(X86Insn::Mov(
                    X86Operand::Reg(return_reg),
                    dest_op,
                    return_type.clone(),
                )),
            }
        }

        Instruction::Ret { value, typ } => {
            let return_reg = reg_for_type(Register::Rax, &typ);
            // let return_reg = Register::Rax;
            if let Some(value) = value {
                let value_reg = map_operand(method_cfg, value, x86_instructions, globals);
                x86_instructions.push(X86Insn::Mov(
                    value_reg,
                    X86Operand::Reg(return_reg),
                    typ.clone(),
                ));
            }

            // Function prologue
            x86_instructions.push(X86Insn::Mov(
                X86Operand::Reg(Register::Rbp),
                X86Operand::Reg(Register::Rsp),
                Type::Long,
            ));

            x86_instructions.push(X86Insn::Pop(X86Operand::Reg(Register::Rbp)));
            for reg in CALLEE_SAVED_REGISTERS.iter().rev() {
                if method_cfg
                    .get_reg_allocs()
                    .contains(&X86Operand::Reg(reg.clone()))
                {
                    x86_instructions.push(X86Insn::Pop(X86Operand::Reg(reg.clone())));
                }
            }
            x86_instructions.push(X86Insn::Ret);
        }
        Instruction::Multiply {
            left,
            right,
            dest,
            typ,
        } => {
            let left_op = map_operand(method_cfg, left, x86_instructions, globals);
            let right_op = map_operand(method_cfg, right, x86_instructions, globals);
            let dest_op = map_operand(method_cfg, dest, x86_instructions, globals);

            if let Some(_) = dest.get_reg() {
                if dest_op != right_op {
                    x86_instructions.push(X86Insn::Mov(
                        left_op.clone(),
                        dest_op.clone(),
                        typ.clone(),
                    ));
                    x86_instructions.push(X86Insn::Mul(right_op.clone(), dest_op.clone()));
                    return;
                } else if dest_op != left_op {
                    x86_instructions.push(X86Insn::Mov(
                        right_op.clone(),
                        dest_op.clone(),
                        typ.clone(),
                    ));
                    x86_instructions.push(X86Insn::Mul(left_op.clone(), dest_op.clone()));
                    return;
                }
            }

            let reg = reg_for_type(Register::Rax, &typ);
            x86_instructions.push(X86Insn::Mov(
                left_op,
                X86Operand::Reg(reg.clone()),
                typ.clone(),
            ));
            x86_instructions.push(X86Insn::Mul(right_op, X86Operand::Reg(reg.clone())));
            x86_instructions.push(X86Insn::Mov(
                X86Operand::Reg(reg.clone()),
                dest_op,
                typ.clone(),
            ));
        }
        Instruction::Divide {
            left,
            right,
            dest,
            typ,
        } => {
            let left_op = map_operand(method_cfg, left, x86_instructions, globals);
            let dest_op = map_operand(method_cfg, dest, x86_instructions, globals);

            if let Operand::Const { value, .. } = right {
                // Optimization only applies to constants ≠ 0 and ≠ 1
                let divisor: i64 = *value;
                let abs_divisor = divisor.abs() as u64;

                if divisor == 0 {
                    panic!("Division by zero");
                } else if abs_divisor.is_power_of_two() {  //SUS
                    // Division by power of 2
                    // Use arithmetic shift for signed divide by power of two
                    let shift = abs_divisor.trailing_zeros();                // always positive
                    x86_instructions.push(X86Insn::Mov(left_op.clone(), dest_op.clone(), typ.clone()));
                    x86_instructions.push(X86Insn::SarImm(shift, dest_op.clone()));
                    if divisor < 0 {
                        x86_instructions.push(X86Insn::Neg(dest_op.clone()));
                    }
                    return;
                } else {
                    // Use magic number division (signed)
                    let (magic, shift) = match typ {
                        Type::Int => magic_number_signed(divisor, false),
                        Type::Long => magic_number_signed(divisor, true),
                        _ => panic!("Divide only supported for int or long"),
                    };
        
                    let reg = match typ {
                        Type::Int => Register::Eax,
                        Type::Long => Register::Rax,
                        _ => unreachable!(),
                    };
                    
                    // left / right = (left * magic) >> shift
                    x86_instructions.push(X86Insn::Mov(left_op.clone(), X86Operand::Reg(reg.clone()),typ.clone()));
                    x86_instructions.push(X86Insn::Mul(X86Operand::Constant(magic), X86Operand::Reg(reg.clone())));
                    x86_instructions.push(X86Insn::SarImm(shift as u32, X86Operand::Reg(reg.clone())));
                    x86_instructions.push(X86Insn::Mov(X86Operand::Reg(reg.clone()), dest_op.clone(), typ.clone()));
                    return;
                }
            }

            let right_op = map_operand(method_cfg, right, x86_instructions, globals);

            // Signed division in x86:
            match typ {
                Type::Int => {
                    x86_instructions.push(X86Insn::Push(X86Operand::Reg(Register::Rdx)));
                    // 32-bit signed division:
                    // Dividend in EAX, sign-extended into EDX using CDQ
                    x86_instructions.push(X86Insn::Mov(
                        left_op,
                        X86Operand::Reg(Register::Eax),
                        Type::Int,
                    ));
                    x86_instructions.push(X86Insn::Cdq); // Sign-extend EAX into EDX
                    x86_instructions.push(X86Insn::Div(right_op, Type::Int)); // Divide EDX:EAX by right
                    x86_instructions.push(X86Insn::Mov(
                        X86Operand::Reg(Register::Eax),
                        dest_op,
                        Type::Int,
                    ));
                    x86_instructions.push(X86Insn::Pop(X86Operand::Reg(Register::Rdx)));
                }
                Type::Long => {
                    x86_instructions.push(X86Insn::Push(X86Operand::Reg(Register::Rdx)));
                    // 64-bit signed division:
                    // Dividend in RAX, sign-extended into RDX using CQO
                    x86_instructions.push(X86Insn::Mov(
                        left_op,
                        X86Operand::Reg(Register::Rax),
                        Type::Long,
                    ));
                    x86_instructions.push(X86Insn::Cqto); // Sign-extend RAX into RDX
                    x86_instructions.push(X86Insn::Div(right_op, Type::Long)); // Divide RDX:RAX by RCX
                    x86_instructions.push(X86Insn::Mov(
                        X86Operand::Reg(Register::Rax),
                        dest_op,
                        Type::Long,
                    ));
                    x86_instructions.push(X86Insn::Pop(X86Operand::Reg(Register::Rdx)));
                }
                _ => panic!("Divide only supported for int or long types"),
            }
        }
        Instruction::Modulo {
            left,
            right,
            dest,
            typ,
        } => {
            let left_op = map_operand(method_cfg, left, x86_instructions, globals);
            let dest_op = map_operand(method_cfg, dest, x86_instructions, globals);
        
            if let Operand::Const { value, .. } = right {
                let divisor: i64 = *value;
                if divisor == 0 {
                    panic!("Modulo by zero");
                } else if (divisor as u64).is_power_of_two() {
                    // x % 2^n = x & (2^n - 1) for unsigned
                    // For signed, we can do: x - ((x >> n) << n)
                    let shift = divisor.trailing_zeros();
        
                    // temp = left >> shift
                    let tmp_reg = match typ {
                        Type::Int => Register::Eax,
                        Type::Long => Register::Rax,
                        _ => panic!("Modulo only supported for int or long"),
                    };
        
                    let mul_reg = match typ {
                        Type::Int => Register::Ecx,
                        Type::Long => Register::Rcx,
                        _ => unreachable!(),
                    };
        
                    // tmp = x >> shift
                    x86_instructions.push(X86Insn::Mov(left_op.clone(), X86Operand::Reg(tmp_reg.clone()), typ.clone()));
                    x86_instructions.push(X86Insn::SarImm(shift, X86Operand::Reg(tmp_reg.clone())));
        
                    // tmp = tmp << shift  → now tmp = x rounded down to nearest multiple of 2^shift
                    x86_instructions.push(X86Insn::Shl(X86Operand::Constant(shift as i64), X86Operand::Reg(tmp_reg.clone()), typ.clone()));
        
                    // remainder = x - tmp
                    x86_instructions.push(X86Insn::Mov(left_op.clone(), X86Operand::Reg(mul_reg.clone()), typ.clone()));
                    x86_instructions.push(X86Insn::Sub(X86Operand::Reg(tmp_reg.clone()), X86Operand::Reg(mul_reg.clone()), typ.clone()));
                    x86_instructions.push(X86Insn::Mov(X86Operand::Reg(mul_reg.clone()), dest_op.clone(), typ.clone()));
                    return;
                } else {
                    // Magic number trick for mod: a - (a / d) * d
                    let (magic, shift) = match typ {
                        Type::Int => magic_number_signed(divisor, false),
                        Type::Long => magic_number_signed(divisor, true),
                        _ => panic!("Modulo only supported for int or long"),
                    };
        
                    let reg = match typ {
                        Type::Int => Register::Eax,
                        Type::Long => Register::Rax,
                        _ => unreachable!(),
                    };
        
                    let tmp = match typ {
                        Type::Int => Register::Ecx,
                        Type::Long => Register::Rcx,
                        _ => unreachable!(),
                    };
        
                    // eax = a
                    x86_instructions.push(X86Insn::Mov(left_op.clone(), X86Operand::Reg(reg.clone()), typ.clone()));
        
                    // eax = (a * magic) >> shift
                    x86_instructions.push(X86Insn::Mul(X86Operand::Constant(magic), X86Operand::Reg(reg.clone())));
                    x86_instructions.push(X86Insn::SarImm(shift as u32, X86Operand::Reg(reg.clone())));
        
                    // tmp = eax * divisor  (quotient * d)
                    x86_instructions.push(X86Insn::Mov(X86Operand::Reg(reg.clone()), X86Operand::Reg(tmp.clone()), typ.clone()));
                    x86_instructions.push(X86Insn::Mul(X86Operand::Constant(divisor), X86Operand::Reg(tmp.clone())));
        
                    // result = a - tmp
                    x86_instructions.push(X86Insn::Mov(left_op.clone(), X86Operand::Reg(reg.clone()), typ.clone()));
                    x86_instructions.push(X86Insn::Sub(X86Operand::Reg(tmp.clone()), X86Operand::Reg(reg.clone()), typ.clone()));
        
                    x86_instructions.push(X86Insn::Mov(X86Operand::Reg(reg.clone()), dest_op.clone(), typ.clone()));
                    return;
                }
            }
        
            let right_op = map_operand(method_cfg, right, x86_instructions, globals);
        
            match typ {
                Type::Int => {
                    x86_instructions.push(X86Insn::Push(X86Operand::Reg(Register::Rdx)));
                    x86_instructions.push(X86Insn::Mov(left_op, X86Operand::Reg(Register::Eax), Type::Int));
                    x86_instructions.push(X86Insn::Cdq);
                    x86_instructions.push(X86Insn::Div(right_op, Type::Int));
                    x86_instructions.push(X86Insn::Mov(X86Operand::Reg(Register::Edx), dest_op, Type::Int));
                    x86_instructions.push(X86Insn::Pop(X86Operand::Reg(Register::Rdx)));
                }
                Type::Long => {
                    x86_instructions.push(X86Insn::Push(X86Operand::Reg(Register::Rdx)));
                    x86_instructions.push(X86Insn::Mov(left_op, X86Operand::Reg(Register::Rax), Type::Long));
                    x86_instructions.push(X86Insn::Cqto);
                    x86_instructions.push(X86Insn::Div(right_op, Type::Long));
                    x86_instructions.push(X86Insn::Mov(X86Operand::Reg(Register::Rdx), dest_op, Type::Long));
                    x86_instructions.push(X86Insn::Pop(X86Operand::Reg(Register::Rdx)));
                }
                _ => panic!("Modulo only supported for int or long types"),
            }
        }

        Instruction::Not { expr, dest } => {
            let expr_op = map_operand(method_cfg, expr, x86_instructions, globals);
            let dest_op = map_operand(method_cfg, dest, x86_instructions, globals);

            let reg = reg_for_type(Register::Rax, &Type::Bool);
            // Move expr to RAX (or any scratch reg), xor with 1
            x86_instructions.push(X86Insn::Mov(
                expr_op,
                X86Operand::Reg(reg.clone()),
                Type::Bool,
            ));
            x86_instructions.push(X86Insn::Xor(
                X86Operand::Constant(1),
                X86Operand::Reg(reg.clone()),
                Type::Bool
            ));
            x86_instructions.push(X86Insn::Mov(
                X86Operand::Reg(reg.clone()),
                dest_op,
                Type::Bool,
            ));
        }
        Instruction::Cast {
            expr,
            dest,
            target_type,
        } => {
            let dest_op: X86Operand = map_operand(method_cfg, dest, x86_instructions, globals);

            let expr_op = map_operand(method_cfg, expr, x86_instructions, globals);

            let expr_typ = expr.get_type();
            let expr_reg = reg_for_type(Register::Rax, &expr_typ);

            if let Some(expr_reg) = expr.get_reg() {
                let expr_reg_reg = expr.get_reg_reg();
                match target_type {
                    Type::Int => {
                        let sized_reg = reg_for_type(expr_reg_reg, target_type);
                        x86_instructions.push(X86Insn::Mov(
                            X86Operand::Reg(sized_reg),
                            dest_op,
                            target_type.clone(),
                        ));
                    }
                    Type::Long => {
                        if expr_typ == Type::Int {
                            // only sign extend if its an int
                            let sized_reg = reg_for_type(expr_reg_reg, &expr_typ);
                            // Movsxd must have 64-bit register as dest
                            x86_instructions.push(X86Insn::Movsxd(
                                X86Operand::Reg(sized_reg),
                                X86Operand::Reg(Register::Rax),
                            ));
                            x86_instructions.push(X86Insn::Mov(
                                X86Operand::Reg(Register::Rax),
                                dest_op,
                                Type::Long,
                            ));
                        } else {
                            // Otherwise do basic move
                            x86_instructions.push(X86Insn::Mov(
                                expr_reg,
                                dest_op.clone(),
                                expr_typ.clone(),
                            ));
                        }
                    }
                    _ => panic!("Shouldnt get here, cannot cast non int or long value"),
                }

                return;
            }

            match target_type {
                Type::Int => {
                    x86_instructions.push(X86Insn::Mov(
                        expr_op,
                        X86Operand::Reg(Register::Eax),
                        Type::Int,
                    ));
                    x86_instructions.push(X86Insn::Mov(
                        X86Operand::Reg(Register::Eax),
                        dest_op,
                        Type::Int,
                    ));
                }
                Type::Long => {
                    x86_instructions.push(X86Insn::Mov(
                        expr_op,
                        X86Operand::Reg(expr_reg.clone()),
                        expr_typ.clone(),
                    ));
                    if expr_typ == Type::Int {
                        // only sign extend if its an int
                        // TODO: potentially redundant move if dest is already a reg
                        x86_instructions.push(X86Insn::Movsxd(
                            X86Operand::Reg(expr_reg.clone()),
                            X86Operand::Reg(Register::Rax),
                        ));
                        x86_instructions.push(X86Insn::Mov(
                            X86Operand::Reg(Register::Rax),
                            dest_op,
                            Type::Long,
                        ));
                    } else {
                        // Otherwise do basic move
                        x86_instructions.push(X86Insn::Mov(
                            X86Operand::Reg(expr_reg.clone()),
                            dest_op,
                            expr_typ.clone(),
                        ));
                    }
                }
                _ => panic!("Shouldnt get here, cannot cast non int or long value"),
            }
        }

        Instruction::Len { expr, dest, .. } => {
            // let expr_typ = expr.get_type();
            // let dest_typ = dest.get_type();

            // let expr_reg = reg_for_type(Register::Rax, &expr_typ);
            // let dest_reg = reg_for_type(Register::Rax, &dest_typ);

            let mut expr_op = map_operand(method_cfg, expr, x86_instructions, globals);
            let dest_op = map_operand(method_cfg, dest, x86_instructions, globals);

            // Case where the expr op could be a Long array, we want type to technically be int here for getting int length
            if let X86Operand::Reg(reg) = expr_op {
                expr_op = X86Operand::Reg(reg_for_type(reg, &Type::Int));
            }

            // // First move is dependent on entry sizes for the array. second will always produce an integer
            // x86_instructions.push(X86Insn::Mov(
            //     expr_op,
            //     X86Operand::Reg(expr_reg),
            //     expr_typ.clone(),
            // ));
            // x86_instructions.push(X86Insn::Mov(X86Operand::Reg(dest_reg), dest_op, dest_typ));

            // Len will always be of type int
            x86_instructions.push(X86Insn::Mov(
                expr_op,
                X86Operand::Reg(Register::Eax),
                Type::Int,
            ));
            x86_instructions.push(X86Insn::Mov(
                X86Operand::Reg(Register::Eax),
                dest_op,
                Type::Int,
            ));
        }
        Instruction::Greater { left, right, dest }
        | Instruction::Less { left, right, dest }
        | Instruction::LessEqual { left, right, dest }
        | Instruction::GreaterEqual { left, right, dest }
        | Instruction::Equal { left, right, dest }
        | Instruction::NotEqual { left, right, dest } => {
            let mut left_op = map_operand(method_cfg, left, x86_instructions, globals);
            let mut right_op = map_operand(method_cfg, right, x86_instructions, globals);
            let mut dest_op = map_operand(method_cfg, dest, x86_instructions, globals);
            let mut swapped = false;

            let left_typ = left.get_type();
            let left_reg = reg_for_type(Register::Rax, &left_typ);

            // handle illegal cmp: (mem, mem)
            if is_memory_operand(&left_op) && is_memory_operand(&right_op) {
                // move value into a register
                x86_instructions.push(X86Insn::Mov(
                    left_op.clone(),
                    X86Operand::Reg(left_reg.clone()),
                    left_typ.clone(),
                ));
                left_op = X86Operand::Reg(left_reg.clone());
            }

            // handle illegal cmp: (imm, imm),
            if is_immediate_operand(&left_op) && is_immediate_operand(&right_op) {
                x86_instructions.push(X86Insn::Mov(
                    left_op.clone(),
                    X86Operand::Reg(left_reg.clone()),
                    left_typ.clone(),
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
                X86Operand::Reg(Register::Rax), // sign-extend
            ));

            // Resize reg to receive mov from EAX
            dest_op = match &dest_op {
                X86Operand::Reg(reg) => X86Operand::Reg(reg_for_type(reg.clone(), &Type::Int)),
                _ => dest_op,
            };
            x86_instructions.push(X86Insn::Mov(
                X86Operand::Reg(Register::Eax),
                dest_op,
                Type::Int,
            ));
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
            let mut cond_op = map_operand(method_cfg, condition, x86_instructions, globals);

            // Resize cond_op to compare with Eax
            cond_op = match &cond_op {
                X86Operand::Reg(reg) => X86Operand::Reg(reg_for_type(reg.clone(), &Type::Int)),
                _ => cond_op,
            };

            // cmp condition; jump if condition is true
            x86_instructions.push(X86Insn::Mov(cond_op, X86Operand::Reg(Register::Eax), Type::Int));
            x86_instructions.push(X86Insn::Cmp(
                X86Operand::Constant(0),
                X86Operand::Reg(Register::Eax),
                Type::Bool,
            ));
            x86_instructions.push(X86Insn::Jne(label)); // jump if condition != 0
        }
        Instruction::Exit { exit_code } => {
            x86_instructions.push(X86Insn::Mov(
                X86Operand::Constant(*exit_code),
                X86Operand::Reg(Register::Rdi), // exit code goes in RDI
                Type::Long
            ));
            x86_instructions.push(X86Insn::Exit);
        }
    }
}

/// Emit x86 code corresponding to the given CFG
/// Returns a vector of x86 instructions.
fn generate_method_x86(
    method_name: &String,
    method_cfg: &mut CFG,
    globals: &BTreeMap<String, Global>,
    reg_alloc: bool,
) -> Vec<X86Insn> {
    let mut x86_instructions: Vec<X86Insn> = Vec::new();

    if method_name == "main" {
        x86_instructions.push(X86Insn::Global(method_name.to_string()));
    }

    x86_instructions.push(X86Insn::Label(method_name.to_string()));

    // === Method Prologue ===

    // Track pushed callee-saved registers
    let mut pushed_callee_saved: Vec<Register> = vec![];

    for reg in CALLEE_SAVED_REGISTERS {
        if method_cfg
            .get_reg_allocs()
            .contains(&X86Operand::Reg(reg.clone()))
        {
            x86_instructions.push(X86Insn::Push(X86Operand::Reg(reg.clone())));
            pushed_callee_saved.push(reg);
        }
    }

    // Set up new frame and push RBP
    x86_instructions.push(X86Insn::Push(X86Operand::Reg(Register::Rbp)));
    x86_instructions.push(X86Insn::Mov(
        X86Operand::Reg(Register::Rsp),
        X86Operand::Reg(Register::Rbp),
        Type::Long,
    ));

    let stack_words = pushed_callee_saved.len(); //Add one for rbp maybe!!
    let total_stack_size = method_cfg.stack_size;

    // Compute necessary padding for 16-byte alignment
    // After pushes, RSP is 8 * stack_words below the original
    let current_offset = (stack_words * 8) as i64;
    let misalignment = (current_offset + total_stack_size) % 16;

    let aligned_stack_size = if misalignment == 0 {
        total_stack_size
    } else {
        total_stack_size + (16 - misalignment)
    };

    // Allocate aligned local stack space
    if aligned_stack_size > 0 {
        x86_instructions.push(X86Insn::Sub(
            X86Operand::Constant(aligned_stack_size),
            X86Operand::Reg(Register::Rsp),
            Type::Long,
        ));
    }

    // === Main Function Array Length Setup ===
    if method_name == "main" {
        for global in globals.values() {
            if let Some(len) = global.length {
                x86_instructions.push(X86Insn::Mov(
                    X86Operand::Constant(len as i64),
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

    let blocks = method_cfg.get_blocks();
    let block_order = method_cfg.get_block_order();

    for i in 0..block_order.len() {
        let id = &block_order[i];
        let block = &blocks[id];
        x86_instructions.push(X86Insn::Label(method_name.to_string() + &id.to_string()));

        let next_id = block_order.get(i + 1);
        let block_instructions = block.get_instructions();

        for (j, insn) in block_instructions.iter().enumerate() {
            // skip unnecessary unconditional jumps
            let is_last_insn = j == block_instructions.len() - 1;
            if is_last_insn {
                if let Instruction::UJmp { id, ..} = insn {
                    if let Some(next) = next_id {
                        if id == next  { // jump to the label right after this insn
                            continue;
                        }
                    }
                }
            }

            add_instruction(method_cfg, &insn, &mut x86_instructions, globals, reg_alloc);
        }

        if *id == method_cfg.exit {
            // === Method Epilogue ===

            // Restore RSP from RBP
            x86_instructions.push(X86Insn::Mov(
                X86Operand::Reg(Register::Rbp),
                X86Operand::Reg(Register::Rsp),
                Type::Long,
            ));

            x86_instructions.push(X86Insn::Pop(X86Operand::Reg(Register::Rbp)));

            // Pop callee-saved in reverse
            for reg in pushed_callee_saved.iter().rev() {
                x86_instructions.push(X86Insn::Pop(X86Operand::Reg(reg.clone())));
            }

            x86_instructions.push(X86Insn::Ret); // return to where function was called

            continue;
        }


    }

    x86_instructions
}

/// Generate x86 assembly code from the CFG/
pub fn generate_assembly(
    file: &str,
    filename: &str,
    optimizations: BTreeSet<Optimization>,
    writer: &mut dyn std::io::Write,
    debug: bool,
) {
    // Generate the method CFGS
    let (mut method_cfgs, globals, strings) = build_cfg(file, filename, writer, debug);
    // println!("globals are {:#?}", globals);

    // if debug {
    //     html_cfgs(&method_cfgs, "no-opt.html".to_string());
    //     println!("\n========== X86 Code ==========\n");
    // }

    // Perform dataflow optimizations (includes register allocation)
    optimize_dataflow(&mut method_cfgs, &optimizations, &globals, debug);

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
            _ => panic!("Should not have had this type global"),
        };

        if global.length.is_some() {
            // allocate an extra element's worth of space to store the length of the array
            global_code.push(X86Insn::Comm(
                global.name.clone(),
                type_length * i64::from(global.length.unwrap() + 1),
                type_length,
            ));
        } else {
            global_code.push(X86Insn::Comm(global.name.clone(), type_length, type_length));
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
        let method_code = generate_method_x86(
            method_name,
            &mut method_cfg,
            &globals,
            optimizations.contains(&Optimization::Regalloc),
        );
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
