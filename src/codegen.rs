use crate::cfg::ELEMENT_SIZE;
use crate::tac::*;
use crate::utils::print::print_cfg;
use crate::x86::*;
use crate::{buildcfg::build_cfg, cfg::CFG};
/**
Generate x86 code from the Control flow graph.
**/
use std::collections::HashMap;

// TODO:
// Use the CFGS to linearize all blocks starting from main
// Convert linear blocks into x86 Assembly

// Remember to follow x86 calling convention:
//  - For now, everything on stack (no registers!)
//  - Args in %rdi, ...
//  - Save all caller saved registers
//  - call
//
//  - callee function prologue (%bp, new stack frame)
//  - save callee saved registers
//  - function body
//  - callee function epilogue
//  - ret

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
            let idx_op = map_operand(method_cfg, idx, x86_instructions);
            x86_instructions.push(X86Insn::Lea(
                X86Operand::RegInt(Register::Rbp, method_cfg.get_stack_offset(arr)),
                X86Operand::Reg(Register::Rax),
            )); // store base address of array in rax
            x86_instructions.push(X86Insn::Mov(idx_op, X86Operand::Reg(Register::R10))); // store index in r10
            X86Operand::Address(None, Some(Register::Rax), Register::R10, ELEMENT_SIZE)
        }
        Operand::GlobalArrElement(arr, idx) => {
            let idx_op = map_operand(method_cfg, idx, x86_instructions);
            println!("{:?}", idx_op);
            x86_instructions.push(X86Insn::Mov(idx_op, X86Operand::Reg(Register::R10))); // store index in r10
            X86Operand::Address(Some(arr.to_string()), None, Register::R10, ELEMENT_SIZE)
        }
        Operand::Argument(pos) => match pos {
            0 => X86Operand::Reg(Register::Rdi),
            1 => X86Operand::Reg(Register::Rsi),
            2 => X86Operand::Reg(Register::Rdx),
            3 => X86Operand::Reg(Register::Rcx),
            4 => X86Operand::Reg(Register::R8),
            5 => X86Operand::Reg(Register::R9),
            _ => todo!(),
        },
        Operand::String(idx) => X86Operand::RegLabel(Register::Rip, format!("str{idx}")),
    }
}

/// Adds the x86 instructions corresponding to insn to x86_instructions
fn add_instruction(method_cfg: &CFG, insn: &Instruction, x86_instructions: &mut Vec<X86Insn>) {
    match insn {
        Instruction::Add { left, right, dest } => {
            let left_op = map_operand(method_cfg, left, x86_instructions);
            let right_op = map_operand(method_cfg, right, x86_instructions);
            let dest_op = map_operand(method_cfg, dest, x86_instructions);

            // rax as working register
            x86_instructions.push(X86Insn::Mov(left_op, X86Operand::Reg(Register::Rax)));
            x86_instructions.push(X86Insn::Add(right_op, X86Operand::Reg(Register::Rax)));
            x86_instructions.push(X86Insn::Mov(X86Operand::Reg(Register::Rax), dest_op));
        }
        Instruction::Subtract { left, right, dest } => {
            let left_op = map_operand(method_cfg, left, x86_instructions);
            let right_op = map_operand(method_cfg, right, x86_instructions);
            let dest_op = map_operand(method_cfg, dest, x86_instructions);

            // rax as working register
            x86_instructions.push(X86Insn::Mov(left_op, X86Operand::Reg(Register::Rax)));
            x86_instructions.push(X86Insn::Sub(right_op, X86Operand::Reg(Register::Rax)));
            x86_instructions.push(X86Insn::Mov(X86Operand::Reg(Register::Rax), dest_op));
        }
        Instruction::Assign { src, dest } => {
            let dest_op = map_operand(method_cfg, dest, x86_instructions);
            let src_op = map_operand(method_cfg, src, x86_instructions);
            x86_instructions.push(X86Insn::Mov(src_op, dest_op));
        }
        Instruction::LoadString { src, dest } => {
            let dest_op = map_operand(method_cfg, dest, x86_instructions);
            let src_op = map_operand(method_cfg, src, x86_instructions);

            // rax as working register
            x86_instructions.push(X86Insn::Lea(src_op, X86Operand::Reg(Register::Rax)));
            x86_instructions.push(X86Insn::Mov(X86Operand::Reg(Register::Rax), dest_op));
        }
        Instruction::MethodCall { name, args, dest } => {
            // Determine the destination operand
            let dest_op = match dest {
                Some(d) => map_operand(method_cfg, d, x86_instructions),
                None => X86Operand::Reg(Register::Rax),
            };

            // Setup arguments
            // TODO: push arguments onto the stack or into registers as per calling convention
            for (i, arg) in args.iter().enumerate() {
                let arg_reg =
                    map_operand(method_cfg, &Operand::Argument(i as i32), x86_instructions);
                let src_reg = map_operand(method_cfg, arg, x86_instructions);
                x86_instructions.push(X86Insn::Mov(src_reg, arg_reg));
            }

            // Make the call
            x86_instructions.push(X86Insn::Call(name.to_string()));

            // Move the result into `dest` if necessary
            match dest_op {
                X86Operand::Reg(Register::Rax) => {}
                _ => x86_instructions.push(X86Insn::Mov(X86Operand::Reg(Register::Rax), dest_op)),
            }
        }
        Instruction::Ret { value } => {
            if let Some(value) = value {
                let value_reg = map_operand(method_cfg, value, x86_instructions);
                x86_instructions.push(X86Insn::Mov(value_reg, X86Operand::Reg(Register::Rax)));
            }
            x86_instructions.push(X86Insn::Ret);
        }
        Instruction::Multiply { left, right, dest } => {
            let left_op = map_operand(method_cfg, left, x86_instructions);
            let right_op = map_operand(method_cfg, right, x86_instructions);
            let dest_op = map_operand(method_cfg, dest, x86_instructions);

            // rax as working register
            x86_instructions.push(X86Insn::Mov(left_op, X86Operand::Reg(Register::Rax)));
            x86_instructions.push(X86Insn::Mul(right_op, X86Operand::Reg(Register::Rax)));
            x86_instructions.push(X86Insn::Mov(X86Operand::Reg(Register::Rax), dest_op));
        }
        Instruction::Divide { left, right, dest } => {
            let left_op = map_operand(method_cfg, left, x86_instructions);
            let right_op = map_operand(method_cfg, right, x86_instructions);
            let dest_op = map_operand(method_cfg, dest, x86_instructions);

            // Signed division in x86:
            // Dividend in RAX, sign-extended into RDX using CQO
            x86_instructions.push(X86Insn::Mov(left_op, X86Operand::Reg(Register::Rax)));
            x86_instructions.push(X86Insn::Xor(
                X86Operand::Reg(Register::Rdx),
                X86Operand::Reg(Register::Rdx),
            )); // TODO is sign extension in Rdx necessary? right now just zero it
            x86_instructions.push(X86Insn::Div(right_op)); // Signed divide RDX:RAX by right_op
            x86_instructions.push(X86Insn::Mov(X86Operand::Reg(Register::Rax), dest_op));
        }
        Instruction::Modulo { left, right, dest } => {
            let left_op: X86Operand = map_operand(method_cfg, left, x86_instructions);
            let right_op = map_operand(method_cfg, right, x86_instructions);
            let dest_op = map_operand(method_cfg, dest, x86_instructions);

            // Signed modulo using XOR to zero out %rdx
            x86_instructions.push(X86Insn::Mov(left_op, X86Operand::Reg(Register::Rax))); // RAX = left
            x86_instructions.push(X86Insn::Xor(
                X86Operand::Reg(Register::Rdx),
                X86Operand::Reg(Register::Rdx),
            )); // zero RDX
            x86_instructions.push(X86Insn::Div(right_op)); // RAX = quotient, RDX = remainder
            x86_instructions.push(X86Insn::Mov(X86Operand::Reg(Register::Rdx), dest_op));
        }
        Instruction::Not { expr, dest } => {
            let expr_op = map_operand(method_cfg, expr, x86_instructions);
            let dest_op = map_operand(method_cfg, dest, x86_instructions);

            // Move expr to RAX (or any scratch reg), xor with 1
            x86_instructions.push(X86Insn::Mov(expr_op, X86Operand::Reg(Register::Rax)));
            x86_instructions.push(X86Insn::Xor(
                X86Operand::Constant(1),
                X86Operand::Reg(Register::Rax),
            ));
            x86_instructions.push(X86Insn::Mov(X86Operand::Reg(Register::Rax), dest_op));
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
                    // TODO implement this when we differentiate ints and longs
                }
                crate::ast::Type::Long => {
                    // TODO implement this when we differentiate ints and longs
                }
                _ => panic!("Shouldnt get here, cannot cast non int or long value"),
            }
        }
        Instruction::Len { expr, dest } => {
            let expr_op = map_operand(method_cfg, expr, x86_instructions);
            let dest_op = map_operand(method_cfg, dest, x86_instructions);

            x86_instructions.push(X86Insn::Mov(expr_op, dest_op));
        }
        Instruction::Greater { left, right, dest }
        | Instruction::Less { left, right, dest }
        | Instruction::LessEqual { left, right, dest }
        | Instruction::GreaterEqual { left, right, dest }
        | Instruction::Equal { left, right, dest }
        | Instruction::NotEqual { left, right, dest } => {
            let left_op = map_operand(method_cfg, left, x86_instructions);
            let right_op = map_operand(method_cfg, right, x86_instructions);
            let dest_op = map_operand(method_cfg, dest, x86_instructions);

            x86_instructions.push(X86Insn::Cmp(right_op, left_op)); // cmp right, left

            let set_instr = match insn {
                Instruction::Greater { .. } => X86Insn::Setg(dest_op),
                Instruction::Less { .. } => X86Insn::Setl(dest_op),
                Instruction::LessEqual { .. } => X86Insn::Setle(dest_op),
                Instruction::GreaterEqual { .. } => X86Insn::Setge(dest_op),
                Instruction::Equal { .. } => X86Insn::Sete(dest_op),
                Instruction::NotEqual { .. } => X86Insn::Setne(dest_op),
                _ => unreachable!(),
            };

            x86_instructions.push(set_instr);
        }
        Instruction::UJmp { name, id } => {
            let label = format!("{}{}", name, id);
            x86_instructions.push(X86Insn::Jmp(label));
        }
        Instruction::CJmp { name, condition, id } => {
            let label = format!("{}{}", name, id);
            let cond_op = map_operand(method_cfg, condition, x86_instructions);

            // cmp condition, 0 → is condition true?
            x86_instructions.push(X86Insn::Cmp(cond_op, X86Operand::Constant(0)));
            x86_instructions.push(X86Insn::Jne(label)); // jump if condition != 0
        }
    }
}

/// Emit x86 code corresponding to the given CFG
/// Returns a vector of strings of x86 instructions.
fn generate_method_x86(method_name: &String, method_cfg: &mut CFG) -> Vec<X86Insn> {
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
    )); // copy stack pointer to base pointer
    x86_instructions.push(X86Insn::Sub(
        X86Operand::Constant(method_cfg.stack_size),
        X86Operand::Reg(Register::Rsp),
    )); // decrease stack pointer to allocate space on the stack

    for (id, block) in method_cfg.get_blocks() {
        x86_instructions.push(X86Insn::Label(method_name.to_string() + &id.to_string()));

        for insn in block.get_instructions() {
            add_instruction(method_cfg, &insn, &mut x86_instructions);
        }
    }

    // method epilogue
    x86_instructions.push(X86Insn::Mov(
        X86Operand::Reg(Register::Rbp),
        X86Operand::Reg(Register::Rsp),
    )); // move base pointer to stack pointer
    x86_instructions.push(X86Insn::Pop(X86Operand::Reg(Register::Rbp))); // pop base pointer off stack
    x86_instructions.push(X86Insn::Ret); // return to where function was called

    x86_instructions
}

/// Generate x86 assembly code from the CFG/
pub fn generate_assembly(file: &str, filename: &str, writer: &mut dyn std::io::Write, debug: bool) {
    // Generate the method CFGS
    let (method_cfgs, globals, strings) = build_cfg(file, filename, writer, debug);
    if debug {
        print_cfg(&method_cfgs);
    }

    let mut global_code: Vec<X86Insn> = Vec::new();

    // global variables
    for global in globals {
        if global.length.is_some() {
            // allocate an extra element's worth of space to store the length of the array
            // 8 byte alignment for now...
            global_code.push(X86Insn::Comm(global.name, ELEMENT_SIZE*i64::from(global.length.unwrap()+1), ELEMENT_SIZE));
        } else {
            global_code.push(X86Insn::Comm(global.name, ELEMENT_SIZE, ELEMENT_SIZE));
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
        let method_code = generate_method_x86(&method_name, &mut method_cfg);
        code.insert(method_name.clone(), method_code);
    }

    // Emit the final code
    println!("\n========== X86 Code ==========\n");

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
