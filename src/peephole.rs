use std::collections::{HashMap, HashSet, VecDeque};
use crate::{ast::Type, cfg::CFG, state::{compute_predecessors, compute_successors}, x86::{self, DefUse, Register, X86Insn, X86Operand}};

fn add_use(use_set: &mut HashSet<X86Operand>, def_set: Option<&HashSet<X86Operand>>, operand: &X86Operand) {
    let basic_op = get_basic_operand(operand);

    // don't include uses of variables that were defined within this basic block
    if basic_op.is_some() && (def_set.is_none() || !def_set.unwrap().contains(&basic_op.as_ref().unwrap())) {
        use_set.insert(basic_op.as_ref().unwrap().clone());
    }
}

fn add_def(def_set: &mut HashSet<X86Operand>, operand: &X86Operand) {
    let basic_op = get_basic_operand(operand);
    if basic_op.is_some() {
        def_set.insert(basic_op.unwrap());
    }
}

fn get_insn_defs_uses(insn: &X86Insn, block_def_set: Option<&HashSet<X86Operand>>) -> DefUse {
    let mut def_set = HashSet::new();
    let mut use_set = HashSet::new();

    match insn {
        X86Insn::Add(op1, op2, _)
        | X86Insn::Mov(op1, op2, _)
        | X86Insn::Movsxd(op1, op2)
        | X86Insn::Movzbq(op1, op2)
        | X86Insn::Sub(op1, op2, _)
        | X86Insn::Mul(op1, op2)
        | X86Insn::Xor(op1, op2, _)
        | X86Insn::Or(op1, op2)
        | X86Insn::Shl(op1, op2)
        | X86Insn::Lea(op1, op2) => {
            add_use(&mut use_set, block_def_set, &op1);
            add_def(&mut def_set, &op2);
        }
        X86Insn::Div(op, _)
        | X86Insn::Push(op) => {
            add_use(&mut use_set, block_def_set, &op);
        }
        X86Insn::Cmp(op1, op2, _) => {
            add_use(&mut use_set, block_def_set, &op1);
            add_use(&mut use_set, block_def_set, &op2);
        }
        X86Insn::Sete(op)
        | X86Insn::Setg(op)
        | X86Insn::Setge(op)
        | X86Insn::Setl(op)
        | X86Insn::Setle(op)
        | X86Insn::Setne(op) 
        | X86Insn::Pop(op) => {
            add_def(&mut def_set, &op);
        }
        _ => {} // don't involve X86Operands
    }

    DefUse { defs: def_set, uses: use_set }
}

fn get_basic_register(register: &Register) -> Register {
    match register {
        Register::Al => Register::Rax,
        Register::Eax => Register::Rax,
        Register::Ebp => Register::Rbp,
        Register::Ebx => Register::Rbx,
        Register::Ecx => Register::Rcx,
        Register::Edi => Register::Rdi,
        Register::Edx => Register::Rdx,
        Register::Esi => Register::Rsi,
        Register::Esp => Register::Rsp,
        Register::R10 => Register::R10,
        Register::R10d => Register::R10,
        Register::R11 => Register::R11,
        Register::R11d => Register::R11,
        Register::R12 => Register::R12,
        Register::R12d => Register::R12,
        Register::R13 => Register::R13,
        Register::R13d => Register::R13,
        Register::R14 => Register::R14,
        Register::R14d => Register::R14,
        Register::R15 => Register::R15,
        Register::R15d => Register::R15,
        Register::R8 => Register::R8,
        Register::R8d => Register::R8,
        Register::R9 => Register::R9,
        Register::R9d => Register::R9,
        Register::Rax => Register::Rax,
        Register::Rbp => Register::Rbp,
        Register::Rbx => Register::Rbx,
        Register::Rcx => Register::Rcx,
        Register::Rdi => Register::Rdi,
        Register::Rdx => Register::Rdx,
        Register::Rip => Register::Rip,
        Register::Rsi => Register::Rsi,
        Register::Rsp => Register::Rsp,
    }
}

fn get_basic_operand(operand: &X86Operand) -> Option<X86Operand> {
    match operand {
        X86Operand::Reg(reg) => Some(X86Operand::Reg(get_basic_register(reg))),
        X86Operand::RegInt(reg, offset, typ) => Some(X86Operand::RegInt(get_basic_register(reg), *offset, typ.clone())),
        X86Operand::RegLabel(reg, label) => Some(X86Operand::RegLabel(get_basic_register(reg), label.clone())),
        X86Operand::Global(name) => Some(X86Operand::Global(name.clone())),
        X86Operand::Address(label, reg1, reg2, scale, typ) => Some(X86Operand::Address(label.clone(), reg1.clone(), reg2.clone(), scale.clone(), typ.clone())),
        X86Operand::Constant(_) => None
    }
}

fn get_basic_type(typ: Type) -> Type {
    match typ {
        Type::Long | Type::String => Type::Long,
        _ => Type::Int
    }
}

fn compute_def_use_sets(method_cfg: &CFG, x86_blocks: &mut HashMap<i32, Vec<X86Insn>>) -> HashMap<i32, DefUse> {
    let mut block_def_use = HashMap::new();

    for (block_id, _) in method_cfg.blocks.iter() {
        let mut def_set: HashSet<X86Operand> = HashSet::new();
        let mut use_set: HashSet<X86Operand> = HashSet::new();

        if let Some(insns) = x86_blocks.get(block_id) {
            for insn in insns.iter() {
                let DefUse { defs: insn_def_set, uses: insn_use_set } = get_insn_defs_uses(insn, Some(&def_set));
                def_set.extend(insn_def_set);
                use_set.extend(insn_use_set);
            }
        }

        block_def_use.insert(block_id.clone(), DefUse { defs: def_set, uses: use_set });
    }

    block_def_use
}

// Worklist equations for liveness analysis:
//      IN[B]  = USE[B] ∪ (OUT[B] - DEF[B])
//      OUT[B] = ∪ IN[S] for all successors S of B
// Returns a tuple (in_map, out_map)
fn compute_maps(method_cfg: &CFG, x86_blocks: &mut HashMap<i32, Vec<X86Insn>>, debug: bool) -> (HashMap<i32, HashSet<X86Operand>>, HashMap<i32, HashSet<X86Operand>>) {
    // Compute predecessor and successor graphs
    let cfg_preds = compute_predecessors(&method_cfg);
    let cfg_succs = compute_successors(&method_cfg);

    // operands that are live going in to this block; hashmap keyed by block_id
    let mut in_map: HashMap<i32, HashSet<X86Operand>> = HashMap::new();
    // operands that are live going out of this block; hashmap keyed by block_id
    let mut out_map: HashMap<i32, HashSet<X86Operand>> = HashMap::new();

    let def_use_sets = compute_def_use_sets(method_cfg, x86_blocks);

    // Worklist of basic block ids
    let mut worklist: VecDeque<i32> = method_cfg.blocks.keys().copied().collect::<VecDeque<i32>>();

    // Iterate until a fixed point
    while let Some(block_id) = worklist.pop_front() {
        let defs = &def_use_sets.get(&block_id).unwrap().defs;
        let uses = &def_use_sets.get(&block_id).unwrap().uses;

        // compute OUT
        let mut out = HashSet::new();
        
        if let Some(succs) = cfg_succs.get(&block_id) {
            for succ_id in succs {
                if let Some(in_succ) = in_map.get(succ_id) {
                    out = &out | in_succ; // union
                }
            }
        }

        // compute IN
        let mut in_set = uses.clone();
        let out_minus_def: HashSet<_> = out.difference(defs).cloned().collect();
        in_set.extend(out_minus_def);

        // if anything changed, queue predecessors
        let old_in = in_map.get(&block_id).cloned().unwrap_or_default();
        let old_out = out_map.get(&block_id).cloned().unwrap_or_default();
        if in_set != old_in || out != old_out {
            // update maps
            in_map.insert(block_id, in_set.clone());
            out_map.insert(block_id, out.clone());

            if let Some(preds) = cfg_preds.get(&block_id) {
                for pred_id in preds {
                    if !worklist.contains(pred_id) {
                        worklist.push_back(*pred_id);
                    }
                }
            }
        }
    }

    (in_map, out_map)
}


/// determine which operands are live at each program point in a CFG
/// returns a map from basic block ID to a vector of sets of operands;
/// each set corresponds to the program point AFTER an x86 instruction
fn liveness_analysis(method_cfg: &CFG, x86_blocks: &mut HashMap<i32, Vec<X86Insn>>, debug: bool) -> HashMap<i32, Vec<HashSet<X86Operand>>> {
    let (_, out_map) = compute_maps(method_cfg, x86_blocks, debug);
    let mut output_map = HashMap::new();

    for (block_id, _) in method_cfg.blocks.iter() {
        let mut instruction_liveness = Vec::new();
        let mut live_out: HashSet<X86Operand> = out_map[block_id].clone(); // OUT set of this basic block

        if let Some(insns) = x86_blocks.get(block_id) {
            // process instructions backwards
            for insn in insns.iter().rev() {
                instruction_liveness.push(live_out.clone());

                // compute the set of operands that are live going into this instruction

                // get operands defined and used by this instruction
                let DefUse { defs: insn_def_set, uses: insn_use_set } = get_insn_defs_uses(insn, None);

                // live_in = (live_out - defs) ∪ uses
                let mut live_in = live_out.clone();
                live_in.retain(|op| !insn_def_set.contains(op)); // remove operands defined by this instruction
                live_in.extend(insn_use_set); // add operands used by this instruction

                // the IN set of this instruction becomes the OUT set of the previous one
                live_out = live_in;
            }

            instruction_liveness.reverse(); // match order of instructions
            output_map.insert(*block_id, instruction_liveness);
        }
    }

   output_map
}

/// peephole optimization; eliminate chains of moves such as:
///      move a, b
///      move b, c
///      move c, d
/// use liveness analysis to only compress instructions when the intermediate operand isn't needed after
fn optimize_mov_chains(method_cfg: &CFG, x86_blocks: &mut HashMap<i32, Vec<X86Insn>>, debug: bool) {
    let liveness_info = liveness_analysis(method_cfg, x86_blocks, debug);

    for (id, _) in method_cfg.get_blocks() {
        let insns = x86_blocks.get_mut(id).unwrap();
        let block_liveness = &liveness_info[id];

        let mut i = 0; // index into insns
        let mut j = 0; // index into insn_liveness
        while i < insns.len() - 1 {
            let insn1 = &insns[i];
            let insn2 = &insns[i + 1];

            // only collapse consecutive mov instructions
            let (
                X86Insn::Mov(src1, dst1, ty1),
                X86Insn::Mov(src2, dst2, ty2),
            ) = (insn1, insn2) else {
                i += 1;
                j += 1;
                continue;
            };

            // can't move from memory to memory
            let is_mem = |op: &X86Operand| matches!(
                op,
                X86Operand::Address(..) | X86Operand::Global(..) | X86Operand::RegInt(..) | X86Operand::RegLabel(..)
            );
            if is_mem(&src1) && is_mem(&dst2) {
                i += 1;
                j += 1;
                continue;
            }

            // make sure intermediate operand and types match
            if dst1 != src2 || get_basic_type(ty1.clone()) != get_basic_type(ty2.clone()) {
                i += 1;
                j += 1;
                continue;
            }

            // intermediate operand must be dead after the second instruction
            let insn_liveness = &block_liveness[j+1]; // liveness after the second instruction
            let intermediate = match get_basic_operand(dst1) {
                Some(op) => op,
                None => {
                    i += 1;
                    j += 1;
                    continue;
                }
            };
            if insn_liveness.contains(&intermediate) {
                if debug {
                    println!(
                        "not replacing {}; {}",
                        insn1, insn2
                    );
                }
                // replace a mem-reg move with a reg-reg move if possible
                if is_mem(&src2) && !is_mem(&src1) {
                    if debug {
                        println!(
                            "avoiding memory access in {}",
                            insn2
                        );
                    }

                    // skip move x, x
                    if src1 == dst2 {
                        insns.remove(i+1);
                        j += 1;
                        continue;
                    }

                    // otherwise replace
                    let new_insn = X86Insn::Mov(src1.clone(), dst2.clone(), ty2.clone());
                    insns[i + 1] = new_insn;
                }
                i += 1;
                j += 1;
                continue;
            }

            // skip move x, x
            if src1 == dst2 {
                if debug {
                    println!("deleting redundant move: {}", src1);
                }
                insns.splice(i..=i + 1, []);
                j += 2;
                continue;
            }

            // do the replacement
            let new_insn = X86Insn::Mov(src1.clone(), dst2.clone(), ty1.clone());
            if debug {
                println!("replacing {}; {} with {}", insn1, insn2, new_insn);
            }
            insns.splice(i..=i + 1, [new_insn]);
            j += 1;
            // don't update i; continue from same index to allow longer chains
        }
    }
}

/// peephole optimization; eliminate sequences of 
///     push x
///     pop x
/// until no more changes
fn push_pop(x86_blocks: &mut HashMap<i32, Vec<X86Insn>>, debug: bool) {
    for (_, insns) in x86_blocks {
        loop {
            let mut changed = false;
            let mut i = 0;
            while i < insns.len() - 1 {
                let insn1 = &insns[i];
                let insn2 = &insns[i + 1];

                // only collapse consecutive push-pop instructions
                let (
                    X86Insn::Push(op1),
                    X86Insn::Pop(op2),
                ) = (insn1, insn2) else {
                    i += 1;
                    continue;
                };

                // check if operands match
                if op1 != op2 {
                    i += 2;
                    continue;
                }

                // delete both instructions
                if debug {
                    println!("deleting {}; {}", insn1, insn2);
                }
                insns.splice(i..=i + 1, []);
                changed = true;
                // don't change i
            }

            if !changed {
                break;
            }
        }
    }
}

/// peephole optimization; delete instructions like
///     move x, x
fn delete_self_moves(x86_blocks: &mut HashMap<i32, Vec<X86Insn>>, debug: bool) {
    for (_, insns) in x86_blocks {
        let mut i = 0;
        while i < insns.len() {
            let insn = &insns[i];

            if let X86Insn::Mov(src, dst, _) = insn {
                if src == dst {
                    if debug {
                        println!("deleting redundant move: {}", src);
                    }
                    insns.remove(i);
                    continue
                }
            }

            i += 1;
        }
    }
}

/// peephole optimization; only keep the last instruction in sequences such as
///     move a, x
///     move b, x
///     move c, x
fn consecutive_movs_to_dst(x86_blocks: &mut HashMap<i32, Vec<X86Insn>>, debug: bool) {
    for (_, insns) in x86_blocks {
        let mut i = 0;
        while i < insns.len() - 1 {
            let insn1 = &insns[i];
            let insn2 = &insns[i + 1];

            // only look for consecutive mov instructions
            let (
                X86Insn::Mov(_, dst1, _) | X86Insn::Movsxd(_, dst1) | X86Insn::Movzbq(_, dst1),
                X86Insn::Mov(_, dst2, _) | X86Insn::Movzbq(_, dst2)
            ) = (insn1, insn2) else {
                i += 1;
                continue;
            };

            // check if destinations are the same
            if get_basic_operand(dst1) != get_basic_operand(dst2) {
                i += 1;
                continue;
            }

            // delete the first instruction
            if debug {
                println!("removing the first instruction in {}; {}", insn1, insn2);
            }
            insns.remove(i);
            // keep i the same to start from the second instruction
        }
    }
}

/// peephole optimization; replace 
///     move 0, %reg
/// with
///     xor %reg, %reg
fn zero_with_xor(x86_blocks: &mut HashMap<i32, Vec<X86Insn>>, debug: bool) {
    for (_, insns) in x86_blocks {
        let mut i = 0;
        while i < insns.len() {
            let insn = &insns[i];

            // pattern match for move 0, %reg
            if let X86Insn::Mov(X86Operand::Constant(0), X86Operand::Reg(reg), typ) = insn {
                if debug {
                    println!("zeroing {} with xor", reg);
                }
                insns[i] = X86Insn::Xor(X86Operand::Reg(reg.clone()), X86Operand::Reg(reg.clone()), typ.clone());
            }

            i += 1;
        }
    }
}

/// peephole optimization; replace multiplication by power of 2 with left shift
/// division by powers of 2 should be covered by magic number division?
fn strength_reduction(x86_blocks: &mut HashMap<i32, Vec<X86Insn>>, debug: bool) {
    for (_, insns) in x86_blocks {
        let mut i = 0;
        while i < insns.len() {
            let insn = &insns[i];

            // pattern match for multiplication
            if let X86Insn::Mul(X86Operand::Constant(val), dst) = insn {
                if *val > 0 {
                    let unsigned = *val as u32;

                    if unsigned.is_power_of_two() {
                        let shift_amt = unsigned.trailing_zeros();

                        let new_insn = X86Insn::Shl(
                            X86Operand::Constant(shift_amt as i64),
                            dst.clone()
                        );

                        if debug {
                            println!(
                                "replacing {} with {} (strength reduction)",
                                insn, new_insn
                            );
                        }

                        insns[i] = new_insn;
                    }
                }
            }

            i += 1;
        }
    }
}

/// perform peephole optimizations on the x86 basic blocks within a method
pub fn peephole(method_cfg: &CFG, x86_blocks: &mut HashMap<i32, Vec<X86Insn>>, debug: bool) {
    optimize_mov_chains(method_cfg, x86_blocks, debug);
    push_pop(x86_blocks, debug);
    delete_self_moves(x86_blocks, debug);
    consecutive_movs_to_dst(x86_blocks, debug);
    zero_with_xor(x86_blocks, debug);
    strength_reduction(x86_blocks, debug);
}