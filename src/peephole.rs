use std::collections::{HashMap, HashSet, VecDeque};
use crate::{cfg::CFG, state::{compute_predecessors, compute_successors}, x86::{DefUse, Register, X86Insn, X86Operand}};

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
        | X86Insn::Pop(op)
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
        | X86Insn::Setne(op) => {
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

        // print!("{}:  defs: ", block_id);
        // for def in defs {
        //     print!("{} ", def);
        // }
        // print!("   uses: ");
        // for use_ in uses {
        //     print!("{} ", use_);
        // }
        // println!();

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

    // println!();

    for (block_id, _) in method_cfg.blocks.iter() {
        let mut instruction_liveness = Vec::new();
        let mut live_out: HashSet<X86Operand> = out_map[block_id].clone(); // OUT set of this basic block

        print!("{}:   ", block_id);
        for live in &live_out {
            print!("{} ", live);
        }
        println!();
        
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
/// move a, b
/// move b, c
/// move c, d
/// use liveness analysis to only compress instructions when valid
fn optimize_mov_chains(method_cfg: &CFG, x86_blocks: &mut HashMap<i32, Vec<X86Insn>>, debug: bool) {
    let liveness_info = liveness_analysis(method_cfg, x86_blocks, debug);

    for (id, block) in method_cfg.get_blocks() {
        let insns = x86_blocks.get_mut(id).unwrap();
        let block_liveness = &liveness_info[id];

        let mut i = 0; // index into insns
        let mut j = 0; // index into insn_liveness
        while i < insns.len()-1 {
            let insn_liveness = &block_liveness[j+1]; // liveness after the second instruction
            j += 1;
            i += 1;

            // only collapse consecutive mov instructions
            if let (
                X86Insn::Mov(src1, dst1, ty1),
                X86Insn::Mov(src2, dst2, ty2),
            ) = (&insns[i-1], &insns[i])
            {
                // can't move from memory to memory
                if !matches!((src1, dst2), 
                            (
                                X86Operand::Constant(_) | X86Operand::Reg(_),
                                X86Operand::Reg(_)
                            )) {
                    continue;
                }

                // make sure intermediate operand and types match
                if dst1 != src2 || ty1 != ty2 {
                    continue;
                }

                // intermediate operand must be dead after the second instruction
                let basic_intermediate = get_basic_operand(dst1);
                if basic_intermediate.is_none() || insn_liveness.contains(&basic_intermediate.unwrap()) {
                    continue;
                }

                let new_insn = X86Insn::Mov(src1.clone(), dst2.clone(), ty1.clone());
                if debug {
                    println!("replacing {}; {} with {}", &insns[i-1], &insns[i], new_insn);
                }

                // replace the two instructions with one
                insns.splice(i-1 .. i+1, [new_insn]);
                // continue from same index to allow longer chains
                i -= 1;
                continue;
            }
        }
    }
}

/// perform peephole optimizations on the x86 basic blocks within a method
pub fn peephole(method_cfg: &CFG, x86_blocks: &mut HashMap<i32, Vec<X86Insn>>, debug: bool) {
    // liveness_analysis(method_cfg, x86_blocks, debug);
    optimize_mov_chains(method_cfg, x86_blocks, debug);
}