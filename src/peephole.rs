use std::collections::{HashMap, HashSet, VecDeque};
use crate::{cfg::CFG, state::{compute_predecessors, compute_successors}, x86::{DefUse, X86Insn, X86Operand}};

fn add_use(use_set: &mut HashSet<X86Operand>, def_set: Option<&HashSet<X86Operand>>, operand: &X86Operand) {
    // don't include uses of variables that were defined within this basic block
    if def_set.is_none() || !def_set.unwrap().contains(&operand) {
        use_set.insert(operand.clone());
    }
}

fn add_def(def_set: &mut HashSet<X86Operand>, operand: &X86Operand) {
    def_set.insert(operand.clone());
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

        // update maps
        in_map.insert(block_id, in_set.clone());
        out_map.insert(block_id, out.clone());

        // if anything changed, queue predecessors
        let old_in = in_map.get(&block_id).cloned().unwrap();
        let old_out = out_map.get(&block_id).cloned().unwrap();
        if in_set != old_in || out != old_out {
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
/// move a, b
/// move b, c
/// move c, d
/// use liveness analysis to only compress instructions when valid
fn optimize_mov_chains(method_cfg: &CFG, x86_blocks: &mut HashMap<i32, Vec<X86Insn>>, debug: bool) {
    let liveness_info = liveness_analysis(method_cfg, x86_blocks, debug);

    for (id, block) in method_cfg.get_blocks() {
        let insns = x86_blocks.get_mut(id).unwrap();
        let block_liveness = &liveness_info[id];

        let mut i = 0;
        while i < insns.len()-1 {
            let insn_liveness = &block_liveness[i+1]; // liveness after the second instruction
            if let (
                X86Insn::Mov(src1, dst1, ty1),
                X86Insn::Mov(src2, dst2, ty2),
            ) = (&insns[i], &insns[i + 1])
            {
                if dst1 == src2 && ty1 == ty2 {
                    if !insn_liveness.contains(dst1) { // operand is dead after second instruction
                        let new_insn = X86Insn::Mov(src1.clone(), dst2.clone(), ty1.clone());
                        if debug {
                            println!("replacing {}; {} with {}", &insns[i], &insns[i + 1], new_insn);
                        }

                        // replace the two instructions with one
                        insns.splice(i..i + 2, [new_insn]);
                        // continue from same index to allow longer chains
                        continue;
                    }
                }
            }
            i += 1;
        }
    }
}

/// perform peephole optimizations on the x86 basic blocks within a method
pub fn peephole(method_cfg: &CFG, x86_blocks: &mut HashMap<i32, Vec<X86Insn>>, debug: bool) {
    if debug {
        println!("\n========== Peephole Optimizations ==========\n");
    }

    optimize_mov_chains(method_cfg, x86_blocks, debug);
}