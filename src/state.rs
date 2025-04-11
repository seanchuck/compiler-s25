/*
Data structures for dataflow analysis.
*/
use std::collections::{HashMap, HashSet};
use crate::cfg::*;
use crate::tac::*;

pub type CopyMap = HashMap<String, String>;

/// Compute the successors graph for a given CFG.
/// Maps each block_id to a set of successor_id
pub fn compute_successors(cfg: &CFG) -> HashMap<i32, HashSet<i32>> {
    let mut successors: HashMap<i32, HashSet<i32>> = HashMap::new();

    // Get block IDs in order
    let block_ids: Vec<i32> = cfg.blocks.keys().cloned().collect();

    for (i, id) in block_ids.iter().enumerate() {
        let block = cfg.blocks.get(id).unwrap();
        let instrs = &block.instructions;
        let mut targets = HashSet::new();

        if instrs.len() >= 2 {
            // Typical conditional jump pattern: CJmp followed by UJmp
            if let Instruction::CJmp { id: true_target, .. } = &instrs[instrs.len() - 2] {
                targets.insert(*true_target);
            }
            if let Instruction::UJmp { id: false_target, .. } = &instrs[instrs.len() - 1] {
                targets.insert(*false_target);
            }
        } else if let Some(Instruction::UJmp { id: target, .. }) = instrs.last() {
            targets.insert(*target);
        } else if let Some(Instruction::Ret { .. }) | Some(Instruction::Exit { .. }) = instrs.last() {
            // Terminal instruction — no successors
        } else {
            // No jump = fallthrough to next block (if it exists)
            if i + 1 < block_ids.len() {
                targets.insert(block_ids[i + 1]);
            }
        }

        successors.insert(*id, targets);
    }

    successors
}


/// Compute the successors graph for a given CFG.
/// Maps each block_id to a set of predecessor_id
pub fn compute_predecessors(cfg: &CFG) -> HashMap<i32, HashSet<i32>> {
    let mut preds: HashMap<i32, HashSet<i32>> = HashMap::new();
    for (src, successors) in compute_successors(cfg) {
        for next_block in successors {
            preds.entry(next_block).or_default().insert(src);
        }
    }
    preds
}

