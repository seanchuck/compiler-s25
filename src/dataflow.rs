/**
Dataflow code generation optimizations
*/
use std::collections::{HashMap, HashSet};
use crate::{
    cfg::CFG,
    utils::cli::Optimization,
};

/// Perform multiple passes over the CFG to apply the given optimizations
/// Returns the optimized CFG
pub fn optimize_dataflow(method_cfgs: &HashMap<String, CFG>, optimizations: HashSet<Optimization>, debug: bool
) -> &HashMap<String, CFG> {

    if debug {
        println!("============= Optimizing dataflow =============");
    
        for opt in optimizations {
            println!("{:#?}", opt);
        }
    }

    method_cfgs
}
