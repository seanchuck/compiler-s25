/**
Dataflow code generation optimizations.
*/
use std::collections::{HashMap, HashSet};
use crate::{
    cfg::CFG,
    utils::cli::Optimization,
};


fn constant_propagation(cfg: &mut CFG) -> bool {
    false
}

fn dead_code_elimination(cfg: &mut CFG) -> bool {
    false
}

fn common_subexpression_elimination(cfg: &mut CFG) -> bool {
    false
}


/// Perform multiple passes over the CFG to apply the given optimizations
/// Returns the optimized CFG
pub fn optimize_dataflow(method_cfgs: &mut HashMap<String, CFG>, optimizations: &HashSet<Optimization>, debug: bool
) -> HashMap<String, CFG> {
    if debug {
        println!("============= Optimizing dataflow =============");
        for opt in optimizations {
            println!("{:#?}", opt);
        }
    }

    // Run the optimizations until the CFG stops changing
    let mut fixed_point = false;

    while !fixed_point {
        fixed_point = true;

        // TODO: ordering?
        if optimizations.contains(&Optimization::Cp) {
            for (method, cfg) in method_cfgs.iter_mut() {
                if constant_propagation(cfg) {
                    fixed_point = false;
                    if debug {
                        println!("Constant propagation changed {}", method);
                    }
                }
            }
        }

        if optimizations.contains(&Optimization::Dce) {
            for (method, cfg) in method_cfgs.iter_mut() {
                if dead_code_elimination(cfg) {
                    fixed_point = false;
                    if debug {
                        println!("Dead code elimination changed {}", method);
                    }
                }
            }
        }

        if optimizations.contains(&Optimization::Cse) {
            for (method, cfg) in method_cfgs.iter_mut() {
                if common_subexpression_elimination(cfg) {
                    fixed_point = false;
                    if debug {
                        println!("Common subexpression elimination elimination changed {}", method);
                    }
                }
            }
        }
    }

    method_cfgs.clone()
}
