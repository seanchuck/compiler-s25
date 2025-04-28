/*
Data structures for dataflow analysis.
*/
use std::collections::{HashMap, HashSet};
use crate::cfg::*;
use std::fmt;

// Map between variable and its source
pub type CopyMap = HashMap<String, String>;

pub type AvailableExpressions = HashMap<Expression, String>; // maps expression to variable

pub type LiveVariables = HashSet<String>; // names of live local variables

#[derive(Hash, PartialEq, Eq, Clone)]
pub enum Expression {
    Add(String, String),
    Subtract(String, String),
    Multiply(String, String),
    Divide(String, String),
    Modulo(String, String)
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expression::Add(left, right) => write!(f, "{} + {}", left, right),
            Expression::Subtract(left, right) => write!(f, "{} - {}", left, right),
            Expression::Multiply(left, right) => write!(f, "{} * {}", left, right),
            Expression::Divide(left, right) => write!(f, "{} / {}", left, right),
            Expression::Modulo(left, right) => write!(f, "{} % {}", left, right),
        }
    }
}

/// Compute the successors graph for a given CFG.
/// Maps each block_id to a set of successor block IDs.
pub fn compute_successors(cfg: &CFG) -> HashMap<i32, HashSet<i32>> {
    let mut successors: HashMap<i32, HashSet<i32>> = HashMap::new();

    for (&src, edges) in &cfg.edges {
        // Unreachable block
        if src == -1 {
            continue;
        }

        for edge in edges {

            successors
                .entry(src)
                .or_default()
                .insert(edge.v); // v is the destination block
        }
    }

    successors
}


/// Compute the successors graph for a given CFG.
/// Maps each block_id to a set of predecessor_id
pub fn compute_predecessors(cfg: &CFG) -> HashMap<i32, HashSet<i32>> {
    let mut preds: HashMap<i32, HashSet<i32>> = HashMap::new();
    for (src, successors) in compute_successors(cfg) {
        // Unreachable block
        if src == -1 {
            continue;
        }

        for next_block in successors {
            preds.entry(next_block).or_default().insert(src);
        }
    }
    preds
}

