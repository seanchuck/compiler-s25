use crate::tac::*;
/**
Control flow graph (CFG) representation.

Consists of basic blocks and directed edges
between those basic blocks.
**/
use std::collections::{BTreeMap, HashMap, HashSet};
use std::fs::File;
use std::io::Write;

// #################################################
// CONSTANTS
// #################################################

pub const ELEMENT_SIZE: i64 = 8; // for now, allocate 8 bytes for everything no matter the type

// #################################################
// STRUCT DEFINITIONS
// #################################################

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum EdgeType {
    True,
    False,
    Unconditional,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct BlockEdge {
    pub u: i32,
    pub v: i32,
    pub truth: EdgeType,
}

#[derive(Debug, Clone)]
pub struct CFG {
    // BTreeMap is hash map that allows constant iteration order
    // entry block has ID 0
    pub name: String,                      // Name of the method for the CFG
    pub blocks: BTreeMap<i32, BasicBlock>, // Maps the ID of basic block to its representation
    // no need to store edges because basic blocks end with a jump/branch instruction

    pub edges: BTreeMap<i32, HashSet<BlockEdge>>,

    // TODO: also increase stack size for function call with ore than 6 args
    pub stack_size: i64, // total space to allocate on the stack for this method
    pub locals: BTreeMap<String, Local>,
    pub exit: i32, // index of the last basic block
}

impl CFG {
    pub fn new(name: String) -> CFG {
        CFG {
            name,
            blocks: BTreeMap::new(),
            edges: BTreeMap::new(),
            stack_size: 0,
            locals: BTreeMap::new(),
            exit: 0, // index of the last basic block
        }
    }

    /// Get all basic blocks
    pub fn get_blocks(&self) -> &BTreeMap<i32, BasicBlock> {
        &self.blocks
    }

    pub fn get_blocks_mut(&mut self) -> &mut BTreeMap<i32, BasicBlock> {
        &mut self.blocks
    }    

    /// Get basic block with ID
    fn get_block_with_id(&mut self, id: i32) -> &mut BasicBlock {
        let block = self.blocks.get_mut(&id);
        if block.is_none() {
            panic!("block with id {id} does not exist");
        }
        block.unwrap()
    }

    /// Add a basic block with a given ID
    pub fn add_block(&mut self, block: &BasicBlock) {
        self.blocks.insert(block.get_id(), block.clone());
    }

    // Adds an edge to the CFG
    pub fn add_edge(&mut self, u: i32, v: i32, edge_type: EdgeType) {
        let edge = BlockEdge {
            u,
            v,
            truth: edge_type,
        };
        self.edges.entry(u).or_default().insert(edge);
    }    

    /// Add instruction to block with ID
    pub fn add_instruction_to_block(&mut self, id: i32, instruction: Instruction) {
        if id == -1 {
            return; // do nothing, because this is an unreachable block
        } else {
            let block = self.get_block_with_id(id);
            block.add_instruction(instruction);
        }
    }

    /// Allocate space on the stack for a new temp var
    pub fn add_temp_var(&mut self, temp: String, length: Option<i64>) {
        let size: i64;
        if length.is_some() {
            // add one to store the array length
            size = (length.unwrap() + 1) * ELEMENT_SIZE;
        } else {
            size = ELEMENT_SIZE;
        }

        self.stack_size += size;
        self.locals.insert(
            temp,
            Local {
                stack_offset: -self.stack_size,
                length,
            },
        );
    }

    /// Get the stack offset of a temp var
    pub fn get_stack_offset(&self, temp: &String) -> i64 {
        self.locals.get(temp).unwrap().stack_offset
    }

    // Used to get nice visualization of CFG
    pub fn to_dot(&self) -> String {
        let mut dot = String::new();
        dot.push_str("digraph CFG {\n");
        dot.push_str("  node [shape=box fontname=\"monospace\"];\n");

        for (id, block) in &self.blocks {
            let label = block
                .get_instructions()
                .iter()
                .map(|insn| {
                    let s = match insn {
                        Instruction::Add { left, right, dest } => format!("{dest} <- {left} + {right}"),
                        Instruction::Assign { src, dest } => format!("{dest} <- {src}"),
                        Instruction::CJmp { name, condition, id } => format!("cjmp {condition}, {name}{id}"),
                        Instruction::Cast { expr, dest, target_type } => {
                            format!("{dest} <- {target_type}({expr})")
                        }
                        Instruction::Divide { left, right, dest } => format!("{dest} <- {left} / {right}"),
                        Instruction::Equal { left, right, dest } => format!("{dest} <- {left} == {right}"),
                        Instruction::Greater { left, right, dest } => format!("{dest} <- {left} > {right}"),
                        Instruction::GreaterEqual { left, right, dest } => format!("{dest} <- {left} >= {right}"),
                        Instruction::Len { expr, dest } => format!("{dest} <- len({expr})"),
                        Instruction::Less { left, right, dest } => format!("{dest} <- {left} < {right}"),
                        Instruction::LessEqual { left, right, dest } => format!("{dest} <- {left} <= {right}"),
                        Instruction::MethodCall { name, args, dest } => {
                            let args_str = args
                                .iter()
                                .map(|op| op.to_string())
                                .collect::<Vec<_>>()
                                .join(", ");
                            if let Some(d) = dest {
                                format!("{d} <- {name}({args_str})")
                            } else {
                                format!("{name}({args_str})")
                            }
                        }
                        Instruction::Modulo { left, right, dest } => format!("{dest} <- {left} % {right}"),
                        Instruction::Multiply { left, right, dest } => format!("{dest} <- {left} * {right}"),
                        Instruction::Not { expr, dest } => format!("{dest} <- !{expr}"),
                        Instruction::NotEqual { left, right, dest } => format!("{dest} <- {left} != {right}"),
                        Instruction::Ret { value } => {
                            if let Some(v) = value {
                                format!("ret {v}")
                            } else {
                                "ret".to_string()
                            }
                        }
                        Instruction::Subtract { left, right, dest } => format!("{dest} <- {left} - {right}"),
                        Instruction::UJmp { name, id } => format!("ujmp {name}{id}"),
                        Instruction::LoadString { src, dest } => format!("{dest} <- {src:?}"),
                        Instruction::Exit { exit_code } => format!("exit({})", exit_code),
                        Instruction::LoadConst { src, dest } => format!("{dest} <- {src}"),
                    };
                    s.replace('\\', "\\\\").replace('"', "\\\"") // escape for DOT
                })                           
                .collect::<Vec<_>>()
                .join("\\l"); // Graphviz line break
            dot.push_str(&format!("  {} [label=\"Block {}\\l{}\\l\"];\n", id, id, label));
        }

        for (_from, edges) in &self.edges {
            for edge in edges {
                let label = match edge.truth {
                    EdgeType::True => "True",
                    EdgeType::False => "False",
                    EdgeType::Unconditional => "",
                };
                dot.push_str(&format!(
                    "  {} -> {} [label=\"{}\"];\n",
                    edge.u, edge.v, label
                ));
            }
        }

        dot.push_str("}\n");
        dot
    }

    /// Dump dot to file and optionally render PNG
    pub fn export_dot(&self, path: &str, render_png: bool) {
        let dot_code = self.to_dot();
        let mut file = File::create(path).expect("Could not create .dot file");
        file.write_all(dot_code.as_bytes())
            .expect("Failed to write DOT");

        println!("DOT file written to: {}", path);

        if render_png {
            let output = std::process::Command::new("dot")
                .arg("-Tpng")
                .arg(path)
                .arg("-o")
                .arg("cfg.png")
                .output()
                .expect("Failed to run Graphviz 'dot' command");
            if output.status.success() {
                println!("Rendered image: cfg.png");
            } else {
                eprintln!("⚠️ Failed to render PNG. Do you have Graphviz installed?");
                eprintln!("{}", String::from_utf8_lossy(&output.stderr));
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct CFGScope {
    pub parent: Option<Box<CFGScope>>,
    pub local_to_temp: HashMap<String, String>, // maps local variable to temp variable
}

impl CFGScope {
    /// Returns this global variable, or the temp variable associated to this local variable
    pub fn lookup_var(&self, var: String) -> Operand {
        if let Some(temp) = self.local_to_temp.get(&var) {
            Operand::LocalVar(temp.to_string())
        } else if let Some(parent) = &self.parent {
            parent.lookup_var(var)
        } else {
            // assume it is in the global CFGScope
            Operand::GlobalVar(var)
        }
    }

    /// Returns this global array element, or the temp array element associated to this local array element
    pub fn lookup_arr(&self, arr: String, idx: Operand) -> Operand {
        if let Some(temp) = self.local_to_temp.get(&arr) {
            Operand::LocalArrElement(temp.to_string(), Box::new(idx))
        } else if let Some(parent) = &self.parent {
            parent.lookup_arr(arr, idx)
        } else {
            // assume it is in the global CFGScope
            Operand::GlobalArrElement(arr, Box::new(idx))
        }
    }

    /// Add a new local variable to the CFGScope
    pub fn add_local(&mut self, local: String, temp: String) {
        self.local_to_temp.insert(local, temp);
    }
}

#[derive(Debug, Clone)]
pub struct BasicBlock {
    // Basic block is just a vector of instructions
    pub instructions: Vec<Instruction>,
    id: i32, // TODO: can add meaningful labels to each BB instead of referring to them by ID
}

impl BasicBlock {
    pub fn new(id: i32) -> BasicBlock {
        BasicBlock {
            instructions: Vec::new(),
            id,
        }
    }

    pub fn get_id(&self) -> i32 {
        self.id
    }

    pub fn get_instructions(&self) -> &Vec<Instruction> {
        &self.instructions
    }

    pub fn get_instructions_mut(&mut self) -> &mut Vec<Instruction> {
        &mut self.instructions
    }
    
    fn add_instruction(&mut self, instruction: Instruction) {
        self.instructions.push(instruction);
    }
}

pub struct Global {
    pub name: String,
    pub length: Option<i32>, // if array
}

#[derive(Debug, Clone)]
pub struct Local {
    stack_offset: i64,
    length: Option<i64>, // if array
}

pub struct Loop {
    pub break_to: i32,    // ID of basic block following the loop
    pub continue_to: i32, // ID of loop's header basic block
}
