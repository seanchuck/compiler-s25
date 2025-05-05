/**
Control flow graph (CFG) representation.

Consists of basic blocks and directed edges
between those basic blocks.
**/

use crate::{ast::Type, scope::{Scope, TableEntry}, tac::*, x86::X86Operand};
use std::{cell::RefCell, collections::{BTreeMap, BTreeSet, HashMap, HashSet}, rc::Rc};

// #################################################
// CONSTANTS
// #################################################

pub const INT_SIZE: i64 = 4; // 32 bits
pub const LONG_SIZE: i64 = 8; // 64 bits

// #################################################
// STRUCT DEFINITIONS
// #################################################

#[derive(Debug, Clone)]
pub struct Temp {
    pub name: String
}

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

    // TODO: 
    pub stack_size: i64, // total space to allocate on the stack for this method
    pub locals: BTreeMap<String, Local>, // maps local variable names to their metadata
    pub param_to_temp: BTreeMap<i32, Temp>, // maps param number to local temp var
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
            param_to_temp: BTreeMap::new(),
            exit: 0, // index of the last basic block
        }
    }

    /// Get all basic blocks
    pub fn get_blocks(&self) -> &BTreeMap<i32, BasicBlock> {
        &self.blocks
    }  

    pub fn get_mut_blocks(&mut self) -> &mut BTreeMap<i32, BasicBlock> {
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

    pub fn get_element_size(&self, typ: Type) -> i64 {
        match typ {
            Type::Int
            | Type::Bool => INT_SIZE,

            Type::Long
            | Type::String
            | _=> LONG_SIZE // default to 64-bit
        }
    }

    /// Allocate space on the stack for a new temp var
    pub fn add_temp_var(&mut self, temp: String, typ: Type, length: Option<i64>) {
        let element_size = self.get_element_size(typ.clone());
        // let element_size = LONG_SIZE;
        let size: i64;

        if length.is_some() {
            // add extra slot for arrays to store array length
            size = (length.unwrap() + 1) * element_size;
        } else {
            size = element_size;
        }

        self.stack_size += size;
        self.locals.insert(
            temp,
            Local {
                stack_offset: -self.stack_size,
                length,
                typ
            },
        );
    }

    // Gets an instruction at location block, instr_idx
    pub fn get_instruction(&mut self, block_id: i32, instruction_index: i32) -> &mut Instruction {
        let block = self.get_block_with_id(block_id);
        block.instructions.get_mut(instruction_index as usize).unwrap_or_else(|| panic!("Instruction index out of bounds"))
    }

    /// Get the stack offset of a temp var
    pub fn get_stack_offset(&self, temp: &String) -> i64 {
        self.locals.get(temp).unwrap().stack_offset
    }

    // Get the block ids that are successors to a given block
    pub fn successors(&self, block: i32) -> HashSet<i32> {
        self.edges
            .get(&block)
            .map(|edges| edges.iter().map(|edge| edge.v).collect())
            .unwrap_or_default()
    }

    // Get the block ids that are predecessors to a given block
    pub fn predecessors(&self, block: i32) -> HashSet<i32> {
        self.edges
            .iter()
            .filter_map(|(&from, edges)| {
                if edges.iter().any(|edge| edge.v == block) {
                    Some(from)
                } else {
                    None
                }
            })
            .collect()
    }

    pub fn op_to_str(&self, op: &Operand) -> String {
        if let Some(reg) = op.get_reg() {
            format!("{op}[*reg: {reg}*]")
        } else {
            format!("{op}")
        }
    }

    // Gets the set of all registers used in the cfg
    pub fn get_reg_allocs(&self) -> BTreeSet<X86Operand> {
        let mut registers: BTreeSet<X86Operand> = BTreeSet::new();
        for (_id, block) in self.get_blocks(){
            for instr in block.get_instructions() {
                let instr_regs: Vec<X86Operand> = instr.get_reg_allocs();
                registers.extend(instr_regs);
            }
        }
        registers
    }
    

    // Used to get nice visualization of CFG
    pub fn to_dot(&self) -> String {
        let mut dot = String::new();
        dot.push_str("digraph CFG {\n");
        dot.push_str("  node [shape=box fontname=\"monospace\"];\n");

        for (id, block) in &self.blocks {
            let label = block
                .get_instructions()
                .iter().enumerate()
                .map(|(idx, insn)| {
                    let s = match insn {
                        Instruction::Add { left, right, dest, .. } => 
                            format!("{} <- {} + {}", self.op_to_str(dest), self.op_to_str(left), self.op_to_str(right)),
                        Instruction::Assign { src, dest, .. } => 
                            format!("{} <- {}", self.op_to_str(dest), self.op_to_str(src)),
                        Instruction::CJmp { name, condition, id } => 
                            format!("cjmp {}, {}{}", self.op_to_str(condition), name, id),
                        Instruction::Cast { expr, dest, target_type } => 
                            format!("{} <- {}({})", self.op_to_str(dest), target_type, self.op_to_str(expr)),
                        Instruction::Divide { left, right, dest, .. } => 
                            format!("{} <- {} / {}", self.op_to_str(dest), self.op_to_str(left), self.op_to_str(right)),
                        Instruction::Equal { left, right, dest } => 
                            format!("{} <- {} == {}", self.op_to_str(dest), self.op_to_str(left), self.op_to_str(right)),
                        Instruction::Greater { left, right, dest } => 
                            format!("{} <- {} > {}", self.op_to_str(dest), self.op_to_str(left), self.op_to_str(right)),
                        Instruction::GreaterEqual { left, right, dest } => 
                            format!("{} <- {} >= {}", self.op_to_str(dest), self.op_to_str(left), self.op_to_str(right)),
                        Instruction::Len { expr, dest, .. } => 
                            format!("{} <- len({})", self.op_to_str(dest), self.op_to_str(expr)),
                        Instruction::Less { left, right, dest } => 
                            format!("{} <- {} < {}", self.op_to_str(dest), self.op_to_str(left), self.op_to_str(right)),
                        Instruction::LessEqual { left, right, dest } => 
                            format!("{} <- {} <= {}", self.op_to_str(dest), self.op_to_str(left), self.op_to_str(right)),
                        Instruction::MethodCall { name, args, dest, .. } => {
                            let args_str = args.iter().map(|op| self.op_to_str(op)).collect::<Vec<_>>().join(", ");
                            if let Some(d) = dest {
                                format!("{} <- {}({})", self.op_to_str(d), name, args_str)
                            } else {
                                format!("{}({})", name, args_str)
                            }
                        },
                        Instruction::Modulo { left, right, dest, .. } => 
                            format!("{} <- {} % {}", self.op_to_str(dest), self.op_to_str(left), self.op_to_str(right)),
                        Instruction::Multiply { left, right, dest, .. } => 
                            format!("{} <- {} * {}", self.op_to_str(dest), self.op_to_str(left), self.op_to_str(right)),
                        Instruction::Not { expr, dest } => 
                            format!("{} <- !{}", self.op_to_str(dest), self.op_to_str(expr)),
                        Instruction::NotEqual { left, right, dest } => 
                            format!("{} <- {} != {}", self.op_to_str(dest), self.op_to_str(left), self.op_to_str(right)),
                        Instruction::Ret { value, .. } => {
                            if let Some(v) = value {
                                format!("ret {}", self.op_to_str(v))
                            } else {
                                "ret".to_string()
                            }
                        },
                        Instruction::Subtract { typ: _, left, right, dest } => 
                            format!("{} <- {} - {}", self.op_to_str(dest), self.op_to_str(left), self.op_to_str(right)),
                        Instruction::UJmp { name, id } => 
                            format!("ujmp {}{}", name, id),
                        Instruction::LoadString { src, dest } => 
                            format!("{} <- {:?}", self.op_to_str(dest), src),
                        Instruction::Exit { exit_code } => 
                            format!("exit({})", exit_code),
                        Instruction::LoadConst { src, dest, typ: _ } => 
                            format!("{} <- {}", self.op_to_str(dest), src),
                    };
                    
                    let numbered = format!("I{}: {}", idx, s); // 1-based index
                    numbered.replace('\\', "\\\\").replace('"', "\\\"") // escape for DOT
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

    /// Generate SVG (or PNG) directly from the DOT representation
    pub fn render_dot(&self, output_format: &str) -> Vec<u8> {
        let dot_code = self.to_dot();

        let mut child = std::process::Command::new("dot")
            .arg(format!("-T{}", output_format)) // example: -Tsvg or -Tpng
            .stdin(std::process::Stdio::piped())
            .stdout(std::process::Stdio::piped())
            .spawn()
            .expect("Failed to spawn dot command");

        {
            let stdin = child.stdin.as_mut().expect("Failed to open stdin");
            use std::io::Write;
            stdin
                .write_all(dot_code.as_bytes())
                .expect("Failed to write DOT code to dot process");
        }

        let output = child
            .wait_with_output()
            .expect("Failed to read dot output");

        if output.status.success() {
            output.stdout
        } else {
            panic!(
                "Graphviz 'dot' command failed:\n{}",
                String::from_utf8_lossy(&output.stderr)
            );
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
    pub fn lookup_var(&self, var: String, typ: Type, register: Option<X86Operand>) -> Operand {
        if let Some(temp) = self.local_to_temp.get(&var) {
            Operand::LocalVar { name: temp.to_string(), typ: typ.clone(), reg: register }
        } else if let Some(parent) = &self.parent {
            parent.lookup_var(var ,typ.clone(), register)
        } else {
            // assume it is in the global CFGScope
            //Operand::GlobalVar(var, typ.clone())
            Operand::GlobalVar { name: var, typ: typ.clone(), reg: register }
        }
    }

    /// Returns this global array element, or the temp array element associated to this local array element
    pub fn lookup_arr(&self, arr: String, idx: Operand, sym_scope: &Rc<RefCell<Scope>>, typ: Type) -> Operand {
        if let Some(temp) = self.local_to_temp.get(&arr) {
            Operand::LocalArrElement { name: temp.to_string(), index: Box::new(idx), typ: typ.clone(), reg: None } // TODO: right now no array elements in registers
        } else if let Some(parent) = &self.parent {
            parent.lookup_arr(arr, idx, sym_scope, typ.clone())
        } else {
            // assume it is in the global CFGScope
            let table_entry = sym_scope.borrow().lookup(&arr).expect("Array not defined in this scope");
            let TableEntry::Variable {  typ, .. } = table_entry else {
                panic!("Expected a variable, found something else!");
            };
            // Operand::GlobalArrElement(arr, Box::new(idx), typ.clone())
            Operand::GlobalArrElement { name: arr, index: Box::new(idx), typ: typ.clone(), reg: None }  // TODO: right now no array elements in registers
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

    pub fn get_mut_instructions(&mut self) -> &mut Vec<Instruction> {
        &mut self.instructions
    }
    
    fn add_instruction(&mut self, instruction: Instruction) {
        self.instructions.push(instruction);
    }
}

#[derive(Debug, Clone)]
pub struct Global {
    pub name: String,
    pub length: Option<i32>, // if array
    pub typ: Type
}

#[derive(Debug, Clone)]
pub struct Local {
    pub stack_offset: i64,
    pub length: Option<i64>, // if array
    pub typ: Type
}
#[derive(Debug, Clone)]
pub struct Loop {
    pub break_to: i32,    // ID of basic block following the loop
    pub continue_to: i32, // ID of loop's header basic block
}
