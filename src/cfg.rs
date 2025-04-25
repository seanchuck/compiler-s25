use crate::{ast::Type, scope::{Scope, TableEntry}, tac::*};
use std::{cell::RefCell, collections::{BTreeMap, HashMap, HashSet}, rc::Rc, process::Command};
/**
Control flow graph (CFG) representation.

Consists of basic blocks and directed edges
between those basic blocks.
**/
use std::fs::File;
use std::io::Write;


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
    pub name: String,
    pub typ: Type
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
    pub scopes: BTreeMap<i32, RefCell<CFGScope>>,

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
            scopes: BTreeMap::new(),
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

    /// Get the stack offset of a temp var
    pub fn get_stack_offset(&self, temp: &String) -> i64 {
        self.locals.get(temp).unwrap().stack_offset
    }

    pub fn add_scope(&mut self, id: i32, scope: CFGScope) {
        self.scopes.insert(id, RefCell::new(scope));
    }
    
    pub fn get_scope(&self, id: i32) -> std::cell::RefMut<'_, CFGScope> {
        self.scopes
            .get(&id)
            .expect("missing scope")
            .borrow_mut()
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
                        Instruction::Add { left, right, dest, .. } => format!("{dest} <- {left} + {right}"),
                        Instruction::Assign { src, dest, .. } => format!("{dest} <- {src}"),
                        Instruction::CJmp { name, condition, id } => format!("cjmp {condition}, {name}{id}"),
                        Instruction::Cast { expr, dest, target_type } => {
                            format!("{dest} <- {target_type}({expr})")
                        }
                        Instruction::Divide {left, right, dest, .. } => format!("{dest} <- {left} / {right}"),
                        Instruction::Equal { left, right, dest } => format!("{dest} <- {left} == {right}"),
                        Instruction::Greater { left, right, dest } => format!("{dest} <- {left} > {right}"),
                        Instruction::GreaterEqual { left, right, dest } => format!("{dest} <- {left} >= {right}"),
                        Instruction::Len {expr, dest, .. } => format!("{dest} <- len({expr})"),
                        Instruction::Less { left, right, dest } => format!("{dest} <- {left} < {right}"),
                        Instruction::LessEqual { left, right, dest } => format!("{dest} <- {left} <= {right}"),
                        Instruction::MethodCall { name, args, dest, .. }=> {
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
                        Instruction::Modulo {left, right, dest , ..}=> format!("{dest} <- {left} % {right}"),
                        Instruction::Multiply {left, right, dest , ..} => format!("{dest} <- {left} * {right}"),
                        Instruction::Not { expr, dest } => format!("{dest} <- !{expr}"),
                        Instruction::NotEqual { left, right, dest } => format!("{dest} <- {left} != {right}"),
                        Instruction::Ret {value, .. }=> {
                            if let Some(v) = value {
                                format!("ret {v}")
                            } else {
                                "ret".to_string()
                            }
                        }
                        Instruction::Subtract { typ, left, right, dest }=> format!("{dest} <- {left} - {right}"),
                        Instruction::UJmp { name, id } => format!("ujmp {name}{id}"),
                        Instruction::LoadString { src, dest } => format!("{dest} <- {src:?}"),
                        Instruction::Exit { exit_code } => format!("exit({})", exit_code),
                        Instruction::LoadConst { src, dest, typ } => format!("{dest} <- {src}"),
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
    pub fn new(parent: Option<Box<CFGScope>>) -> CFGScope {
        CFGScope { parent, local_to_temp: HashMap::new() }
    }
    /// Returns this global variable, or the temp variable associated to this local variable
    pub fn lookup_var(&self, var: String, typ: Type) -> Operand {
        if let Some(temp) = self.local_to_temp.get(&var) {
            Operand::LocalVar(temp.to_string(), typ.clone())
        } else if let Some(parent) = &self.parent {
            parent.lookup_var(var ,typ.clone())
        } else {
            // assume it is in the global CFGScope
            Operand::GlobalVar(var, typ.clone())
        }
    }

    /// Returns this global array element, or the temp array element associated to this local array element
    pub fn lookup_arr(&self, arr: String, idx: Operand, sym_scope: &Rc<RefCell<Scope>>, typ: Type) -> Operand {
        if let Some(temp) = self.local_to_temp.get(&arr) {
            Operand::LocalArrElement(temp.to_string(), Box::new(idx), typ.clone())
        } else if let Some(parent) = &self.parent {
            parent.lookup_arr(arr, idx, sym_scope, typ.clone())
        } else {
            // assume it is in the global CFGScope
            let table_entry = sym_scope.borrow().lookup(&arr).expect("Array not defined in this scope");
            let TableEntry::Variable {  typ, .. } = table_entry else {
                panic!("Expected a variable, found something else!");
            };
            Operand::GlobalArrElement(arr, Box::new(idx), typ.clone())
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
    pub typ: Type
}

#[derive(Debug, Clone)]
pub struct Local {
    pub stack_offset: i64,
    pub length: Option<i64>, // if array
    pub typ: Type
}

pub struct Loop {
    pub break_to: i32,    // ID of basic block following the loop
    pub continue_to: i32, // ID of loop's header basic block
}
