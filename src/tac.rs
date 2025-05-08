/*
Linearized IR. Includes instructions that are mostly in
the simple three-address-code (TAC) format.

Similar to the AST.rs file, but includes
instructions that are simplified, and TAC.
*/

use crate::{ast::Type, x86::{Register, X86Operand}};
use std::{collections::HashSet, fmt};

#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub enum Operand {
    GlobalVar {
        name: String,
        typ: Type,
        reg: Option<X86Operand>,
    },
    GlobalArrElement {
        name: String,
        index: Box<Operand>,
        typ: Type,
        reg: Option<X86Operand>,
    },
    String {
        id: i32,
        typ: Type,
        reg: Option<X86Operand>,
    },
    LocalVar {
        name: String,
        typ: Type,
        reg: Option<X86Operand>,
    },
    LocalArrElement {
        name: String,
        index: Box<Operand>,
        typ: Type,
        reg: Option<X86Operand>,
    },
    Const {
        value: i64,
        typ: Type,
        reg: Option<X86Operand>,
    },
    Argument {
        position: i32,
        typ: Type,
        reg: Option<X86Operand>,
    },
}



impl Operand {
    pub fn get_type(&self) -> Type {
        match self {
            Operand::GlobalVar { typ, .. }
            | Operand::GlobalArrElement { typ, .. }
            | Operand::String { typ, .. }
            | Operand::LocalVar { typ, .. }
            | Operand::LocalArrElement { typ, .. }
            | Operand::Const { typ, .. }
            | Operand::Argument { typ, .. } => typ.clone(),
        }
    }

    pub fn get_reg(&self) -> Option<X86Operand> {
        match self {
            Operand::GlobalVar { reg , ..}
            | Operand::GlobalArrElement { reg , ..}
            | Operand::String { reg , ..}
            | Operand::LocalVar { reg , ..}
            | Operand::LocalArrElement { reg , ..}
            | Operand::Const { reg , ..}
            | Operand::Argument { reg, ..} => reg.clone()
        }
    }

    pub fn get_reg_reg(&self) -> Register {
        let op = self.get_reg();
        match op {
            Some(X86Operand::Reg(reg)) => reg,
            _ => {
                unreachable!();
            }
        }
    }

    pub fn set_reg(&mut self, new_reg: &Option<X86Operand>) {
        // println!("set reg: {:#?}", new_reg);
        match self {
            Operand::GlobalVar { reg, .. }
            | Operand::GlobalArrElement { reg, .. }
            | Operand::String { reg, .. }
            | Operand::LocalVar { reg, .. }
            | Operand::LocalArrElement { reg, .. }
            | Operand::Const { reg, .. }
            | Operand::Argument { reg, .. } => {
                *reg = new_reg.clone();
            }
        }
    }

    pub fn is_const(&self) -> bool {
        match self {
            Operand::Const {..} => true,
            _=> false
        }
    }
}
    

impl fmt::Display for Operand {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Operand::Const { value, .. } => write!(f, "{}", value),
            Operand::String { id, .. } => write!(f, "str{}", id),
            Operand::GlobalVar { name, .. } => write!(f, "{}", name),
            Operand::GlobalArrElement { name, index, .. } => write!(f, "{}[{}]", name, index),
            Operand::LocalVar { name, .. } => write!(f, "{}", name),
            Operand::LocalArrElement { name, index, .. } => write!(f, "{}[{}]", name, index),
            Operand::Argument { position, .. } => write!(f, "arg{}", position),
        }
    }
}


#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub enum Instruction {
    // ARITHMETIC BINARY OPERATIONS
    // t <- X + Y
    Add {
        typ: Type,
        left: Operand,
        right: Operand,
        dest: Operand,
    },
    Subtract {
        typ: Type,
        left: Operand,
        right: Operand,
        dest: Operand,
    },
    Multiply {
        typ: Type,
        left: Operand,
        right: Operand,
        dest: Operand,
    },
    Divide {
        typ: Type,
        left: Operand,
        right: Operand,
        dest: Operand,
    },
    Modulo {
        typ: Type,
        left: Operand,
        right: Operand,
        dest: Operand,
    },
    // And and Or not needed because of short-circuiting
    // UNARY OPERATIONS
    // t <- !X

    // Neg not needed; replaced by subtraction from 0
    Not {
        expr: Operand,
        dest: Operand,
    },
    Cast {
        expr: Operand,
        dest: Operand,
        target_type: Type,
    },
    Len {
        typ: Type, // is array type int or long
        expr: Operand,
        dest: Operand,
    },

    // CONDITIONALS
    // t <- X > Y
    Greater {
        left: Operand,
        right: Operand,
        dest: Operand,
    },
    Less {
        left: Operand,
        right: Operand,
        dest: Operand,
    },
    LessEqual {
        left: Operand,
        right: Operand,
        dest: Operand,
    },
    GreaterEqual {
        left: Operand,
        right: Operand,
        dest: Operand,
    },
    Equal {
        left: Operand,
        right: Operand,
        dest: Operand,
    },
    NotEqual {
        left: Operand,
        right: Operand,
        dest: Operand,
    },

    // CONTROL FLOW
    MethodCall {
        name: String,
        args: Vec<Operand>,
        return_type: Type,
        dest: Option<Operand>,
    },
    UJmp {
        // unconditonal jump
        name: String, // Name of the method containing the Bblock
        id: i32,      // ID of basic block to jump to
    },
    CJmp {
        // conditional jump; jump if condition is TRUE
        name: String, // Name of the method containing the Bblock
        condition: Operand,
        id: i32, // ID of basic block to jump to if condition is true
    },
    Ret {
        typ: Type,
        value: Option<Operand>,
    },

    // SPECIAL
    Assign {
        typ: Type,
        src: Operand,
        dest: Operand,
    },

    // MEMORY
    LoadString {
        src: Operand,
        dest: Operand,
    },

    // Runtime Exit
    Exit {
        exit_code: i64,
    },

    LoadConst {
        src: i64,
        dest: Operand,
        typ: Type
    },
}

impl Instruction {

    // Recursively collects vars in an oeperand 
    fn collect_vars_in_operand(&self, op: &Operand) -> HashSet<String> {
        let mut vars: HashSet<String> = HashSet::new();
        match op {
            Operand::LocalVar { name, .. } | Operand::GlobalVar { name, .. }=> {vars.insert(name.clone());},

            Operand::LocalArrElement { name, index, .. }
            | Operand::GlobalArrElement { name, index, .. } => {
                vars.insert(name.clone());

                // Recurse on recursive operand idx
                vars.extend(self.collect_vars_in_operand(index));
            }

            // These don’t reference any variable
            Operand::Const{..} | Operand::String{..} | Operand::Argument{..} => {},
        }
        vars
    }

    /// Returns a HashSet of source vars involved in an instruction.
    pub fn get_used_vars(&self) -> HashSet<String> {
        match self {
            // t <- X
            Instruction::Assign { src, dest, .. } => {
                let mut vars = self.collect_vars_in_operand(src);
                if let Operand::LocalArrElement {index, ..}| Operand::GlobalArrElement {index, ..} = dest {
                    vars.extend(self.collect_vars_in_operand(index));
                }
                vars
            }

            // t <- X op Y
            Instruction::Add { left, right, .. }
            | Instruction::Subtract { left, right, .. }
            | Instruction::Multiply { left, right, .. }
            | Instruction::Divide { left, right, .. }
            | Instruction::Modulo { left, right, .. }
            | Instruction::Greater { left, right, .. }
            | Instruction::Less { left, right, .. }
            | Instruction::LessEqual { left, right, .. }
            | Instruction::GreaterEqual { left, right, .. }
            | Instruction::Equal { left, right, .. }
            | Instruction::NotEqual { left, right, .. } => {
                let mut vars = self.collect_vars_in_operand(left);
                vars.extend(self.collect_vars_in_operand(right));
                vars
            }

            // t <- !X / cast(X) / len(X)
            Instruction::Not { expr, .. }
            | Instruction::Cast { expr, .. }
            | Instruction::Len { expr, .. } => {
                self.collect_vars_in_operand(expr)
            }

            // Method call arguments
            Instruction::MethodCall { args, .. } => {
                let mut vars = HashSet::new();
                for op in args {
                    vars.extend(self.collect_vars_in_operand(op));
                }
                vars
            }

            // Conditional jump depends on condition
            Instruction::CJmp { condition, .. } => {
                self.collect_vars_in_operand(condition)
            }

            // Return value
            Instruction::Ret { value, .. } => {
                if let Some(op) = value {
                    self.collect_vars_in_operand(op)
                } else {
                    HashSet::new()
                }
            }

            // LoadString reads from a source variable
            Instruction::LoadString { src, .. } => {
                self.collect_vars_in_operand(src)
            }

            // LoadConst, UJmp, Exit don't use vars
            Instruction::LoadConst { .. }
            | Instruction::UJmp { .. }
            | Instruction::Exit { .. } => HashSet::new(),
        }
    }

    /// Get the destination that an instruction writes to
    pub fn get_def_var(&self) -> Option<String> {
        match self {
            Instruction::Assign { dest, .. }
            | Instruction::Add { dest, .. }
            | Instruction::Subtract { dest, .. }
            | Instruction::Multiply { dest, .. }
            | Instruction::Divide { dest, .. }
            | Instruction::Modulo { dest, .. }
            | Instruction::Cast { dest, .. }
            | Instruction::Not { dest, .. }
            | Instruction::Len { dest, .. }
            | Instruction::Equal { dest, .. }
            | Instruction::Less { dest, .. }
            | Instruction::Greater { dest, .. }
            | Instruction::LessEqual { dest, .. }
            | Instruction::GreaterEqual { dest, .. }
            | Instruction::NotEqual {dest, .. }
            | Instruction::LoadString {dest, .. }
            | Instruction::LoadConst {dest, .. } => {
                Some(dest.to_string())
            }

            Instruction::MethodCall { dest, .. } =>{
                if let Some(dest) = dest {
                    Some(dest.to_string())
                } else {
                    None
                }
            }
            _=> None
        }
    }

    pub fn get_operands(&self) -> Vec<&Operand> {
        let mut operands = vec![];
        match self {
            Instruction::Add { left, right, dest, .. }
            | Instruction::Subtract { left, right, dest, .. }
            | Instruction::Multiply { left, right, dest, .. }
            | Instruction::Divide { left, right, dest, .. }
            | Instruction::Greater { left, right, dest, .. }
            | Instruction::LessEqual { left, right, dest, .. }
            | Instruction::GreaterEqual { left, right, dest, .. }
            | Instruction::Equal { left, right, dest, .. }
            | Instruction::NotEqual { left, right, dest, .. }
            | Instruction::Less { left, right, dest, .. }
            | Instruction::Modulo { left, right, dest, .. } => {
                operands = vec![left, right, dest];
            }

            Instruction::Not { expr, dest }
            | Instruction::Cast { expr, dest, .. }
            | Instruction::Len { expr, dest, .. } => {
                operands = vec![expr, dest];
            }

            Instruction::MethodCall { args, dest, .. } => {
                operands = args.iter().collect();
                if let Some(dest_op) = dest {
                    operands.push(dest_op);
                }
            }
            Instruction::CJmp { condition, .. } => {
                operands = vec![condition];
            }

            Instruction::Ret { value, .. } => {
                if let Some(val_op) = value {
                    operands = vec![val_op];
                }
            }

            Instruction::Assign { src, dest, .. }
            | Instruction::LoadString { src, dest } => {
                operands = vec![src, dest];
            }

            Instruction::LoadConst { dest, .. } => {
                operands = vec![dest];
            }

            Instruction::Exit { .. }
            | Instruction::UJmp { .. } => {
                // Nothing, these instructions have no operands
            }
        }
        operands
    }

    pub fn get_reg_allocs(&self) -> Vec<X86Operand> {
        let mut reg_allocs = vec![];
        let operands = self.get_operands();
        for op in operands {
            if let Some(reg) = op.get_reg() {
                reg_allocs.push(reg);
            }
        }
        reg_allocs
    }
}

