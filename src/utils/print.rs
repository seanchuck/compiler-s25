use crate::cfg::*;
use crate::scope::*;
use crate::symtable::*;
use crate::tac::*;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

/// Pretty-print the CFG
pub fn print_cfg(method_cfgs: &HashMap<String, CFG>) {
    println!("\n==================== CFG =======================");

    for (method_name, cfg) in method_cfgs {
        println!("\n{method_name}:");

        for (id, block) in cfg.get_blocks() {
            println!("    {id}:");

            for insn in block.get_instructions() {
                match insn {
                    Instruction::Add { left, right, dest } => {
                        println!("        {dest} <- {left} + {right}");
                    }
                    Instruction::Assign { src, dest } => {
                        println!("        {dest} <- {src}");
                    }
                    Instruction::CJmp { name, condition, id } => {
                        println!("        cjmp {condition}, {name}{id}");
                    }
                    Instruction::Cast {
                        expr,
                        dest,
                        target_type,
                    } => {
                        println!("        {dest} <- {target_type}({expr})");
                    }
                    Instruction::Divide { left, right, dest } => {
                        println!("        {dest} <- {left} / {right}");
                    }
                    Instruction::Equal { left, right, dest } => {
                        println!("        {dest} <- {left} == {right}");
                    }
                    Instruction::Greater { left, right, dest } => {
                        println!("        {dest} <- {left} > {right}");
                    }
                    Instruction::GreaterEqual { left, right, dest } => {
                        println!("        {dest} <- {left} >= {right}");
                    }
                    Instruction::Len { expr, dest } => {
                        println!("        {dest} <- len({expr})");
                    }
                    Instruction::Less { left, right, dest } => {
                        println!("        {dest} <- {left} < {right}");
                    }
                    Instruction::LessEqual { left, right, dest } => {
                        println!("        {dest} <- {left} <= {right}");
                    }
                    Instruction::MethodCall { name, args, dest } => {
                        let args_string = args
                            .iter()
                            .map(|op| op.to_string())
                            .collect::<Vec<_>>()
                            .join(", ");
                        if dest.is_some() {
                            let dest_string = dest.clone().unwrap();
                            println!("        {dest_string} <- {name}({args_string})");
                        } else {
                            println!("        {name}({args_string})");
                        }
                    }
                    Instruction::Modulo { left, right, dest } => {
                        println!("        {dest} <- {left} % {right}");
                    }
                    Instruction::Multiply { left, right, dest } => {
                        println!("        {dest} <- {left} * {right}");
                    }
                    Instruction::Not { expr, dest } => {
                        println!("        {dest} <- !{expr}");
                    }
                    Instruction::NotEqual { left, right, dest } => {
                        println!("        {dest} <- {left} != {right}");
                    }
                    Instruction::Ret { value } => {
                        if value.is_some() {
                            let val_str = value.clone().unwrap();
                            println!("        ret {val_str}");
                        } else {
                            println!("        ret");
                        }
                    }
                    Instruction::Subtract { left, right, dest } => {
                        println!("        {dest} <- {left} - {right}");
                    }
                    Instruction::UJmp { name, id } => {
                        println!("        ujmp {name}{id}");
                    }
                    Instruction::LoadString { src, dest } => {
                        println!("        {dest} <- {src}");
                    }
                }
            }
        }
    }
}

// #################################################
// PRETTY PRINT SYMBOL TABLE TREE
// #################################################
/// Pretty-print an IR program
pub fn print_symtree(ir: &SymProgram) {
    println!("SymProgram {{");
    println!("  span: {:?},", ir.span);
    print_scope(&ir.global_scope, 2);

    println!("\n  methods: {{");
    for (name, method) in &ir.methods {
        println!("\n    \"{}\":", name);
        print_method(method, 6);
    }
    println!("  }}\n}}");
}

/// Pretty-print an IR method
fn print_method(method: &Rc<SymMethod>, indent: usize) {
    let indent_str = " ".repeat(indent);

    println!("{}SymMethod {{", indent_str);
    println!("{}  name: \"{}\",", indent_str, method.name);
    println!("{}  return_type: {:?},", indent_str, method.return_type);
    println!("{}  params: {:?},", indent_str, method.params);
    println!("{}  span: {:?},", indent_str, method.span);

    println!("\n{}  scope:", indent_str);
    print_scope(&method.scope, indent + 4);

    println!("\n{}  body:", indent_str);
    print_block(&method.body, indent + 4);

    println!("{}}}", indent_str);
}

/// Pretty-print an IR block
fn print_block(block: &SymBlock, indent: usize) {
    let indent_str = " ".repeat(indent);

    println!("{}SymBlock {{", indent_str);
    println!("{}  span: {:?},", indent_str, block.span);

    println!("\n{}  scope:", indent_str);
    print_scope(&block.scope, indent + 4);

    println!("\n{}  statements: [", indent_str);
    for stmt in &block.statements {
        println!("{}    {:?},", indent_str, stmt);
    }
    println!("{}  ]", indent_str);

    println!("{}}}", indent_str);
}

/// Pretty-/// Pretty-print a scope while keeping the parent ID instead of full scope details
fn print_scope(scope: &Rc<RefCell<Scope>>, indent: usize) {
    let indent_str = " ".repeat(indent);
    let scope = scope.borrow();

    let parent_id = match &scope.parent {
        Some(parent) => parent
            .borrow()
            .id
            .clone()
            .unwrap_or_else(|| "Unknown".to_string()),
        None => "None".to_string(),
    };

    println!("{}**SCOPE** {{", indent_str);

    // Print ID first
    println!("{}  id: {:?},", indent_str, scope.id);

    // Then print the parent ID
    println!("{}  parent: \"{}\",", indent_str, parent_id);

    println!("\n{}  table: {{", indent_str);
    for (_, entry) in &scope.table {
        match entry {
            TableEntry::Variable {
                name,
                typ,
                length,
                span,
            } => {
                let array_str = if length.is_some() { "[]" } else { "" };
                println!(
                    "{}    \"{}\": Variable {{ typ: {:?}{}, span: {:?} }},",
                    indent_str, name, typ, array_str, span
                );
            }
            TableEntry::Method {
                name,
                return_type,
                params,
                span,
            } => {
                println!(
                    "{}    \"{}\": Method {{ return_type: {:?}, params: {:?}, span: {:?} }},",
                    indent_str, name, return_type, params, span
                );
            }
            TableEntry::Import { name, span } => {
                println!(
                    "{}    \"{}\": Import {{ return_type: int,span: {:?} }},",
                    indent_str, name, span
                );
            }
        }
    }
    println!("{}  }}", indent_str); // Closing table

    println!("{}}}", indent_str); // Closing scope
}
