use crate::ast::*;
use crate::symtable::*;
use crate::scope::*;

use std::cell::RefCell;
use std::fmt::Write;
use std::fs::File;
use std::io::Write as ioWrite;
use std::rc::Rc;

/// Implementation of the AST to allow tree visualization with GraphViz
/// The parser will save the file to `parse_ast.dot` if --debug is passed
/// run `dot -Tpng ast.dot -o ast.png` to generate the png file.
impl AST {
    pub fn to_dot(&self) -> String {
        let mut output = String::from("digraph AST {\n");
        let mut counter = 0;
        fn generate_dot(
            node: &AST,
            parent: Option<usize>,
            counter: &mut usize,
            output: &mut String,
        ) -> usize {
            let node_id = *counter;
            *counter += 1;
            let label = match node {
                AST::Program { .. } => "Program",
                AST::ImportDecl { id } => &format!("Import: {}", id),
                AST::FieldDecl { typ, .. } => &format!("FieldDecl: {:?}", typ),
                AST::ArrayFieldDecl { id, size } => &format!("ArrayFieldDecl: {}[{}]", id, size),
                AST::MethodDecl { name, .. } => &format!("Method: {}", name),
                AST::Block { .. } => "Block",
                AST::Statement(_) => "Statement",
                AST::Expr(_) => "Expr",
                AST::Identifier(name) => &format!("Identifier: {}", name),
                AST::Type(typ) => &format!("Type: {:?}", typ),
            };

            writeln!(output, "    {} [label=\"{}\"];", node_id, label).unwrap();

            if let Some(parent_id) = parent {
                writeln!(output, "    {} -> {};", parent_id, node_id).unwrap();
            }

            match node {
                AST::Program {
                    imports,
                    fields,
                    methods,
                } => {
                    for child in imports.iter().chain(fields).chain(methods) {
                        generate_dot(child, Some(node_id), counter, output);
                    }
                }
                AST::FieldDecl { decls, .. } => {
                    for child in decls {
                        generate_dot(child, Some(node_id), counter, output);
                    }
                }
                AST::MethodDecl { block, .. } => {
                    generate_dot(block, Some(node_id), counter, output);
                }
                AST::Block {
                    field_decls,
                    statements,
                } => {
                    for child in field_decls.iter().chain(statements) {
                        generate_dot(child, Some(node_id), counter, output);
                    }
                }
                _ => {}
            }
            node_id
        }

        generate_dot(self, None, &mut counter, &mut output);
        output.push_str("}}\n");
        output
    }
}

pub fn save_dot_file(ast: &AST, filename: &str) {
    let dot_representation = ast.to_dot();
    let mut file = File::create(filename).expect("Unable to create file");
    file.write_all(dot_representation.as_bytes())
        .expect("Unable to write file");
}


// #################################################
// PRETTY PRINT SYMBOL TABLE TREE
// #################################################

/// Pretty-print an IR program
pub fn print_symtree(ir: &IRProgram) {
    println!("IRProgram {{");
    print_scope(&ir.global_scope, 2);
    
    println!("\n  methods: {{");
    for (name, method) in &ir.methods {
        println!("\n    \"{}\":", name);
        print_method(method, 6);
    }
    println!("  }}\n}}");
}

/// Pretty-print an IR method
fn print_method(method: &Rc<IRMethod>, indent: usize) {
    let indent_str = " ".repeat(indent);
    
    println!("{}IRMethod {{", indent_str);
    println!("{}  name: \"{}\",", indent_str, method.name);
    println!("{}  return_type: {:?},", indent_str, method.return_type);
    println!("{}  params: {:?},", indent_str, method.params);
    
    println!("\n{}  scope:", indent_str);
    print_scope(&method.scope, indent + 4);
    
    println!("\n{}  body:", indent_str);
    print_block(&method.body, indent + 4);
    
    println!("{}}}", indent_str);
}

/// Pretty-print an IR block
fn print_block(block: &IRBlock, indent: usize) {
    let indent_str = " ".repeat(indent);
    
    println!("{}IRBlock {{", indent_str);
    
    println!("\n{}  scope:", indent_str);
    print_scope(&block.scope, indent + 4);
    
    println!("\n{}  statements: [", indent_str);
    for stmt in &block.statements {
        println!("{}    {:?},", indent_str, stmt);
    }
    println!("{}  ]", indent_str);
    
    println!("{}}}", indent_str);
}

/// Pretty-print a scope while keeping the parent ID instead of full scope details
fn print_scope(scope: &Rc<RefCell<Scope>>, indent: usize) {
    let indent_str = " ".repeat(indent);
    let scope = scope.borrow();

    let parent_id = match &scope.parent {
        Some(parent) => parent.borrow().id.clone().unwrap_or_else(|| "Unknown".to_string()),
        None => "None".to_string(),
    };

    println!("{}Scope {{", indent_str);
    println!("{}  parent: \"{}\",", indent_str, parent_id);
    
    println!("\n{}  table: {{", indent_str);
    for (_, entry) in &scope.table {
        match entry {
            TableEntry::Variable { name, typ, is_array } => {
                let array_str = if *is_array { "[]" } else { "" };
                println!("{}    \"{}\": Variable {{ typ: {:?}{} }},", indent_str, name, typ, array_str);
            }
            TableEntry::Method { name, return_type, params } => {
                println!(
                    "{}    \"{}\": Method {{ return_type: {:?}, params: {:?} }},",
                    indent_str, name, return_type, params
                );
            }
        }
    }
    println!("{}  }},\n", indent_str);
    println!("{}  id: {:?},", indent_str, scope.id);
    println!("{}}}", indent_str);
}
