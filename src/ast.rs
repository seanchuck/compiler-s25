/*
Abstract syntax tree structures for parser, which
mimic tree-like structure of the Decaf grammar.

Some are reused from token.rs.
*/

use crate::token::Literal;
use std::fmt::Write;
use std::fs::File;
use std::io::Write as ioWrite;


// Todo: Un-nest, this should not be an enum.
#[allow(dead_code)]
#[derive(Debug)]
pub enum AST {
    Program {
        imports: Vec<Box<AST>>,
        fields: Vec<Box<AST>>,
        methods: Vec<Box<AST>>,
    },
    ImportDecl {
        id: String,
    },
    FieldDecl {
        typ: Type,
        decls: Vec<Box<AST>>, // Identifier or ArrayFieldDecl
    },
    ArrayFieldDecl {
        id: String,
        size: String,
    },
    MethodDecl {
        return_type: Type,
        name: String,
        params: Vec<(Type, String)>,
        block: Box<AST>,
    },
    Block {
        field_decls: Vec<Box<AST>>,
        statements: Vec<Box<AST>>,
    },
    Statement(Statement),
    Expr(Expr),
    Identifier(String),
    Type(Type),
}

#[allow(dead_code)]
#[derive(Debug)]
pub enum Statement {
    Assignment {
        location: Box<AST>,
        expr: Box<AST>,
        op: AssignOp,
    },
    MethodCall {
        method_name: String,
        args: Vec<Box<AST>>,
    },
    If {
        condition: Box<AST>,
        then_block: Box<AST>,
        else_block: Option<Box<AST>>,
    },
    For {
        var: String,
        init: Box<AST>,
        condition: Box<AST>,
        update: Box<AST>,
        block: Box<AST>,
    },
    Update {
        location: Box<AST>,
    },
    While {
        condition: Box<AST>,
        block: Box<AST>,
    },
    Return {
        expr: Option<Box<AST>>,
    },
    Break,
    Continue,
}

#[allow(dead_code)]
#[derive(Debug)]
pub enum Expr {
    ArrAccess {
        id: String,
        index: Box<AST>,
    },
    MethodCall {
        method_name: String,
        args: Vec<Box<AST>>,
    },
    Literal(Literal),
    Cast {
        target_type: Type,
        expr: Box<AST>,
    },
    UnaryExpr {
        op: UnaryOp,
        expr: Box<AST>,
    },
    BinaryExpr {
        op: BinaryOp,
        left: Box<AST>,
        right: Box<AST>,
    },
    Len {
        id: String,
    },
}

#[derive(Debug, Clone)]
pub enum BinaryOp {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
    Less,
    Greater,
    LessEqual,
    GreaterEqual,
    Equal,
    NotEqual,
    And,
    Or,
}

#[derive(Debug, Clone)]
pub enum UnaryOp {
    Neg,
    Not,
}
#[derive(Debug)]
pub enum AssignOp {
    Assign,
    PlusAssign,
    MinusAssign,
    MultiplyAssign,
    DivideAssign,
    ModuloAssign,
}

#[derive(Debug, Clone)]
pub enum Type {
    Int,
    Long,
    Bool,
    Void,
    // HexInt,
    // HexLong
}

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
        output.push_str("}\n");
        output
    }
}

pub fn save_dot_file(ast: &AST, filename: &str) {
    let dot_representation = ast.to_dot();
    let mut file = File::create(filename).expect("Unable to create file");
    file.write_all(dot_representation.as_bytes())
        .expect("Unable to write file");
}
