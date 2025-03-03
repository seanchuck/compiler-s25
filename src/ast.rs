/*
Abstract syntax tree structures for parser, which
mimic tree-like structure of the Decaf grammar.

Some are reused from token.rs.

BUG NOTE: Many places use Box<AST> instead of the
more specific type (i.e., Box<Expr) for ease-of-use.
This may lead to BUGS(!) so be sure to check the validity.
    - Idiomatic Rust also probably has structs instead
    of packing stuff in enums, but whatever.
*/

use crate::token::{Literal, Span};

#[allow(dead_code)]
#[derive(Debug)]
pub enum AST {
    Program {
        imports: Vec<Box<AST>>,
        fields: Vec<Box<AST>>,
        methods: Vec<Box<AST>>,
        span: Span,
    },
    ImportDecl {
        id: String,
        span: Span,
    },
    FieldDecl {
        typ: Type,
        decls: Vec<Box<AST>>,
        span: Span,
    },
    ArrayFieldDecl {
        id: String,
        size: String,
        span: Span,
    },
    MethodDecl {
        return_type: Type,
        name: String,
        params: Vec<Param>,
        block: Box<AST>,
        span: Span,
    },
    Block {
        field_decls: Vec<Box<AST>>,
        statements: Vec<Box<AST>>,
        span: Span,
    },
    Identifier {
        id: String,
        span: Span,
    },
    Type {
        typ: Type,
        span: Span,
    },
    Statement(Statement),
    Expr(Expr),
    Empty,
}

#[allow(dead_code)]
#[derive(Debug)]
pub struct Param {
    pub typ: Type,
    pub name: Box<AST>,
    pub span: Span,
}

#[allow(dead_code)]
#[derive(Debug)]
pub enum Statement {
    Assignment {
        location: Box<AST>,
        expr: Box<AST>,
        op: AssignOp,
        span: Span,
    },
    MethodCall {
        method_name: String,
        args: Vec<Box<AST>>,
        span: Span,
    },
    If {
        condition: Box<AST>,
        then_block: Box<AST>,
        else_block: Option<Box<AST>>,
        span: Span,
    },
    For {
        var: String,
        init: Box<AST>,
        condition: Box<AST>,
        update: Box<AST>,
        block: Box<AST>,
        span: Span,
    },
    Update {
        location: Box<AST>,
        span: Span,
    },
    While {
        condition: Box<AST>,
        block: Box<AST>,
        span: Span,
    },
    Return {
        expr: Option<Box<AST>>,
        span: Span,
    },
    Break {
        span: Span,
    },
    Continue {
        span: Span,
    },
}

#[allow(dead_code)]
#[derive(Debug)]
pub enum Expr {
    ArrAccess {
        id: String,
        index: Box<AST>,
        span: Span,
    },
    MethodCall {
        method_name: String,
        args: Vec<Box<AST>>,
        span: Span,
    },
    Literal {
        lit: Literal,
        span: Span,
    },
    Cast {
        target_type: Type,
        expr: Box<AST>,
        span: Span,
    },
    UnaryExpr {
        op: UnaryOp,
        expr: Box<AST>,
        span: Span,
    },
    BinaryExpr {
        op: BinaryOp,
        left: Box<AST>,
        right: Box<AST>,
        span: Span,
    },
    Len {
        id: Box<AST>,
        span: Span,
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
}

/// Implementation of Expr to enable changing the span!
impl Expr {
    pub fn set_span(&mut self, span: Span) {
        match self {
            Expr::ArrAccess { span: s, .. }
            | Expr::MethodCall { span: s, .. }
            | Expr::Literal { span: s, .. }
            | Expr::Cast { span: s, .. }
            | Expr::UnaryExpr { span: s, .. }
            | Expr::BinaryExpr { span: s, .. }
            | Expr::Len { span: s, .. } => *s = span,
        }
    }
}

// /// Implementation of the AST to allow tree visualization with GraphViz
// /// The parser will save the file to `parse_ast.dot` if --debug is passed
// /// run `dot -Tpng ast.dot -o ast.png` to generate the png file.
// impl AST {
//     pub fn to_dot(&self) -> String {
//         let mut output = String::from("digraph AST {\n");
//         let mut counter = 0;
//         fn generate_dot(node: &AST, parent: Option<usize>, counter: &mut usize, output: &mut String) -> usize {
//             let node_id = *counter;
//             *counter += 1;
//             let label = match node {
//                 AST::Program { .. } => "Program",
//                 AST::ImportDecl { id } => &format!("Import: {}", id),
//                 AST::FieldDecl { typ, .. } => &format!("FieldDecl: {:?}", typ),
//                 AST::ArrayFieldDecl { id, size } => &format!("ArrayFieldDecl: {}[{}]", id, size),
//                 AST::MethodDecl { name, .. } => &format!("Method: {}", name),
//                 AST::Block { .. } => "Block",
//                 AST::Statement(_) => "Statement",
//                 AST::Expr(_) => "Expr",
//                 AST::Identifier(name) => &format!("Identifier: {}", name),
//                 AST::Type(typ) => &format!("Type: {:?}", typ),
//             };

//             writeln!(output, "    {} [label=\"{}\"];", node_id, label).unwrap();

//             if let Some(parent_id) = parent {
//                 writeln!(output, "    {} -> {};", parent_id, node_id).unwrap();
//             }

//             match node {
//                 AST::Program { imports, fields, methods } => {
//                     for child in imports.iter().chain(fields).chain(methods) {
//                         generate_dot(child, Some(node_id), counter, output);
//                     }
//                 }
//                 AST::FieldDecl { decls, .. } => {
//                     for child in decls {
//                         generate_dot(child, Some(node_id), counter, output);
//                     }
//                 }
//                 AST::MethodDecl { block, .. } => {
//                     generate_dot(block, Some(node_id), counter, output);
//                 }
//                 AST::Block { field_decls, statements } => {
//                     for child in field_decls.iter().chain(statements) {
//                         generate_dot(child, Some(node_id), counter, output);
//                     }
//                 }
//                 _ => {}
//             }
//             node_id
//         }

//         generate_dot(self, None, &mut counter, &mut output);
//         output.push_str("}\n");
//         output
//     }
// }

// pub fn save_dot_file(ast: &AST, filename: &str) {
//     let dot_representation = ast.to_dot();
//     let mut file = File::create(filename).expect("Unable to create file");
//     file.write_all(dot_representation.as_bytes()).expect("Unable to write file");
// }
