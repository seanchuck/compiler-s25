/*
Abstract syntax tree structures for parser, which
mimic tree-like structure of the Decaf grammar.

Some are reused from token.rs.
*/

use crate::token::Literal;


// Todo: Un-nest, this should not be an enum.
#[allow(dead_code)]
#[derive(Debug, Clone)]
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
#[derive(Debug, Clone)]
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
#[derive(Debug, Clone)]
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
#[derive(Debug, Clone)]
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