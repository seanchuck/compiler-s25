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
use std::fmt;

#[derive(Debug, Eq, PartialEq, Hash)]
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
        size: Literal, // must be Int or HexInt
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

#[derive(Debug, Eq, PartialEq, Hash)]
pub struct Param {
    pub typ: Type,
    pub name: Box<AST>,
    pub span: Span,
}

#[derive(Debug, Eq, PartialEq, Hash)]
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

#[derive(Debug, Eq, PartialEq, Hash)]
pub enum Expr {
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
    Cast {
        target_type: Type,
        expr: Box<AST>,
        span: Span,
    },
    Len {
        id: Box<AST>, // AST::Identifier
        span: Span,
    },
    Literal {
        lit: Literal,
        span: Span,
    },
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
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

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum UnaryOp {
    Neg,
    Not,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum AssignOp {
    Assign,
    PlusAssign,
    MinusAssign,
    MultiplyAssign,
    DivideAssign,
    ModuloAssign,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum Type {
    Int,
    Long,
    Bool,
    String,
    Array(Box<Type>),
    Method(Box<Type>),
    Void,
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Debug::fmt(self, f)
    }
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
