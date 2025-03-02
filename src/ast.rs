/*
Abstract syntax tree structures for parser, which
mimic tree-like structure of the Decaf grammar.

Some are reused from token.rs.
*/

// Reuse Literal enum from token.rs
use crate::token::{Literal, Span};


#[derive(Debug, Clone)]
pub enum AST {
    Program(Program),
    ImportDecl(ImportDecl),
    FieldDecl(FieldDecl),
    ArrayFieldDecl(ArrayFieldDecl),
    MethodDecl(MethodDecl),
    Block(Block),
    Statement(Statement),
    Expr(Expr),
    Identifier(Identifier),
    Type(Type)
}

#[derive(Debug, Clone)]
pub struct Program {
    pub imports: Vec<Box<AST>>,
    pub fields: Vec<Box<AST>>,
    pub methods: Vec<Box<AST>>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct ImportDecl {
    pub id: String,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct FieldDecl {
    pub typ: Type,
    pub decls: Vec<Box<AST>>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct ArrayFieldDecl {
    pub id: String,
    pub size: String,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct MethodDecl {
    pub return_type: Type,
    pub name: String,
    pub params: Vec<(Type, String, Span)>,
    pub block: Box<AST>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Block {
    pub field_decls: Vec<Box<AST>>,
    pub statements: Vec<Box<AST>>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum Statement {
    Assignment(Assignment),
    MethodCall(MethodCall),
    If(If),
    For(For),
    Update(Update),
    While(While),
    Return(Return),
    Break,
    Continue,
}


#[derive(Debug, Clone)]
pub struct Assignment {
    pub location: Box<AST>,
    pub expr: Box<AST>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct MethodCall {
    pub method_name: String,
    pub args: Vec<Box<AST>>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct If {
    pub condition: Box<AST>,
    pub then_block: Box<AST>,
    pub else_block: Option<Box<AST>>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct For {
    pub var: String,
    pub init: Box<AST>,
    pub condition: Box<AST>,
    pub update: Box<AST>,
    pub block: Box<AST>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Update {
    location: Box<AST>,
    pub span: Span
}

#[derive(Debug, Clone)]
pub struct While {
    pub condition: Box<AST>,
    pub block: Box<AST>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Return {
    pub expr: Option<Box<AST>>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Identifier {
    pub id: String,
    pub span: Span
}

/// EXPRESSION ENUM
#[derive(Debug, Clone)]
pub enum Expr {
    ArrAccess(ArrAccess),
    MethodCall(MethodCallExpr),
    Literal(LiteralExpr),
    Cast(CastExpr),
    UnaryExpr(UnaryExpr),
    BinaryExpr(BinaryExpr),
    Len(LenExpr),
}

#[derive(Debug, Clone)]
pub struct ArrAccess {
    pub id: String,
    pub index: Box<AST>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct MethodCallExpr {
    pub method_name: String,
    pub args: Vec<Box<AST>>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct LiteralExpr {
    pub value: Literal,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct CastExpr {
    pub target_type: Type,
    pub expr: Box<AST>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct UnaryExpr {
    pub expr: Box<AST>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct BinaryExpr {
    pub left: Box<AST>,
    pub right: Box<AST>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct LenExpr {
    pub id: String,
    pub span: Span,
}

/// BINARYOP ENUM
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

/// UNARYOP ENUM
#[derive(Debug, Clone)]
pub enum UnaryOp {
    Neg,
    Not,
}

/// ASSIGNOP ENUM
#[derive(Debug, Clone)]
pub enum AssignOp {
    Assign,
    PlusAssign,
    MinusAssign,
    MultiplyAssign,
    DivideAssign,
    ModuloAssign,
}

/// TYPE ENUM
#[derive(Debug, Clone)]
pub enum Type {
    Int,
    Long,
    Bool,
    Void,
}


/// merge the spans of multiple tokens to cover both of them.
pub fn merge_spans(left: &Option<Span>, right: &Option<Span>) -> Option<Span> {
    match (left, right) {
        (Some(s1), Some(s2)) => Some(Span {
            sline: s1.sline,
            scol: s1.scol,
            eline: s2.eline,
            ecol: s2.ecol,
        }),
        (Some(s1), None) => Some(s1.clone()),
        (None, Some(s2)) => Some(s2.clone()),
        _ => None,
    }
}