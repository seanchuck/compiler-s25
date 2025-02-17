/*
Abstract syntax tree structures for parser
*/

pub enum AST {
    Program {
        imports: Vec<Box<AST>>,
        fields: Vec<Box<AST>>,
        methods: Vec<Box<AST>>,
    },
    ImportDecl {
        identifier: String,
    },
    FieldDecl {
        typ: Type,
        decls: Vec<Box<AST>>,
    },
    ArrayFieldDecl {
        identifier: String,
        size: i32,
    },
    MethodDecl {
        return_type: Option<Type>,
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

pub enum Type {
    Int,
    Long,
    Bool,
    Void,
}

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

pub enum AssignOp {
    Assign,
    PlusAssign,
    MinusAssign,
    MultiplyAssign,
    DivideAssign,
    ModuloAssign,
}

pub enum Expr {
    // Location(Box<AST>),
    ArrAccess {
        identifier: String,
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
    UnaryOp {
        op: UnaryOp,
        expr: Box<AST>,
    },
    BinaryOp {
        left: Box<AST>,
        op: BinaryOp,
        right: Box<AST>,
    },
    Len {
        identifier: String,
    },
}

pub enum Literal {
    Int(i32),
    Long(i64),
    Bool(bool),
    Char(char),
    String(String),
}

pub enum UnaryOp {
    Neg,
    Not,
}

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
