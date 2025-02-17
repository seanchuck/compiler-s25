/*
Token data structures for scanner.
*/

#[derive(Debug)]
#[allow(dead_code)]
pub struct TokenInfo {
    pub token: Token,
    pub display: String,
    pub line: i32,
    pub col: i32,
}

#[derive(Debug)]
#[allow(dead_code)]
pub enum Token {
    Keyword(Keyword), // Keyword variant has type Keyword
    Identifier(String),
    Symbol(Symbol),
    Literal(Literal),
}

#[derive(Debug)]
pub enum Keyword {
    If,
    Bool,
    Break,
    Import,
    Continue,
    Else,
    For,
    While,
    Int,
    Long,
    Return,
    Len,
    Void,
    // never constructed since captured in boolean literals
    // True,
    // False,
}

#[derive(Debug)]
#[allow(dead_code)]
pub enum Symbol {
    Operator(Operator),
    Punctuation(Punctuation),
}

#[derive(Debug)]
pub enum Operator {
    Plus,
    Minus,
    Multiply,
    Divide,
    Modulo,
    Assign,
    PlusAssign,
    MinusAssign,
    MultiplyAssign,
    DivideAssign,
    ModuloAssign,
    Increment,
    Decrement,
    Equal,
    NotEqual,
    Less,
    Greater,
    LessEqual,
    GreaterEqual,
    LogicalAnd,
    LogicalOr,
    LogicalNot,
}

#[derive(Debug)]
pub enum Punctuation {
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    LeftBracket,
    RightBracket,
    Semicolon,
    Comma,
}

#[derive(Debug)]
pub enum Literal {
    Char(char),
    Int(String),
    Long(String),
    HexInt(String),
    HexLong(String),
    String(String),
    Bool(bool),
}
