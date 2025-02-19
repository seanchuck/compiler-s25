/*
Token data structures for scanner.
*/
// use nom::{InputTake};

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

#[derive(Debug, PartialEq, Eq, Clone)]
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

#[derive(Debug, PartialEq, Eq, Clone)]
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

#[derive(Debug, PartialEq, Eq)]
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
    String(String),
    Int(String),
    Long(String),
    HexInt(String),
    HexLong(String),
    Bool(bool),
}


// use nom::Input;
// use std::ops::RangeFrom;


// impl Input for &[Token] {
//     type Token = Token;

//     fn input_len(&self) -> usize {
//         self.len()
//     }

//     fn take(&self, count: usize) -> Self {
//         &self[..count]
//     }

//     fn take_split(&self, count: usize) -> (Self, Self) {
//         self.split_at(count)
//     }

// }