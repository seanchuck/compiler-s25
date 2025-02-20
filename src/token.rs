/*
Token data structures for scanner.
*/
// use nom::{InputTake};

#[derive(Debug, Clone, PartialEq)]
#[allow(dead_code)]
pub struct TokenInfo {
    pub token: Token,
    pub display: String,
    pub line: i32,
    pub col: i32,
}

#[derive(Debug, Clone, PartialEq)]
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

#[derive(Debug, Clone, PartialEq)]
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

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
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

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Char(char),
    String(String),
    Int(String),
    Long(String),
    HexInt(String),
    HexLong(String),
    Bool(bool),
}



// Must implement Nom's Input trait to use combinators like many0, etc.
// Rust won't let us implement this on Token natively, so we must create
// the wrapper class TokenSlice.
use nom::Input;

#[derive(Debug, Clone, PartialEq)]
struct TokenSlice<'a>(&'a [Token]);

impl<'a> nom::Input for TokenSlice<'a> {
    type Item = &'a Token;
    type Iter = std::slice::Iter<'a, Token>;
    type IterIndices = std::iter::Enumerate<std::slice::Iter<'a, Token>>;

    fn input_len(&self) -> usize {
        self.0.len()
    }

    fn take(&self, index: usize) -> Self {
        assert!(index <= self.0.len(), "Index out of bounds in take()");
        TokenSlice(&self.0[..index])
    }

    fn take_from(&self, index: usize) -> Self {
        assert!(index <= self.0.len(), "Index out of bounds in take_from()");
        TokenSlice(&self.0[index..])
    }

    fn take_split(&self, index: usize) -> (Self, Self) {
        assert!(index <= self.0.len(), "Index out of bounds in take_split()");
        (TokenSlice(&self.0[index..]), TokenSlice(&self.0[..index]))
    }

    fn position<P>(&self, predicate: P) -> Option<usize>
    where
        P: Fn(Self::Item) -> bool,
    {
        self.0.iter().position(predicate)
    }

    fn iter_elements(&self) -> Self::Iter {
        self.0.iter()
    }

    fn iter_indices(&self) -> Self::IterIndices {
        self.0.iter().enumerate()
    }

    fn slice_index(&self, count: usize) -> Result<usize, nom::Needed> {
        if count <= self.0.len() {
            Ok(count)
        } else {
            Err(nom::Needed::Unknown)
        }
    }
}
