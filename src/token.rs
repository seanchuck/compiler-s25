/*
Token data structures for scanner.
*/

use std::ops::Deref;

/// Span keep track of line information
/// to throw more specific errors.
#[derive(Debug, Clone, PartialEq)]
pub struct Span {
    pub sline: i32,
    pub scol: i32,
    pub eline: i32,
    pub ecol: i32,
}

/// Token is the main data structure used by the scanner.
#[derive(Debug, Clone, PartialEq)]
#[allow(dead_code)]
pub enum Token {
    Keyword {
        value: Keyword,
        span: Span,
        display: String
    },
    Identifier {
        value: String,
        span: Span,
        display: String,
    },
    Symbol {
        value: Symbol,
        span: Span,
        display: String,
    },
    Literal {
        value: Literal,
        span: Span,
        display: String,
    }
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


/// A public wrapper around a slice of `Token`s to enable custom trait implementations.
/// Parser combinator `nom` requires implement custom trait `Input` to use combinators like
/// many0 and separated lists.
/// TokenSlice acts as a view (or a "window") over a Vec<Token>.
/// It allows you to process tokens incrementally without copying or modifying the original vector.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct TokenSlice<'a>(pub &'a [Token]);

/// Allow `TokenSlice` to behave like a standard
/// slice by implementing `Deref`.
impl<'a> Deref for TokenSlice<'a> {
    type Target = [Token];

    fn deref(&self) -> &Self::Target {
        self.0
    }
}

/// Implement `nom::Input` for `TokenSlice`,
/// allowing it to be used in Nom parsers.
impl<'a> nom::Input for TokenSlice<'a> {
    type Item = &'a Token;
    type Iter = std::slice::Iter<'a, Token>;
    type IterIndices = std::iter::Enumerate<std::slice::Iter<'a, Token>>;

    /// Returns the length of the input.
    fn input_len(&self) -> usize {
        self.0.len()
    }

    /// Returns a slice of the first `index` tokens.
    fn take(&self, index: usize) -> Self {
        assert!(index <= self.0.len(), "Index out of bounds in take()");
        TokenSlice(&self.0[..index])
    }

    /// Returns a slice starting at `index`.
    fn take_from(&self, index: usize) -> Self {
        assert!(index <= self.0.len(), "Index out of bounds in take_from()");
        TokenSlice(&self.0[index..])
    }

    /// Splits the input at `index`, returning two `TokenSlice`s.
    fn take_split(&self, index: usize) -> (Self, Self) {
        assert!(index <= self.0.len(), "Index out of bounds in take_split()");
        (TokenSlice(&self.0[index..]), TokenSlice(&self.0[..index]))
    }

    /// Finds the position of the first element satisfying the predicate.
    fn position<P>(&self, predicate: P) -> Option<usize>
    where
        P: Fn(Self::Item) -> bool,
    {
        self.0.iter().position(predicate)
    }

    /// Returns an iterator over the elements.
    fn iter_elements(&self) -> Self::Iter {
        self.0.iter()
    }

    /// Returns an iterator over elements and their indices.
    fn iter_indices(&self) -> Self::IterIndices {
        self.0.iter().enumerate()
    }

    /// Computes the slice index for a given count.
    fn slice_index(&self, count: usize) -> Result<usize, nom::Needed> {
        if count <= self.0.len() {
            Ok(count)
        } else {
            Err(nom::Needed::Unknown)
        }
    }
}
