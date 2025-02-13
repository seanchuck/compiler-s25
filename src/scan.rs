use std::iter::Scan;

use anyhow::Result;

/*
Tokens: Identifier, Keyword, string literal, numeric literal (decimal, hex), Operator
Keywords: if, bool, break, import, continue, else, false, for, while, int, long, return, len, true, void
Operators: -, !, int(), long(), *, /, %, +, -, <, <=, ==, >=, >, !=, &&, ||

Long: 100L
Whitespace: space, newline, comments (// and /**/), carriage return
*/


/*
Stages:
    1. Handle subset of tokens
    2. Handle entire set of tokens
    3. Handle illegal inputs (error handling)
    4. Format output as desired
*/




/*

    remaining + tokens
    array of chars --> array of tokens

    struct: create data type (don't think)




*/


/*
Take a Decaf program as an &str. Convert it into a vector of 
tokens to be used by the parser.
*/
#[allow(dead_code)]
#[derive(Debug)]
pub enum Token {
    Keyword(Keyword),    // Keyword variant has type Keyword
    Identifier(String),
    Symbol(Symbol),
    Literal(Literal),
    EOF,
    ScanError(ScanError)
}

#[derive(Debug)]
enum Keyword {
    If, Bool, Break, Import, Continue, Else, False, For, 
    While, Int, Long, Return, Len, True, Void,
}

#[derive(Debug)]
enum Symbol {
    Operator(Operator),
    Punctuation(Punctuation)
}

#[derive(Debug)]
enum Operator {
    Plus, Minus, Multiply, Divide, Modulo,
    Assign, PlusAssign, MinusAssign, MultiplyAssign, DivideAssign, ModuloAssign,
    Increment, Decrement,
    Equal, NotEqual, Less, Greater, LessEqual, GreaterEqual,
    LogicalAnd, LogicalOr, LogicalNot,
}

#[derive(Debug)]
#[allow(dead_code)]
enum Punctuation {
    LeftParen, RightParen, LeftBrace, RightBrace, LeftBracket, RightBracket,
    Semicolon, Comma,
}
#[derive(Debug)]
#[allow(dead_code)]
enum Literal {
    Int(i32), Long(i64), Bool(bool), Char(char), String(String),
}

#[derive(Debug)]
#[allow(dead_code)]
enum ScanError {
    Standard(String), Special(String),
}

#[allow(dead_code)]
impl ScanError {
    fn standard(msg: &str) -> Self {
        ScanError::Standard(msg.to_string())
    }

    fn special(msg: &str) -> Self {
        ScanError::Special(msg.to_string())
    }
}

// ---------------------------------------------------------------------------------------


// Option type is: Some<char> or None<char>
// fn peek(&program) -> Option<char> {
//     program.get(0).copied() // obtain ownership with copied
// }

// fn take_while(pattern_fn: fn(char) -> bool) -> Vec<char> {
//     self.program
//         .iter() 
//         .take_while(|&&c| pattern_fn(c)) 
//         .cloned()
//         .collect()
// }



// fn symbol_to_token(symbol: &str) -> Option<Token> {
//     match symbol {
//         "==" => Some(Token::Symbol(Symbol::Operator(Operator::Equal))),
//         "+" => Some(Token::Symbol(Symbol::Operator(Operator::Plus))),
//         "-" => Some(Token::Symbol(Symbol::Operator(Operator::Minus))),
//         "*" => Some(Token::Symbol(Symbol::Operator(Operator::Multiply))),
//         "/" => Some(Token::Symbol(Symbol::Operator(Operator::Divide))),
//         "%" => Some(Token::Symbol(Symbol::Operator(Operator::Modulo))),
//         "(" => Some(Token::Symbol(Symbol::Punctuation(Punctuation::LeftParen))),
//         ")" => Some(Token::Symbol(Symbol::Punctuation(Punctuation::RightParen))),
//         "{" => Some(Token::Symbol(Symbol::Punctuation(Punctuation::LeftBrace))),
//         "}" => Some(Token::Symbol(Symbol::Punctuation(Punctuation::RightBrace))),
//         "," => Some(Token::Symbol(Symbol::Punctuation(Punctuation::Comma))),
//         ";" => Some(Token::Symbol(Symbol::Punctuation(Punctuation::Semicolon))),
//         _ => None,
//     }
// }
// fn keyword_to_token(keyword: &str) -> Option<Token> {
    // match keyword {
    //     "if" => Some(Token::Keyword(Keyword::If)),
    //     "bool" => Some(Token::Keyword(Keyword::Bool)),
    //     "break" => Some(Token::Keyword(Keyword::Break)),
    //     "import" => Some(Token::Keyword(Keyword::Import)),
    //     "continue" => Some(Token::Keyword(Keyword::Continue)),
    //     "else" => Some(Token::Keyword(Keyword::Else)),
    //     "false" => Some(Token::Keyword(Keyword::False)),
    //     "for" => Some(Token::Keyword(Keyword::For)),
    //     "while" => Some(Token::Keyword(Keyword::While)),
    //     "int" => Some(Token::Keyword(Keyword::Int)),
    //     "long" => Some(Token::Keyword(Keyword::Long)),
    //     "return" => Some(Token::Keyword(Keyword::Return)),
    //     "len" => Some(Token::Keyword(Keyword::Len)),
    //     "true" => Some(Token::Keyword(Keyword::True)),
    //     "void" => Some(Token::Keyword(Keyword::Void)),
    //     _ => None, 
    // }
// }
    
    // fn lex(&mut self) -> Vec<Token> {
    //     let tokens: Vec<Token> = vec![];

    //     while !self.is_eof() {
    //         // Find whitespace
    //         if let Some(character) = self.program.get(0) {
    //             if character.is_whitespace() {
    //                 self.consume(1);
    //                 continue;
    //             }
    //         }

    //         // Find symbols
    //         let mut found_symbol = false;

    //         // 


    //     }


    //     tokens
    // }
    

// }

// split on whitespace

// take_while



// ---------------------------------------------------------------------------------------


fn consume(program: &mut Vec<char>, ntoken: i32) {
    let ntoken = ntoken.max(0) as usize;
    let ntoken = ntoken.min(program.len());
    program.drain(0..ntoken);
}

fn gobble_whitespace(program: &mut Vec<char>) {
    // program is a mutable reference
    while !program.is_empty() {
        // Only satified if we get Some and not None type
        if let Some(&character) = program.first() {
            if character.is_whitespace() {
                consume(program, 1);
                continue;
            }
        }
        break;
    }
}

// Special cases: [",'(){}_]

fn lex_keyword_or_identifier(program: &mut Vec<char>) -> Option<Token> {
    let mut keyword_or_identifier = String::new();

    while let Some(&character) = program.get(0) {
        // If we reach any of the following we should bail out: [ " , ' ( ) { } _ ]
        match character {
            ' ' | '(' | ')' => { break; }
            _ => {
                keyword_or_identifier.push(character);
                consume(program, 1); // Consume each character inside the string

            }

        }

    }

    match keyword_or_identifier.as_str() {
        "if" => Some(Token::Keyword(Keyword::If)),
        "bool" => Some(Token::Keyword(Keyword::Bool)),
        "break" => Some(Token::Keyword(Keyword::Break)),
        "import" => Some(Token::Keyword(Keyword::Import)),
        "continue" => Some(Token::Keyword(Keyword::Continue)),
        "else" => Some(Token::Keyword(Keyword::Else)),
        "false" => Some(Token::Keyword(Keyword::False)),
        "for" => Some(Token::Keyword(Keyword::For)),
        "while" => Some(Token::Keyword(Keyword::While)),
        "int" => Some(Token::Keyword(Keyword::Int)),
        "long" => Some(Token::Keyword(Keyword::Long)),
        "return" => Some(Token::Keyword(Keyword::Return)),
        "len" => Some(Token::Keyword(Keyword::Len)),
        "true" => Some(Token::Keyword(Keyword::True)),
        "void" => Some(Token::Keyword(Keyword::Void)),
        _ => {
            Some(Token::Identifier(keyword_or_identifier))
        }
    }
}

fn lex_string_literal(program: &mut Vec<char>) -> Option<Token> {
    let mut sliteral = String::new();
    consume(program, 1); // Consume opening quote

    while let Some(&character) = program.get(0) {
        if character == '"' {
            consume(program, 1); // Consume the closing quote
            return Some(Token::Literal(Literal::String(sliteral)));
        } else {
            sliteral.push(character);
            consume(program, 1); // Consume each character inside the string
        }
    }

    Some(Token::ScanError(ScanError::standard("String literal missing closing quote mark")))
}


// does the matching and shii
fn get_next_token(program: &mut Vec<char>) -> Option<Token> {
    if let Some(&character) = program.get(0) {
        let token = match character {
            '(' => Some(Token::Symbol(Symbol::Punctuation(Punctuation::LeftParen))),
            ')' => Some(Token::Symbol(Symbol::Punctuation(Punctuation::RightParen))),
            '{' => Some(Token::Symbol(Symbol::Punctuation(Punctuation::LeftBrace))),
            '}' => Some(Token::Symbol(Symbol::Punctuation(Punctuation::RightBrace))),
            '[' => Some(Token::Symbol(Symbol::Punctuation(Punctuation::LeftBracket))),
            ']' => Some(Token::Symbol(Symbol::Punctuation(Punctuation::RightBracket))),
            ';' => Some(Token::Symbol(Symbol::Punctuation(Punctuation::Semicolon))),
            ',' => Some(Token::Symbol(Symbol::Punctuation(Punctuation::Comma))),
            '"' => lex_string_literal(program),
            
            // Fallback: check for identifiers or numbers
            _ => {
                if character.is_alphanumeric() {
                    lex_keyword_or_identifier(program)
                } else {
                    None
                }
            }
        };

        consume(program, 1); 
        token
    } else {
        None
    }
}




/*
The main scan function for this file.
Input: a Decaf source file String as input. 
Effects: Outputs a sequence of tokens. 
*/
pub fn scan(file: &str) -> Vec<Token> {
    println!("SCANNING");
    let mut program: Vec<char> = file.chars().collect(); // convert into vector of characters
    let mut tokens: Vec<Token> = vec![];

    while let Some(&character) = program.get(0) {
        if character.is_whitespace() {
            gobble_whitespace(&mut program);
            continue;
        }

        if let Some(token) = get_next_token(&mut program) {
            tokens.push(token);
        }
    }

    println!("Tokens are: {:?}", tokens);
    tokens
}
