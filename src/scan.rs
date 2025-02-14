/*
Scanner.
*/

use anyhow::{anyhow, Result};
/*
Tokens: Identifier, Keyword, string literal, numeric literal (decimal, hex), Operator
Keywords: if, bool, break, import, continue, else, false, for, while, int, long, return, len, true, void
Operators: -, !, int(), long(), *, /, %, +, -, <, <=, ==, >=, >, !=, &&, ||

Long: 100L
Whitespace: space, newline, comments (// and /**/), carriage return
*/

/*
Stages:
    1. Rigorously check the grammar for what else needs to be parsed (100L, int(), long(), etc.)
    2. Handle illegal inputs
    3. Format output as desired
    4. Keep track of line numbers, etc.
*/

/*
Rigorous lexer spec:
    Keywords:
    Identifiers:
    Symbols:
    Literals:
    Errors:


Casts:
    int() as int, (, )
    long() as long, (, )


Print the following as specialized token types:
    - CHARLITERAL, INTLITERAL, LONGLITERAL, BOOLEANLITERAL, STRINGLITERAL and IDENTIFIER
    - Everything else: just the token

Must keep track of current line until \n!


Whitespace: newline (\n), tab (\t),

Legal ASCII char: TODO
    - NO: quote ("), single quote ('), backslash (\)
    - Allows: ASCII in [32, 126], \\, \t, \n, \r, \f


Errors:
    - unclosed ""
    - unclosed ''
    - illegal chars: outside ASCII
    - invalid escapes: not in (\"  \'  \\  \t  \n  \r  \f)
    - illegal identifier: must start with alphabetical or _
    - unclsoed comments /* */


All tokens:
    import, void, int, long, bool, if, else, for, while, return, break,
    continue, ;, =, +=, -=, *=, /=, %=, ++, --, int(), long(), len(),
    -, !, (), +, -, *, /, %, <, >, <=, >=, ==, !=, &&, ||, a-z,
    A-Z, _, 0-9, hex: 0x[0-9][a-f][A-F],
    long: dec or hex literal + L, true, false,

TODO: hex, +L, comments, errors
    - NO: quote ("), single quote ('), backslash (\), Allows: ASCII in [32, 126], \\, \t, \n, \r, \f


Ensure: consumption is done correctly


*/

#[derive(Debug)]
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
    False,
    For,
    While,
    Int,
    Long,
    Return,
    Len,
    True,
    Void,
}

#[derive(Debug)]
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
    Int(i32),
    Long(i64),
    Bool(bool),
    String(String),
}

// ---------------------------------------------------------------------------------------
/*
Consume up to nchar characters from the program vector.
*/
fn consume(program: &mut Vec<char>, current_col: &mut i32, nchar: i32) {
    let nchar = nchar.max(0) as usize;
    let nchar = nchar.min(program.len());
    program.drain(0..nchar);
    *current_col += nchar as i32;
}

/*
Consume all whitespace characters before the next
token or EOF.

Handles line numbers since is the only fn handling newlines.
*/
fn gobble_whitespace(program: &mut Vec<char>, current_line: &mut i32, current_col: &mut i32) {
    // program is a mutable reference
    while !program.is_empty() {
        // Only satified if we get Some and not None type
        if let Some(&character) = program.first() {
            if character.is_whitespace() {
                if character == '\n' {
                    *current_line += 1;
                    *current_col = 1;
                }
                consume(program, current_col, 1);
                continue;
            }
        }
        break;
    }
}

//
fn gobble_comment(program: &mut Vec<char>) {}

fn lex_hex() {}

/*
Lex a Keyword or Identifier.
*/
fn lex_keyword_or_identifier(program: &mut Vec<char>, current_col: &mut i32) -> Result<Token> {
    let mut keyword_or_identifier = String::new();

    // Build the keyword or identifier
    while let Some(&character) = program.get(0) {
        // Bail out if we see whitespace or opening parens, signaling end of token
        
        match character {
            ' ' | '(' => {
                break;
            }
            _ => {
                keyword_or_identifier.push(character);
                consume(program, current_col, 1); // Consume each character inside the string
            }
        }
    }

    match keyword_or_identifier.as_str() {
        "if" => Ok(Token::Keyword(Keyword::If)),
        "bool" => Ok(Token::Keyword(Keyword::Bool)),
        "break" => Ok(Token::Keyword(Keyword::Break)),
        "import" => Ok(Token::Keyword(Keyword::Import)),
        "continue" => Ok(Token::Keyword(Keyword::Continue)),
        "else" => Ok(Token::Keyword(Keyword::Else)),
        "false" => Ok(Token::Keyword(Keyword::False)),
        "for" => Ok(Token::Keyword(Keyword::For)),
        "while" => Ok(Token::Keyword(Keyword::While)),
        "int" => Ok(Token::Keyword(Keyword::Int)),
        "long" => Ok(Token::Keyword(Keyword::Long)),
        "return" => Ok(Token::Keyword(Keyword::Return)),
        "len" => Ok(Token::Keyword(Keyword::Len)),
        "true" => Ok(Token::Keyword(Keyword::True)),
        "void" => Ok(Token::Keyword(Keyword::Void)),

        // If the token doesn't match any keywords, return it as an identifier
        _ => Ok(Token::Identifier(keyword_or_identifier)),
    }
}

/*
Lex a string literal, identified by a leading quotation mark.
Returns errors for incomplete strings and illegal characters.
*/
fn lex_string_literal(program: &mut Vec<char>, current_col: &mut i32) -> Result<Token> {
    let mut sliteral = String::new();
    consume(program, current_col, 1);

    while let Some(&char1) = program.get(0) {
        consume(program, current_col, 1);

        match char1 {
            '"' => return Ok(Token::Literal(Literal::String(sliteral))), // match closing quote: end of string
            '\'' => return Err(anyhow!("Error: Illegal single quote in string literal")), // Reject single quote
            '\\' => match program.get(0) {
                Some(&char2 @ ('"' | '\\' | 't' | 'n' | 'r' | 'f')) => {
                    sliteral.push(match char2 {
                        '"' => '"',
                        '\\' => '\\',
                        't' => '\t',
                        'n' => '\n',
                        'r' => '\r',
                        'f' => '\x0C',
                        _ => unreachable!(),
                    });
                    consume(program, current_col, 1);
                }
                Some(&invalid) => {
                    return Err(anyhow!("Error: Invalid escape sequence \\{}", invalid))
                } // Reject invalid backslashes
                None => {
                    return Err(anyhow!(
                        "Error: Incomplete escape sequence in string literal"
                    ))
                }
            },
            _ => sliteral.push(char1),
        }
    }

    Err(anyhow!("Error: String literal missing closing quote"))
}

/*
Lex a character literal, identified by a leading single quote.
Returns errors for incomplete or illegal characters.
*/
fn lex_char_literal(program: &mut Vec<char>, current_col: &mut i32) -> Result<Token> {
    consume(program, current_col, 1); // Consume opening quote (')

    let char1 = match program.get(0) {
        Some(&c) => c,
        None => {
            return Err(anyhow!(
                "Error: Unexpected end of input in character literal"
            ));
        }
    };

    consume(program, current_col, 1); // Move to next character

    // Match length-2 escaped sequences
    let char_value = if char1 == '\\' {
        match program.get(0) {
            Some(&escaped_char @ ('\'' | '"' | '\\' | 't' | 'n' | 'r' | 'f')) => {
                consume(program, current_col, 1);
                escaped_char
            }
            Some(&invalid_char) => {
                return Err(anyhow!("Error: Invalid escape sequence \\{}", invalid_char));
            }
            None => return Err(anyhow!("Error: Incomplete escape sequence")),
        }
    } else {
        // Reject illegal single characters: `'`, `"`, `\`
        if char1 == '\'' || char1 == '"' || char1 == '\\' {
            return Err(anyhow!(
                "Error: Illegal character '{}' inside character literal",
                char1
            ));
        }
        char1
    };

    // Ensure there is a closing single quote
    match program.get(0) {
        Some('\'') => {
            consume(program, current_col, 1); // Consume closing quote
            Ok(Token::Literal(Literal::Char(char_value)))
        }
        _ => Err(anyhow!("Error: Character literal missing closing quote")),
    }
}

// does the matching and shii
fn get_next_token(program: &mut Vec<char>, current_col: &mut i32) -> Result<Token> {
    if let Some(&char1) = program.get(0) {
        // Attempt to match length-2 symbols first
        if let Some(&char2) = program.get(1) {
            let token = match (char1, char2) {
                ('=', '=') => Some(Token::Symbol(Symbol::Operator(Operator::Equal))),
                ('!', '=') => Some(Token::Symbol(Symbol::Operator(Operator::NotEqual))),
                ('<', '=') => Some(Token::Symbol(Symbol::Operator(Operator::LessEqual))),
                ('>', '=') => Some(Token::Symbol(Symbol::Operator(Operator::GreaterEqual))),
                ('&', '&') => Some(Token::Symbol(Symbol::Operator(Operator::LogicalAnd))),
                ('|', '|') => Some(Token::Symbol(Symbol::Operator(Operator::LogicalOr))),
                ('+', '+') => Some(Token::Symbol(Symbol::Operator(Operator::Increment))),
                ('-', '-') => Some(Token::Symbol(Symbol::Operator(Operator::Decrement))),
                ('+', '=') => Some(Token::Symbol(Symbol::Operator(Operator::PlusAssign))),
                ('-', '=') => Some(Token::Symbol(Symbol::Operator(Operator::MinusAssign))),
                ('*', '=') => Some(Token::Symbol(Symbol::Operator(Operator::MultiplyAssign))),
                ('/', '=') => Some(Token::Symbol(Symbol::Operator(Operator::DivideAssign))),
                ('%', '=') => Some(Token::Symbol(Symbol::Operator(Operator::ModuloAssign))),
                _ => None,
            };
            if token.is_some() {
                consume(program, current_col, 2);
                return Ok(token.unwrap());
            }
        };

        println!("current program: {:?}", program);
        // Match single-character Symbols directly
        let token = match char1 {
            '(' => Some(Token::Symbol(Symbol::Punctuation(Punctuation::LeftParen))),
            ')' => Some(Token::Symbol(Symbol::Punctuation(Punctuation::RightParen))),
            '{' => Some(Token::Symbol(Symbol::Punctuation(Punctuation::LeftBrace))),
            '}' => Some(Token::Symbol(Symbol::Punctuation(Punctuation::RightBrace))),
            '[' => Some(Token::Symbol(Symbol::Punctuation(Punctuation::LeftBracket))),
            ']' => Some(Token::Symbol(Symbol::Punctuation(
                Punctuation::RightBracket,
            ))),
            ';' => Some(Token::Symbol(Symbol::Punctuation(Punctuation::Semicolon))),
            ',' => Some(Token::Symbol(Symbol::Punctuation(Punctuation::Comma))),

            '=' => Some(Token::Symbol(Symbol::Operator(Operator::Assign))),
            '+' => Some(Token::Symbol(Symbol::Operator(Operator::Plus))),
            '-' => Some(Token::Symbol(Symbol::Operator(Operator::Minus))),
            '*' => Some(Token::Symbol(Symbol::Operator(Operator::Multiply))),
            '/' => Some(Token::Symbol(Symbol::Operator(Operator::Divide))),
            '%' => Some(Token::Symbol(Symbol::Operator(Operator::Modulo))),
            '<' => Some(Token::Symbol(Symbol::Operator(Operator::Greater))),
            '>' => Some(Token::Symbol(Symbol::Operator(Operator::Less))),
            '!' => Some(Token::Symbol(Symbol::Operator(Operator::LogicalNot))),

            // These fn handle their own consumption and return immediately
            '\'' => return lex_char_literal(program, current_col),
            '"' => return lex_string_literal(program, current_col),

            // Fallback: check for Keyword, Identifier, Literal
            _ => return lex_keyword_or_identifier(program, current_col),
        };

        // Consume for direct matches only
        if token.is_some() {
            consume(program, current_col, 1);
        }
        return Ok(token.unwrap());
    }
    Err(anyhow!("Failed to generate next token"))
}


/*
The main scan function for this file.
    Input: a Decaf source file String as input.
    Effects: Outputs a sequence of Tokens.
*/
pub fn scan(file: &str) -> Vec<Token> {
    println!("SCANNING");
    let mut program: Vec<char> = file.chars().collect();
    let mut tokens: Vec<Token> = vec![];

    // Keep track of line and column for error-handling purposes
    let (mut current_line, mut current_col) = (1, 1);

    // Clear whitespace and gobble tokens until hit EOF
    while let Some(&character) = program.get(0) {
        if character.is_whitespace() {
            gobble_whitespace(&mut program, &mut current_line, &mut current_col);
            continue;
        }

        let token = get_next_token(&mut program, &mut current_col);
        match token {
            Ok(token_value) => {
                println!(
                    "Token received at line {} col {}: {:?}\n",
                    current_line, current_col, token_value
                );
                tokens.push(token_value);
            }
            Err(token_value) => {
                println!(
                    "Error received at line {} col {}: {:?}\n",
                    current_line, current_col, token_value
                );
            }
        }
    }

    println!("Tokens are:\n {:?}", tokens);
    tokens
}
