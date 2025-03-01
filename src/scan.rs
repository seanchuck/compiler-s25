/*
Scanner.
*/

use super::token::*;
use anyhow::{anyhow, Result};
use std::process;

/*
Tasks:
    1. Gobble whitespace and comments.
    2. Lex: Keyword, Identifier, Punctuation, Operators, Literals (char, string, bool, numeric (hex, decimal, int, long))
    3. Throw errors for illegal inputs (illegal chars, unmatched punctuation, etc.)
    4. Keep track of line numbers, etc.
    5. Format output


All tokens:
    Whitespace: space, newline, comments (// and /**/), carriage return
    Tokens: Identifier, Keyword, string literal, numeric literal (decimal, hex), Operator
    Keywords: if, bool, break, import, continue, else, false, for, while, int, long, return, len, true, void
    Operators: ;, =, +=, -=, *=, /=, %=, ++, --, int(), long(), len(),
    -, !, (), +, -, *, /, %, <, >, <=, >=, ==, !=, &&, ||, a-z,
    A-Z, _, 0-9, hex: 0x[0-9][a-f][A-F],
        long: dec or hex literal + L, true, false,

    Whitespace: space, newline, comments (// and /**/), carriage return
*/

/// Consume up to nchar characters from the program vector.
fn consume(program: &mut Vec<char>, current_col: &mut i32, nchar: i32) {
    let nchar = nchar.max(0) as usize;
    let nchar = nchar.min(program.len());
    program.drain(0..nchar);
    *current_col += nchar as i32;
}

/// Consume all whitespace characters before the next
/// token or EOF.
fn gobble_whitespace(program: &mut Vec<char>, current_line: &mut i32, current_col: &mut i32) {
    while !program.is_empty() {
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

/// Attempts to gobble a comment.
/// Throws an error if a block comment is not closed.
fn gobble_comment(
    program: &mut Vec<char>,
    current_col: &mut i32,
    current_line: &mut i32,
) -> Result<String> {
    if let Some((&char1, &char2)) = program.get(0).zip(program.get(1)) {
        consume(program, current_col, 2);

        match (char1, char2) {
            // Line comment
            ('/', '/') => {
                while let Some(&c) = program.first() {
                    consume(program, current_col, 1);
                    if c == '\n' {
                        *current_line += 1;
                        *current_col = 1;
                        break;
                    }
                }
                Ok("Gobbled line comment".to_string())
            }
            // Block comment
            ('/', '*') => {
                while let Some(&c) = program.first() {
                    consume(program, current_col, 1);
                    // pattern matching with &'\\'!!!
                    if c == '*' && program.first() == Some(&'/') {
                        consume(program, current_col, 1);
                        return Ok("Gobbled block comment".to_string());
                    }
                    if c == '\n' {
                        *current_line += 1;
                        *current_col = 1;
                    }
                }
                Err(anyhow!("Unclosed block comment"))
            }
            _ => Err(anyhow!("Unrecognized comment syntax")),
        }
    } else {
        Err(anyhow!("Incomplete comment"))
    }
}

/// Helper to lex integer based on decimal or hex type.
fn lex_numeric_helper(
    program: &mut Vec<char>,
    current_col: &mut i32,
    current_line: &i32,
    is_hex: bool,
) -> Result<Token, anyhow::Error> {
    let start_line = *current_line;
    let start_col = *current_col;
    let mut nliteral = String::new();

    while let Some(&c) = program.get(0) {
        match c {
            '0'..='9' => {
                nliteral.push(c);
                consume(program, current_col, 1);
            }
            // hex allows a-f and A-F
            'a'..='f' | 'A'..='F' if is_hex => {
                nliteral.push(c);
                consume(program, current_col, 1);
            }
            'L' => {
                consume(program, current_col, 1);
                
                let literal_value = match is_hex {
                    true => Literal::HexLong(nliteral.clone()),
                    false => Literal::Long(nliteral.clone()),
                };
                
                return Ok(Token::Literal {
                    value: literal_value,
                    span: Some(Span {
                        sline: start_line,
                        scol: start_col,
                        eline: *current_line,
                        ecol: *current_col,
                    }),
                    display: nliteral,
                });
            }
            _ => break, // Stop parsing on any other character
        }
    }

    // If we hit EOF or break, return
    let literal_value = match is_hex {
        true => Literal::HexInt(nliteral.clone()),
        false => Literal::Int(nliteral.clone()),
    };
    return Ok(Token::Literal {
        value: literal_value,
        span: Some(Span {
            sline: start_line,
            scol: start_col,
            eline: *current_line,
            ecol: *current_col,
        }),
        display: nliteral,
    });
}

/// Lex a decimal or hex numeric literal of int or long type.
fn lex_numeric_literal(
    program: &mut Vec<char>,
    current_line: &mut i32,
    current_col: &mut i32,
) -> Result<Token> {
    if let Some(&char1) = program.get(0) {
        if let Some(&char2) = program.get(1) {
            match (char1, char2) {
                ('0', 'x') => {
                    consume(program, current_col, 2);
                    return lex_numeric_helper(program, current_col, current_line, true);
                }
                _ => {
                    return lex_numeric_helper(program, current_col, current_line, false);
                }
            }
        }
        return lex_numeric_helper(program, current_col, current_line, false);
    }
    // Potentially while loop if we don't do anything (i.e., consume),
    // but we never actually hit this case
    Err(anyhow!("Numeric literal lexing ending unexpectedly"))
}

/// Lex a Keyword or Identifier.
/// Attempt to match a Keyword first.
fn lex_keyword_or_identifier(
    program: &mut Vec<char>,
    current_line: &mut i32,
    current_col: &mut i32,
) -> Result<Token> {
    let mut keyword_or_identifier = String::new();
    let start_line = *current_line;
    let start_col = *current_col;

    // Build the keyword or identifier as the longest string of alphanumerics or underscores
    while let Some(&character) = program.get(0) {
        if character.is_alphanumeric() || character == '_' {
            keyword_or_identifier.push(character);
            consume(program, current_col, 1);
        } else {
            break;
        }
    }
    
    let span = Some(Span {
        sline: start_line,
        scol: start_col,
        eline: *current_line,
        ecol: *current_col,
    });

    let token = match keyword_or_identifier.as_str() {
        "if" => Token::Keyword {
            value: Keyword::If,
            span: span.clone(),
            display: "if".to_string(),
        },
        "bool" => Token::Keyword {
            value: Keyword::Bool,
            span: span.clone(),
            display: "bool".to_string(),
        },
        "break" => Token::Keyword {
            value: Keyword::Break,
            span: span.clone(),
            display: "break".to_string(),
        },
        "import" => Token::Keyword {
            value: Keyword::Import,
            span: span.clone(),
            display: "import".to_string(),
        },
        "continue" => Token::Keyword {
            value: Keyword::Continue,
            span: span.clone(),
            display: "continue".to_string(),
        },
        "else" => Token::Keyword {
            value: Keyword::Else,
            span: span.clone(),
            display: "else".to_string(),
        },
        "for" => Token::Keyword {
            value: Keyword::For,
            span: span.clone(),
            display: "for".to_string(),
        },
        "while" => Token::Keyword {
            value: Keyword::While,
            span: span.clone(),
            display: "while".to_string(),
        },
        "int" => Token::Keyword {
            value: Keyword::Int,
            span: span.clone(),
            display: "int".to_string(),
        },
        "long" => Token::Keyword {
            value: Keyword::Long,
            span: span.clone(),
            display: "long".to_string(),
        },
        "return" => Token::Keyword {
            value: Keyword::Return,
            span: span.clone(),
            display: "return".to_string(),
        },
        "len" => Token::Keyword {
            value: Keyword::Len,
            span: span.clone(),
            display: "len".to_string(),
        },
        "void" => Token::Keyword {
            value: Keyword::Void,
            span: span.clone(),
            display: "void".to_string(),
        },
    
        // Boolean literals
        "false" => Token::Literal {
            value: Literal::Bool(false),
            span: span.clone(),
            display: "false".to_string(),
        },
        "true" => Token::Literal {
            value: Literal::Bool(true),
            span: span.clone(),
            display: "true".to_string(),
        },
    
        // Identifiers
        _ => Token::Identifier {
            value: keyword_or_identifier.clone(),
            span,
            display: keyword_or_identifier.clone(),
        },
    };
    
    return Ok(token);
    
    

}

/// Lex a string literal, identified by a leading quotation mark.
/// Returns errors for incomplete strings and illegal characters.
fn lex_string_literal(
    program: &mut Vec<char>,
    current_line: &mut i32,
    current_col: &mut i32,
) -> Result<Token> {
    let mut sliteral = String::new();
    let start_line = *current_line;
    let start_col = *current_col;
    
    consume(program, current_col, 1);

    while let Some(&char1) = program.get(0) {
        consume(program, current_col, 1);
        // Actual return, newline, tabs are not allowable
        // only corresponding escaped char sequences
        match char1 {
            '\n' | '\t' | '\r' => {
                return Err(anyhow!(
                    "Unexpected newline,tab, or return in string literal"
                ));
            }
            _ => {}
        }

        match char1 {
            '"' => {
                return Ok(Token::Literal {
                    value: Literal::String(sliteral.clone()),
                    span: Some(Span {
                        sline: start_line,
                        scol: start_col,
                        eline: *current_line,
                        ecol: *current_col,
                    }),
                    display: sliteral.clone(),
                });
                
            }
            '\'' => return Err(anyhow!("Illegal single quote in string literal")),
            '\\' => match program.get(0) {
                Some(&char2 @ ('"' | '\'' | '\\' | 't' | 'n' | 'r' | 'f')) => {
                    sliteral.push_str(match char2 {
                        '"' => "\\\"",
                        '\'' => "\\'",
                        '\\' => "\\\\",
                        't' => "\\t",
                        'n' => "\\n",
                        'r' => "\\r",
                        'f' => "\\f",
                        _ => unreachable!(),
                    });
                    consume(program, current_col, 1);
                }
                Some(&invalid) => return Err(anyhow!("Invalid escape sequence \'\\{}\'", invalid)),
                None => return Err(anyhow!("Incomplete escape sequence in string literal")),
            },
            _ => sliteral.push(char1),
        }
    }

    Err(anyhow!("String literal missing closing quote"))
}

/// Lex a character literal, identified by a leading single quote.
/// Returns errors for incomplete or illegal characters.
fn lex_char_literal(
    program: &mut Vec<char>,
    current_line: &mut i32,
    current_col: &mut i32,
) -> Result<Token> {
    consume(program, current_col, 1);
    let start_line = *current_line;
    let start_col = *current_col;

    let char1 = match program.get(0) {
        Some(&c) => c,
        None => {
            return Err(anyhow!("Unexpected end of input in character literal"));
        }
    };

    match char1 {
        '\n' | '\t' | '\r' => {
            return Err(anyhow!("Unexpected newline,tab, or return in char literal"));
        }
        _ => {}
    }

    consume(program, current_col, 1); // Move to next character

    // Match escaped sequences first
    let char_value = if char1 == '\\' {
        match program.get(0) {
            Some(&escaped_char) => {
                consume(program, current_col, 1);
                match escaped_char {
                    '\'' => '\'',
                    '"' => '"',
                    '\\' => '\\',
                    't' => '\t',
                    'n' => '\n',
                    'r' => '\r',
                    'f' => '\x0C',
                    _ => {
                        return Err(anyhow!("Invalid escape sequence \\{}", escaped_char));
                    }
                }
            }
            None => '\\', // If there's no character after '\', just return it
        }
    } else {
        // Reject illegal single characters: `'`, `"`, `\`
        if char1 == '\'' || char1 == '"' || char1 == '\\' {
            return Err(anyhow!(
                "Illegal character '{}' inside character literal",
                char1
            ));
        }
        char1
    };

    // Ensure there is a closing single quote
    match program.get(0) {
        Some('\'') => {
            consume(program, current_col, 1); // Consume closing quote
            Ok(Token::Literal {
                value: Literal::Char(char_value),
                span: Some(Span {
                    sline: start_line,
                    scol: start_col,
                    eline: *current_line,
                    ecol: *current_col,
                }),
                display: char_value.to_string(),
            })
            
        }
        _ => Err(anyhow!("Character literal missing closing quote")),
    }
}

/*
Main function to get the next token from the program.

Assumes immediate whitespace and tokens have been gobbled,
so can immediately begin matching.
*/
fn get_next_token(
    program: &mut Vec<char>,
    current_line: &mut i32,
    current_col: &mut i32,
) -> Result<Token> {
    if let Some(&char1) = program.get(0) {
        let span = Some(Span {
            sline: *current_line,
            scol: *current_col,
            eline: *current_line, // Assuming single-token span initially
            ecol: *current_col + 1, // Adjust for token width
        });
    
        // Attempt to match length-2 symbols first
        if let Some(&char2) = program.get(1) {
            let token = match (char1, char2) {
                ('=', '=') => Some(Token::Symbol {
                    value: Symbol::Operator(Operator::Equal),
                    span: span.clone(),
                    display: "==".to_string(),
                }),
                ('+', '+') => Some(Token::Symbol {
                    value: Symbol::Operator(Operator::Increment),
                    span: span.clone(),
                    display: "++".to_string(),
                }),
                ('-', '-') => Some(Token::Symbol {
                    value: Symbol::Operator(Operator::Decrement),
                    span: span.clone(),
                    display: "--".to_string(),
                }),
                ('!', '=') => Some(Token::Symbol {
                    value: Symbol::Operator(Operator::NotEqual),
                    span: span.clone(),
                    display: "!=".to_string(),
                }),
                ('<', '=') => Some(Token::Symbol {
                    value: Symbol::Operator(Operator::LessEqual),
                    span: span.clone(),
                    display: "<=".to_string(),
                }),
                ('>', '=') => Some(Token::Symbol {
                    value: Symbol::Operator(Operator::GreaterEqual),
                    span: span.clone(),
                    display: ">=".to_string(),
                }),
                ('&', '&') => Some(Token::Symbol {
                    value: Symbol::Operator(Operator::LogicalAnd),
                    span: span.clone(),
                    display: "&&".to_string(),
                }),
                ('|', '|') => Some(Token::Symbol {
                    value: Symbol::Operator(Operator::LogicalOr),
                    span: span.clone(),
                    display: "||".to_string(),
                }),
                ('+', '=') => Some(Token::Symbol {
                    value: Symbol::Operator(Operator::PlusAssign),
                    span: span.clone(),
                    display: "+=".to_string(),
                }),
                ('-', '=') => Some(Token::Symbol {
                    value: Symbol::Operator(Operator::MinusAssign),
                    span: span.clone(),
                    display: "-=".to_string(),
                }),
                ('*', '=') => Some(Token::Symbol {
                    value: Symbol::Operator(Operator::MultiplyAssign),
                    span: span.clone(),
                    display: "*=".to_string(),
                }),
                ('/', '=') => Some(Token::Symbol {
                    value: Symbol::Operator(Operator::DivideAssign),
                    span: span.clone(),
                    display: "/=".to_string(),
                }),
                ('%', '=') => Some(Token::Symbol {
                    value: Symbol::Operator(Operator::ModuloAssign),
                    span: span.clone(),
                    display: "%=".to_string(),
                }),
                _ => None,
            };
    
            if let Some(token) = token {
                consume(program, current_col, 2);
                return Ok(token);
            }
        }
    
        // Match single-character symbols
        let token = match char1 {
            '(' => Some(Token::Symbol {
                value: Symbol::Punctuation(Punctuation::LeftParen),
                span: span.clone(),
                display: "(".to_string(),
            }),
            ')' => Some(Token::Symbol {
                value: Symbol::Punctuation(Punctuation::RightParen),
                span: span.clone(),
                display: ")".to_string(),
            }),
            '{' => Some(Token::Symbol {
                value: Symbol::Punctuation(Punctuation::LeftBrace),
                span: span.clone(),
                display: "{".to_string(),
            }),
            '}' => Some(Token::Symbol {
                value: Symbol::Punctuation(Punctuation::RightBrace),
                span: span.clone(),
                display: "}".to_string(),
            }),
            '[' => Some(Token::Symbol {
                value: Symbol::Punctuation(Punctuation::LeftBracket),
                span: span.clone(),
                display: "[".to_string(),
            }),
            ']' => Some(Token::Symbol {
                value: Symbol::Punctuation(Punctuation::RightBracket),
                span: span.clone(),
                display: "]".to_string(),
            }),
            ';' => Some(Token::Symbol {
                value: Symbol::Punctuation(Punctuation::Semicolon),
                span: span.clone(),
                display: ";".to_string(),
            }),
            ',' => Some(Token::Symbol {
                value: Symbol::Punctuation(Punctuation::Comma),
                span: span.clone(),
                display: ",".to_string(),
            }),
            '=' => Some(Token::Symbol {
                value: Symbol::Operator(Operator::Assign),
                span: span.clone(),
                display: "=".to_string(),
            }),
            '+' => Some(Token::Symbol {
                value: Symbol::Operator(Operator::Plus),
                span: span.clone(),
                display: "+".to_string(),
            }),
            '-' => Some(Token::Symbol {
                value: Symbol::Operator(Operator::Minus),
                span: span.clone(),
                display: "-".to_string(),
            }),
            '*' => Some(Token::Symbol {
                value: Symbol::Operator(Operator::Multiply),
                span: span.clone(),
                display: "*".to_string(),
            }),
            '/' => Some(Token::Symbol {
                value: Symbol::Operator(Operator::Divide),
                span: span.clone(),
                display: "/".to_string(),
            }),
            '%' => Some(Token::Symbol {
                value: Symbol::Operator(Operator::Modulo),
                span: span.clone(),
                display: "%".to_string(),
            }),
            '<' => Some(Token::Symbol {
                value: Symbol::Operator(Operator::Less),
                span: span.clone(),
                display: "<".to_string(),
            }),
            '>' => Some(Token::Symbol {
                value: Symbol::Operator(Operator::Greater),
                span: span.clone(),
                display: ">".to_string(),
            }),
            '!' => Some(Token::Symbol {
                value: Symbol::Operator(Operator::LogicalNot),
                span: span.clone(),
                display: "!".to_string(),
            }),
    
            // These fn handle their own consumption
            '\'' => return lex_char_literal(program, current_line, current_col),
            '"' => return lex_string_literal(program, current_line, current_col),

            // If no direct match: lex as Keyword, Identifier, and Literal
            _ => {
                if char1.is_alphabetic() || char1 == '_' {
                    return lex_keyword_or_identifier(program, current_line, current_col);
                } else if char1.is_numeric() {
                    return lex_numeric_literal(program, current_line, current_col);
                } else {
                    // Consume a character to prevent infinite loops!
                    consume(program, current_col, 1);
                    return Err(anyhow!("Character not recognized!"));
                }
            }
        };

        // Consume for direct matches only
        if token.is_some() {
            consume(program, current_col, 1);
        }
        return Ok(token.unwrap());
    }
    Err(anyhow!("Failed to generate next token"))
}

/// The main scan function for this file.
///  - Input: a Decaf source file String as input.
///  - Effects: Outputs a sequence of Tokens.
///     If debug is specified, will print the sequence of tokens
///     using the provided writer
///     
pub fn scan(
    file: &str,
    filename: &str,
    writer: &mut Box<dyn std::io::Write>,
    debug: bool,
) -> Vec<Token> {
    let mut program: Vec<char> = file.chars().collect();
    let mut tokens: Vec<Token> = vec![];
    let mut found_err = false;

    // Keep track of line and column for error-handling purposes
    let (mut current_line, mut current_col) = (1, 1);

    // Clear whitespace and comments; gobble tokens until hit EOF
    while let Some(&char1) = program.get(0) {
        if char1.is_whitespace() {
            gobble_whitespace(&mut program, &mut current_line, &mut current_col);
            continue;
        }

        if char1 == '/' {
            if let Some(&char2) = program.get(1) {
                match (char1, char2) {
                    // match line and block comments
                    ('/', '/') | ('/', '*') => {
                        if gobble_comment(&mut program, &mut current_col, &mut current_line)
                            .is_err()
                        {
                            println!("Error while processing comment!\n");
                        }
                        continue;
                    }
                    _ => {}
                }
            }
        }

        // Generate and display the next token to standard out
        let wrapped_token = get_next_token(&mut program, &mut current_line, &mut current_col);
        match wrapped_token {
            Ok(token) => {
                // extract line number
                let line_number = match &token {
                    Token::Keyword { span, .. }
                    | Token::Identifier { span, .. }
                    | Token::Literal { span, .. }
                    | Token::Symbol { span, .. } => {
                        span.as_ref().map_or("unknown".to_string(), |s| s.sline.to_string())
                    }
                    _ => unreachable!()
                };

                // extract token display for non-special tokens
                let token_display = match &token {
                    Token::Keyword { display, .. }
                    | Token::Identifier { display, .. }
                    | Token::Literal { display, .. }
                    | Token::Symbol { display, .. } => {
                        display.as_str()
                    }
                    _ => unreachable!()
                };
        
                let template_string = match &token {
                    Token::Identifier { value, .. } => {
                        format!("{} IDENTIFIER {}", line_number, value)
                    }
                    Token::Literal { value: Literal::Char(text), .. } => {
                        let display_char = match text {
                            '\n' => "\\n".to_string(),
                            '\t' => "\\t".to_string(),
                            '\r' => "\\r".to_string(),
                            '\x0C' => "\\f".to_string(),
                            '\'' => "\\'".to_string(),
                            '"' => "\\\"".to_string(),
                            '\\' => "\\\\".to_string(),
                            _ => text.to_string(),
                        };
                        format!("{} CHARLITERAL \'{}\'", line_number, display_char)
                    }
                    Token::Literal { value: Literal::String(text), .. } => {
                        format!("{} STRINGLITERAL \"{}\"", line_number, text)
                    }
                    Token::Literal { value: Literal::Int(value), .. } => {
                        format!("{} INTLITERAL {}", line_number, value)
                    }
                    Token::Literal { value: Literal::Long(value), .. } => {
                        format!("{} LONGLITERAL {}L", line_number, value)
                    }
                    Token::Literal { value: Literal::HexInt(value), .. } => {
                        format!("{} INTLITERAL 0x{}", line_number, value)
                    }
                    Token::Literal { value: Literal::HexLong(value), .. } => {
                        format!("{} LONGLITERAL 0x{}L", line_number, value)
                    }
                    Token::Literal { value: Literal::Bool(value), .. } => {
                        format!("{} BOOLEANLITERAL {}", line_number, value)
                    }
                    _ => {
                        format!("{} {}", line_number, token_display)
                    }
                };
        
                if debug {
                    writeln!(writer, "{}", template_string)
                        .expect("Failed to write error to stdout!");
                }
                tokens.push(token);
            }
        

            Err(token_value) => {
                found_err = true;
                let template_string = format!(
                    "Error in \"{}\" (line {}, column {})→\t{}",
                    filename, current_line, current_col, token_value
                );
                if debug {
                    writeln!(writer, "{}", template_string)
                        .expect("Failed to write error to stdout!");
                }
            }
        }
    }

    if found_err {
        process::exit(1);
    }

    tokens
}
