/*
Scanner.
*/

use std::{fmt::Write, process};

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
pub struct TokenInfo {
    pub token: Token,
    pub display: String,
    pub line: i32,
    pub col: i32

}
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
    Int(String),
    Long(String),
    HexInt(String),
    HexLong(String),
    String(String),
    Bool(bool),
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

/*
Attempts to gobble a comment.
Throws an error if a block comment is not closed.
*/
fn gobble_comment(
    program: &mut Vec<char>,
    current_col: &mut i32,
    current_line: &mut i32,
) -> Result<String> {
    if let Some((&char1, &char2)) = program.get(0).zip(program.get(1)) {
        consume(program, current_col, 2);

        match (char1, char2) {
            ('/', '/') => {
                // Line comment
                while let Some(&c) = program.first() {
                    consume(program, current_col, 1);
                    if c == '\n' {
                        *current_line += 1;
                        *current_col = 1;
                        break;
                    }
                }
                Ok("Gobbled comment".to_string())
            }
            ('/', '*') => {
                // Block comment
                while let Some(&c) = program.first() {
                    consume(program, current_col, 1);
                    // pattern matching with &'\\'!!!
                    if c == '*' && program.first() == Some(&'/') {
                        consume(program, current_col, 1);
                        return Ok("Gobbled comment".to_string());
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


/* 
Typically for literals, we continue matching until a termination character: 
(';' | ',' | ')' | ']' | ' ' | '(' | '\n')
*/
fn lex_numeric_literal(program: &mut Vec<char>, current_line: &mut i32, current_col: &mut i32) -> Result<TokenInfo> {
    // Allow: Hex (0x), normal long
    // Termination at: (;), (,), (L), ()), (])
    let mut nliteral = String::new();

    if let Some(&char1) = program.get(0) {
        if let Some(&char2) = program.get(1) {
            match (char1, char2) {
                // Hex evaluation
                ('0', 'x') => {
                    consume(program, current_col, 2); // Consume "0x"

                    while let Some(&c) = program.get(0) {
                        match c {
                            '0'..='9' | 'a'..='f' | 'A'..='F' => {
                                nliteral.push(c);
                                consume(program, current_col, 1);
                            }
                            'L' => {
                                consume(program, current_col, 1);
                                return Ok( TokenInfo { token : Token::Literal(Literal::HexLong(nliteral.clone())), display: nliteral, line: *current_line, col: *current_col });
                            }
                            // ';' | ',' | ')' | ']' | ' ' | '\n' => {
                            _ => {
                                return Ok( TokenInfo { token : Token::Literal(Literal::HexInt(nliteral.clone())), display: nliteral, line: *current_line, col: *current_col });
                            }
                            // _ => {
                            //     return Err(anyhow!("Illegal char in hex literal!"));
                            // }
                        }
                    }
                    return Err(anyhow!("Unexpected end of hex literal"));
                }
                _ => {}
            }
        }

        // Decimal evaluation
        while let Some(&c) = program.get(0) {
            match c {
                '0'..='9' => {
                    nliteral.push(c);
                    consume(program, current_col, 1);
                }
                'L' => {
                    consume(program, current_col, 1);
                    return Ok( TokenInfo { token : Token::Literal(Literal::Long(nliteral.clone())), display: nliteral, line: *current_line, col: *current_col });
                }

                // Can be terminated by punctuation or by an identifier (e.g., 24value1)
                // ';' | ',' | ')' | ']' | ' ' | '}'| '\n' | 'a'..='z' | 'A'..='K' | 'M'..='Z' => {

                // No special termination characters: just end as soon as we stop matching
                _ => {
                    return Ok( TokenInfo { token : Token::Literal(Literal::Int(nliteral.clone())), display: nliteral, line: *current_line, col: *current_col });
                }
                // _ => {
                //     return Err(anyhow!("Illegal char in decimal literal!"));
                // }
            }
        }
    }

    Err(anyhow!("Unexpected end of numeric literal"))
}

/*
Lex a Keyword or Identifier.
*/
fn lex_keyword_or_identifier(program: &mut Vec<char>, current_line: &mut i32, current_col: &mut i32) -> Result<TokenInfo> {
    let mut keyword_or_identifier = String::new();

    // Build the keyword or identifier as the longest string of alphanumerics or underscores
    while let Some(&character) = program.get(0) {
        if character.is_alphanumeric() || character == '_' {
            keyword_or_identifier.push(character);
            consume(program, current_col, 1);

        } else {
            break;
        }
        // match character {
        //     ';' | ',' | ')' | '[' | ']' | ' ' | '(' | '\n'  => {
                
        //         break;
        //     }
        //     _ => {
        //         keyword_or_identifier.push(character);
        //         consume(program, current_col, 1); // Consume each character inside the string
        //     }
        // }
    }

    match keyword_or_identifier.as_str() {
        "if" => Ok(TokenInfo { token: Token::Keyword(Keyword::If), display: "if".to_string(), line: *current_line, col: *current_col }),
        "bool" => Ok(TokenInfo { token: Token::Keyword(Keyword::Bool), display: "bool".to_string(), line: *current_line, col: *current_col }),
        "break" => Ok(TokenInfo { token: Token::Keyword(Keyword::Break), display: "break".to_string(), line: *current_line, col: *current_col }),
        "import" => Ok(TokenInfo { token: Token::Keyword(Keyword::Import), display: "import".to_string(), line: *current_line, col: *current_col }),
        "continue" => Ok(TokenInfo { token: Token::Keyword(Keyword::Continue), display: "continue".to_string(), line: *current_line, col: *current_col }),
        "else" => Ok(TokenInfo { token: Token::Keyword(Keyword::Else), display: "else".to_string(), line: *current_line, col: *current_col }),
        "for" => Ok(TokenInfo { token: Token::Keyword(Keyword::For), display: "for".to_string(), line: *current_line, col: *current_col }),
        "while" => Ok(TokenInfo { token: Token::Keyword(Keyword::While), display: "while".to_string(), line: *current_line, col: *current_col }),
        "int" => Ok(TokenInfo { token: Token::Keyword(Keyword::Int), display: "int".to_string(), line: *current_line, col: *current_col }),
        "long" => Ok(TokenInfo { token: Token::Keyword(Keyword::Long), display: "long".to_string(), line: *current_line, col: *current_col }),
        "return" => Ok(TokenInfo { token: Token::Keyword(Keyword::Return), display: "return".to_string(), line: *current_line, col: *current_col }),
        "len" => Ok(TokenInfo { token: Token::Keyword(Keyword::Len), display: "len".to_string(), line: *current_line, col: *current_col }),
        "void" => Ok(TokenInfo { token: Token::Keyword(Keyword::Void), display: "void".to_string(), line: *current_line, col: *current_col }),
        
        // "false" and "true" are keywords, but lexed as boolean literals
        "false" => Ok(TokenInfo { token: Token::Literal(Literal::Bool(false)), display: "false".to_string()  ,line: *current_line, col: *current_col }),
        "true" => Ok(TokenInfo { token: Token::Literal(Literal::Bool(true)), display: "true".to_string()  ,line: *current_line, col: *current_col }),

        // If the token doesn't match any keywords, return it as an identifier
        _ => Ok(TokenInfo { token: Token::Identifier(keyword_or_identifier.clone()), display: keyword_or_identifier, line: *current_line, col: *current_col }),
    }
}
    


/*
Lex a string literal, identified by a leading quotation mark.
Returns errors for incomplete strings and illegal characters.
*/
fn lex_string_literal(program: &mut Vec<char>, current_line: &mut i32, current_col: &mut i32) -> Result<TokenInfo> {
    let mut sliteral = String::new();
    consume(program, current_col, 1);

    while let Some(&char1) = program.get(0) {
        // If we encounter an actual newline (0xA == '\n'), reject immediately (newline char "\n" is ok)
        if char1 == '\n' {
            return Err(anyhow!("Newline (0xA) not allowed in string literals"));
        }

        consume(program, current_col, 1);

        match char1 {
            '"' => return Ok( TokenInfo { token : Token::Literal(Literal::String(sliteral.clone())), display: sliteral, line: *current_line, col: *current_col }),
            '\'' => return Err(anyhow!("Illegal single quote in string literal")),
            '\\' => match program.get(0) {
                Some(&char2 @ ('"' | '\'' | '\\' | 't' | 'n' | 'r' | 'f')) => {
                    sliteral.push_str(match char2 {
                        // must also pushing leading backslash
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
                Some(&invalid) => return Err(anyhow!("Invalid escape sequence \'\\{}\'", invalid)), // Reject invalid backslashes
                None => return Err(anyhow!("Incomplete escape sequence in string literal")),
            },
            _ => sliteral.push(char1),
        }
    }

    Err(anyhow!("String literal missing closing quote"))
}

/*
Lex a character literal, identified by a leading single quote.
Returns errors for incomplete or illegal characters.
*/
fn lex_char_literal(program: &mut Vec<char>, current_line: &mut i32, current_col: &mut i32) -> Result<TokenInfo> {
    consume(program, current_col, 1); // Consume opening quote (')

    let char1 = match program.get(0) {
        Some(&c) => c,
        None => {
            return Err(anyhow!("Unexpected end of input in character literal"));
        }
    };

    consume(program, current_col, 1); // Move to next character

    // Match length-2 escaped sequences
    let char_value = if char1 == '\\' {
        match program.get(0) {
            Some(&escaped_char) => {
                consume(program, current_col, 1);
                match escaped_char {
                    '\'' => '\'',
                    '"'  => '"',
                    '\\' => '\\',
                    't'  => '\t',
                    'n'  => '\n',
                    'r'  => '\r',
                    'f'  => '\x0C',  // Form feed
                    '0'  => '\0',    // Null character
                    _    => {
                        return Err(anyhow!("Invalid escape sequence \\{}", escaped_char));
                    }, // Return as is for unrecognized escapes
                }
            },
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
            Ok( TokenInfo { token : Token::Literal(Literal::Char(char_value)), display: char_value.to_string(), line: *current_line, col: *current_col })

        }
        _ => Err(anyhow!("Character literal missing closing quote")),
    }
}

// does the matching and shii
fn get_next_token(program: &mut Vec<char>, current_line: &mut i32, current_col: &mut i32) -> Result<TokenInfo> {
    if let Some(&char1) = program.get(0) {
        // Attempt to match length-2 symbols first
        if let Some(&char2) = program.get(1) {
            let token = match (char1, char2) {
                ('=', '=') => Some(TokenInfo { token: Token::Symbol(Symbol::Operator(Operator::Equal)), display: "==".to_string(), line: *current_line, col: *current_col }),
                ('+', '+') => Some(TokenInfo { token: Token::Symbol(Symbol::Operator(Operator::Increment)), display: "++".to_string(), line: *current_line, col: *current_col }),
                ('-', '-') => Some(TokenInfo { token: Token::Symbol(Symbol::Operator(Operator::Decrement)), display: "--".to_string(), line: *current_line, col: *current_col }),
                ('!', '=') => Some(TokenInfo { token: Token::Symbol(Symbol::Operator(Operator::NotEqual)), display: "!=".to_string(), line: *current_line, col: *current_col }),
                ('<', '=') => Some(TokenInfo { token: Token::Symbol(Symbol::Operator(Operator::LessEqual)), display: "<=".to_string(), line: *current_line, col: *current_col }),
                ('>', '=') => Some(TokenInfo { token: Token::Symbol(Symbol::Operator(Operator::GreaterEqual)), display: ">=".to_string(), line: *current_line, col: *current_col }),
                ('&', '&') => Some(TokenInfo { token: Token::Symbol(Symbol::Operator(Operator::LogicalAnd)), display: "&&".to_string(), line: *current_line, col: *current_col }),
                ('|', '|') => Some(TokenInfo { token: Token::Symbol(Symbol::Operator(Operator::LogicalOr)), display: "||".to_string(), line: *current_line, col: *current_col }),
                ('+', '=') => Some(TokenInfo { token: Token::Symbol(Symbol::Operator(Operator::PlusAssign)), display: "+=".to_string(), line: *current_line, col: *current_col }),
                ('-', '=') => Some(TokenInfo { token: Token::Symbol(Symbol::Operator(Operator::MinusAssign)), display: "-=".to_string(), line: *current_line, col: *current_col }),
                ('*', '=') => Some(TokenInfo { token: Token::Symbol(Symbol::Operator(Operator::MultiplyAssign)), display: "*=".to_string(), line: *current_line, col: *current_col }),
                ('/', '=') => Some(TokenInfo { token: Token::Symbol(Symbol::Operator(Operator::DivideAssign)), display: "/=".to_string(), line: *current_line, col: *current_col }),
                ('%', '=') => Some(TokenInfo { token: Token::Symbol(Symbol::Operator(Operator::ModuloAssign)), display: "%=".to_string(), line: *current_line, col: *current_col }),
                _ => None,
            };
            if token.is_some() {
                consume(program, current_col, 2);
                return Ok(token.unwrap());
            }
        };

        // println!("current program: {:?}", program);
        // Match single-character Symbols directly
        let token = match char1 {
            '(' => Some(TokenInfo { token: Token::Symbol(Symbol::Punctuation(Punctuation::LeftParen)), display: '('.to_string(), line: *current_line, col: *current_col }),
            ')' => Some(TokenInfo { token: Token::Symbol(Symbol::Punctuation(Punctuation::RightParen)), display: ')'.to_string(), line: *current_line, col: *current_col }),
            '{' => Some(TokenInfo { token: Token::Symbol(Symbol::Punctuation(Punctuation::LeftBrace)), display: '{'.to_string(), line: *current_line, col: *current_col }),
            '}' => Some(TokenInfo { token: Token::Symbol(Symbol::Punctuation(Punctuation::RightBrace)), display: '}'.to_string(), line: *current_line, col: *current_col }),
            '[' => Some(TokenInfo { token: Token::Symbol(Symbol::Punctuation(Punctuation::LeftBracket)), display: '['.to_string(), line: *current_line, col: *current_col }),
            ']' => Some(TokenInfo { token: Token::Symbol(Symbol::Punctuation(Punctuation::RightBracket)), display: ']'.to_string(), line: *current_line, col: *current_col }),
            ';' => Some(TokenInfo { token: Token::Symbol(Symbol::Punctuation(Punctuation::Semicolon)), display: ';'.to_string(), line: *current_line, col: *current_col }),
            ',' => Some(TokenInfo { token: Token::Symbol(Symbol::Punctuation(Punctuation::Comma)), display: ','.to_string(), line: *current_line, col: *current_col }),
    
            '=' => Some(TokenInfo { token: Token::Symbol(Symbol::Operator(Operator::Assign)), display: '='.to_string(), line: *current_line, col: *current_col }),
            '+' => Some(TokenInfo { token: Token::Symbol(Symbol::Operator(Operator::Plus)), display: '+'.to_string(), line: *current_line, col: *current_col }),
            '-' => Some(TokenInfo { token: Token::Symbol(Symbol::Operator(Operator::Minus)), display: '-'.to_string(), line: *current_line, col: *current_col }),
            '*' => Some(TokenInfo { token: Token::Symbol(Symbol::Operator(Operator::Multiply)), display: '*'.to_string(), line: *current_line, col: *current_col }),
            '/' => Some(TokenInfo { token: Token::Symbol(Symbol::Operator(Operator::Divide)), display: '/'.to_string(), line: *current_line, col: *current_col }),
            '%' => Some(TokenInfo { token: Token::Symbol(Symbol::Operator(Operator::Modulo)), display: '%'.to_string(), line: *current_line, col: *current_col }),
            '<' => Some(TokenInfo { token: Token::Symbol(Symbol::Operator(Operator::Less)), display: '<'.to_string(), line: *current_line, col: *current_col }),
            '>' => Some(TokenInfo { token: Token::Symbol(Symbol::Operator(Operator::Greater)), display: '>'.to_string(), line: *current_line, col: *current_col }),
            '!' => Some(TokenInfo { token: Token::Symbol(Symbol::Operator(Operator::LogicalNot)), display: '!'.to_string(), line: *current_line, col: *current_col }),
        

            // These fn handle their own consumption and return immediately
            '\'' => return lex_char_literal(program, current_line, current_col),
            '"' => return lex_string_literal(program, current_line, current_col),

            // Fallback: check for Keyword, Identifier, Literal
            _ => {
                if char1.is_alphabetic() || char1 == '_' {
                    return lex_keyword_or_identifier(program, current_line, current_col);
                } else if char1.is_numeric() {
                    return lex_numeric_literal(program, current_line, current_col);
                } else {
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

/*
The main scan function for this file.
    Input: a Decaf source file String as input.
    Effects: Outputs a sequence of Tokens.
*/
pub fn scan(file: &str, filename: &str, writer: &mut Box<dyn std::io::Write>) -> Vec<Token> {
    let mut program: Vec<char> = file.chars().collect();
    let mut tokens: Vec<Token> = vec![];
    let mut found_err = false;

    // Keep track of line and column for error-handling purposes
    let (mut current_line, mut current_col) = (1, 1);

    // Clear whitespace and comments; gobble tokens until hit EOF
    while let Some(&char1) = program.get(0) {
        // Clear any whitespace
        if char1.is_whitespace() {
            gobble_whitespace(&mut program, &mut current_line, &mut current_col);
            continue;
        }

        // Gobble any comment
        if char1 == '/' {
            if let Some(&char2) = program.get(1) {
                match (char1, char2) {
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

        // Get and display the next token
        let token = get_next_token(&mut program, &mut current_line, &mut current_col);
        match token {
            Ok(token_info) => {
                let template_string = match &token_info.token {
                    Token::Identifier(text) => {
                        format!("{} IDENTIFIER {}", token_info.line, text)
                    }
                    Token::Literal(Literal::Char(text)) => {
                        let display_char: String = match text {
                            '\n' => "\\n".to_string(),
                            '\t' => "\\t".to_string(),
                            '\r' => "\\r".to_string(),
                            '\x0C' => "\\f".to_string(),
                            '\'' => "\\'".to_string(),
                            '"' => "\\\"".to_string(),
                            '\\' => "\\\\".to_string(),
                            _ => format!("{}", text)
                        };
                        format!("{} CHARLITERAL \'{}\'", token_info.line, display_char)
                    }
                    Token::Literal(Literal::String(text)) => {
                        format!("{} STRINGLITERAL \"{}\"", token_info.line, text)
                    }
                    Token::Literal(Literal::Int(value)) => {
                        format!("{} INTLITERAL {}", token_info.line, value)
                    }
                    Token::Literal(Literal::Long(value)) => {
                        format!("{} LONGLITERAL {}L", token_info.line, value)
                    }
                    Token::Literal(Literal::HexInt(value)) => {
                        format!("{} INTLITERAL 0x{}", token_info.line, value)
                    }
                    Token::Literal(Literal::HexLong(value)) => {
                        format!("{} LONGLITERAL 0x{}L", token_info.line, value)
                    }
                    Token::Literal(Literal::Bool(value)) => {
                        format!("{} BOOLEANLITERAL {}", token_info.line, value)
                    }
                    _ => {
                        format!("{} {}", token_info.line, token_info.display)
                    }
                };

                writeln!(writer, "{}", template_string).expect("Failed to write error to stdout!");
                tokens.push(token_info.token);
            }
            Err(token_value) => {
                found_err = true;
                let template_string = format!("Error in \"{}\" (line {}, column {})\t→\t{}",
                    filename, current_line, current_col, token_value);
                    writeln!(writer, "{}", template_string).expect("Failed to write error to stdout!");
            }
        }
    }

    if found_err {
        process::exit(1);
    }

    // println!("Tokens are:\n {:?}", tokens);
    tokens
}
