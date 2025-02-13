/*
Scanner.
*/

// use anyhow::Result;
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
*/



#[derive(Debug)]
pub enum Token {
    Keyword(Keyword),    // Keyword variant has type Keyword
    Identifier(String),
    Symbol(Symbol),
    Literal(Literal),
    // EOF,
    ScanError(ScanError)
}

#[derive(Debug)]
pub enum Keyword {
    If, Bool, Break, Import, Continue, Else, False, For, 
    While, Int, Long, Return, Len, True, Void,
}

#[derive(Debug)]
pub enum Symbol {
    Operator(Operator),
    Punctuation(Punctuation)
}

#[derive(Debug)]
pub enum Operator {
    Plus, Minus, Multiply, Divide, Modulo,
    Assign, PlusAssign, MinusAssign, MultiplyAssign, DivideAssign, ModuloAssign,
    Increment, Decrement,
    Equal, NotEqual, Less, Greater, LessEqual, GreaterEqual,
    LogicalAnd, LogicalOr, LogicalNot,
}

#[derive(Debug)]
pub enum Punctuation {
    LeftParen, RightParen, LeftBrace, RightBrace, LeftBracket, RightBracket,
    Semicolon, Comma,
}
#[derive(Debug)]
pub enum Literal {
    Char(char), Int(i32), Long(i64), Bool(bool), String(String),
}


#[derive(Debug)]
#[allow(dead_code)]
pub enum ScanError {
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
/*
Consume up to nchar characters from the program vector.
*/
fn consume(program: &mut Vec<char>, nchar: i32) {
    let nchar = nchar.max(0) as usize;
    let nchar = nchar.min(program.len());
    program.drain(0..nchar);
}

/*
Consume all whitespace characters before the next
token or EOF.
*/
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

/*
Lex a Keyword or Identifier. 
*/
fn lex_keyword_or_identifier(program: &mut Vec<char>) -> Option<Token> {
    let mut keyword_or_identifier = String::new();

    while let Some(&character) = program.get(0) {

        // Bail out if we see whitespace or opening parens, signaling end of token
        match character {
            ' ' | '(' => { break; }
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


/*
Lex a string literal, identified by a leading quotation mark.
*/
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

fn lex_char_literal(program: &mut Vec<char>) -> Option<Token> {
    consume(program, 1);

    if let Some(&character) = program.get(0) {
        if program.len() > 1 && program[1] == '\'' {
            consume(program, 1);
            consume(program, 1);
            return Some(Token::Literal(Literal::Char(character)));
        }
    }

    Some(Token::ScanError(ScanError::standard("Character literal missing closing quote or invalid format")))
}


// does the matching and shii
fn get_next_token(program: &mut Vec<char>) -> Option<Token> {
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
                consume(program, 2);
                return token;
            }
        };

        // Match single-character Symbols directly
        let token = match char1 {
            '(' => Some(Token::Symbol(Symbol::Punctuation(Punctuation::LeftParen))),
            ')' => Some(Token::Symbol(Symbol::Punctuation(Punctuation::RightParen))),
            '{' => Some(Token::Symbol(Symbol::Punctuation(Punctuation::LeftBrace))),
            '}' => Some(Token::Symbol(Symbol::Punctuation(Punctuation::RightBrace))),
            '[' => Some(Token::Symbol(Symbol::Punctuation(Punctuation::LeftBracket))),
            ']' => Some(Token::Symbol(Symbol::Punctuation(Punctuation::RightBracket))),
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
            '\'' => return lex_char_literal(program),
            '"' => return lex_string_literal(program),
            
            // Fallback: check for Keyword, Identifier, Literal
            _ => return lex_keyword_or_identifier(program),
        };

        // Consume for direct matches only
        if token.is_some() { consume(program, 1); }
        token
    } else {
        None
    }
}


/*
The main scan function for this file.
    Input: a Decaf source file String as input. 
    Effects: Outputs a sequence of Tokens. 
*/
pub fn scan(file: &str) -> Vec<Token> {
    println!("SCANNING");
    let mut program: Vec<char> = file.chars().collect(); // convert into vector of characters
    let mut tokens: Vec<Token> = vec![];

    // Clear whitespace and gobble tokens until hit EOF
    while let Some(&character) = program.get(0) {
        if character.is_whitespace() {
            gobble_whitespace(&mut program);
            continue;
        }

        if let Some(token) = get_next_token(&mut program) {
            tokens.push(token);
        }
    }

    println!("Tokens are:\n {:?}", tokens);
    tokens
}
