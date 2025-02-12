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
Take a Decaf program as an &str. Convert it into a vector of 
tokens to be used by the parser.
*/

enum Token {
    Keyword(Keyword),    // Keyword variant has type Keyword
    Identifier(String),
    Symbol(Symbol),
    Literal(Literal),
    EOF
}

enum Keyword {
    If, Bool, Break, Import, Continue, Else, False, For, 
    While, Int, Long, Return, Len, True, Void,
}

enum Symbol {
    Operator(Operator),
    Punctuation(Punctuation)
}

enum Operator {
    Plus, Minus, Multiply, Divide, Modulo,
    Assign, PlusAssign, MinusAssign, MultiplyAssign, DivideAssign, ModuloAssign,
    Increment, Decrement,
    Equal, NotEqual, Less, Greater, LessEqual, GreaterEqual,
    LogicalAnd, LogicalOr, LogicalNot,
}

enum Punctuation {
    LeftParen, RightParen, LeftBrace, RightBrace, LeftBracket, RightBracket,
    Semicolon, Comma,
}

enum Literal {
    Int(i32), Long(i64), Bool(bool), Char(char), String(String),
}

enum ScanError {
    Standard(String), Special(String),
}


struct Scanner {
    // Program will be consumed until the vector is empty
    program: Vec<char>,
}

impl Scanner {
    fn new(input: &str) -> Self {
        Self {
            program: input.chars().collect(), // converts into a vector of characters
        }
    }

    fn is_eof(&self) -> bool {
        self.program.len() == 0
    }

    // Option type is: Some<char> or None<char>
    fn peek(&self) -> Option<char> {
        self.program.get(0).copied() // obtain ownership with copied
    }

    fn take_while(&self, pattern_fn: fn(char) -> bool) -> Vec<char> {
        self.program
            .iter() 
            .take_while(|&&c| pattern_fn(c)) 
            .cloned()
            .collect()
    }

    fn consume(&mut self, ntoken: i32) {
        let ntoken = ntoken.max(0) as usize;
        self.program = self.program.split_off(ntoken.min(self.program.len()));
    }


    fn symbol_to_token(symbol: &str) -> Option<Token> {
        match symbol {
            "==" => Some(Token::Symbol(Symbol::Operator(Operator::Equal))),
            "+" => Some(Token::Symbol(Symbol::Operator(Operator::Plus))),
            "-" => Some(Token::Symbol(Symbol::Operator(Operator::Minus))),
            "*" => Some(Token::Symbol(Symbol::Operator(Operator::Multiply))),
            "/" => Some(Token::Symbol(Symbol::Operator(Operator::Divide))),
            "%" => Some(Token::Symbol(Symbol::Operator(Operator::Modulo))),
            "(" => Some(Token::Symbol(Symbol::Punctuation(Punctuation::LeftParen))),
            ")" => Some(Token::Symbol(Symbol::Punctuation(Punctuation::RightParen))),
            "{" => Some(Token::Symbol(Symbol::Punctuation(Punctuation::LeftBrace))),
            "}" => Some(Token::Symbol(Symbol::Punctuation(Punctuation::RightBrace))),
            "," => Some(Token::Symbol(Symbol::Punctuation(Punctuation::Comma))),
            ";" => Some(Token::Symbol(Symbol::Punctuation(Punctuation::Semicolon))),
            _ => None,
        }
    }
    fn keyword_to_token(keyword: &str) -> Option<Token> {
        match keyword {
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
            _ => None, 
        }
    }
    



    // fn lex(&self) -> Vec<Token> {

    // }
    


}


/*
 Input: a Decaf source file String as input. 

 Effects: Outputs a sequence of tokens. 
*/
pub fn scan(file: &str) -> Vec<u8>{
    // careful about comments!
    println!("SCANNING");
    println!("Input is {}\n", file);

    let scanner = Scanner::new(file);

    println!("Nice {:?}", scanner.program);

    let output = vec![];
    output
}