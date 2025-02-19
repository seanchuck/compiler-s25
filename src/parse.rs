/*
Scanner.

Idea: Use the nom parser combinator to build small parsers and 
gradually compose them with `alt` (OR) or '' (AND) to parse 
the entire program. Each parser returns An IResult, which either
has an error, or the parsed token and list of remaining tokens, 
if the parse was succesful.

We will mimic the grammar with the way we build up parsers 
with more complex (composed) parsers further down the file.
However, some productions in the grammar don't need to be explicitly 
defined as they are covered by the other parsers.
(e.g., alpha or digit are just covered by literals).

nom's IResult types either return 
    - succesful parse : Ok(rest_tokens, AST)
    - unsuccesfful parse : Err

Each parser follows the same pattern:
    1. attempt to match something
    2. either build AST or throw error based on match result
    
Big game of matching.
A parser can be seen as an "attempt" to match.
*/

use crate::ast::*;
use crate::token::*;
use crate::scan::scan;

use nom::multi::many0;
use nom::{combinator::{map, opt, value}, multi::{fold_many1, separated_list1}, branch::alt, error::Error, sequence::{delimited}, IResult, Parser, bytes::complete::tag};



// The most basic parsers are tags, which match a simple pattern
// Each parser follows th

// -----------------------------------------------------------------------------------------------------
// These generators are great because we can use them to individually 
// match any grouping of operator, punctuation, identifier, or keyword.

/// Generates a parser that matches a specific `Operator`
fn tag_operator_gen(expected_token: Operator) -> impl Fn(&[Token]) -> IResult<&[Token], Operator> {
    move |input: &[Token]| match input.split_first() {
        // succeeds only if we have match type and token
        Some((Token::Symbol(Symbol::Operator(first_token)), rest)) if *first_token == expected_token => Ok((rest, first_token.clone())),
        _ => Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Tag))),
    }
}

/// Generates a parser that matches a specific `Punctuation`
fn tag_punctuation_gen(expected_token: Punctuation) -> impl Fn(&[Token]) -> IResult<&[Token], ()> {
    move |input: &[Token]| match input.first() {
        Some(Token::Symbol(Symbol::Punctuation(first_token))) if *first_token == expected_token => Ok((&input[1..], ())),
        _ => Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Tag))),
    }
}

/// Generates a parser that matches a specific `Keyword`
fn tag_keyword_gen(expected_token: Keyword) -> impl Fn(&[Token]) -> IResult<&[Token], Keyword> {
    move |input: &[Token]| match input.split_first() {
        Some((Token::Keyword(first_token), rest)) if *first_token == expected_token => Ok((rest, first_token.clone())),
        _ => Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Tag))),
    }
}

// -----------------------------------------------------------------------------------------------------

/// Parses an identifier.
fn parse_identifier(input: &[Token]) -> IResult<&[Token], AST> {
    if let Some(Token::Identifier(id)) = input.first() {
        Ok((&input[1..], AST::Identifier(id.clone())))
    } else {
        Err(nom::Err::Error(Error::new(
            input,
            nom::error::ErrorKind::Tag,
        )))
    }
}

fn parse_char_bool_literal(input: &[Token]) -> IResult<&[Token], AST> {
    match input.first() {
        Some(Token::Literal(Literal::Char(id))) => Ok((
            &input[1..],
            AST::Expr(Expr::Literal(Literal::Char(id.clone()))),
        )),
        Some(Token::Literal(Literal::Bool(id))) => Ok((
            &input[1..],
            AST::Expr(Expr::Literal(Literal::Bool(id.clone()))),
        )),
        _ => Err(nom::Err::Error(Error::new(
            input,
            nom::error::ErrorKind::Tag,
        ))),
    }
}

fn parse_long_literal(input: &[Token]) -> IResult<&[Token], AST> {
    match input.first() {
        Some(Token::Literal(Literal::Long(id))) => Ok((
            &input[1..],
            AST::Expr(Expr::Literal(Literal::Long(id.clone()))),
        )),
        Some(Token::Literal(Literal::HexLong(id))) => Ok((
            &input[1..],
            AST::Expr(Expr::Literal(Literal::HexLong(id.clone()))),
        )),
        _ => Err(nom::Err::Error(Error::new(
            input,
            nom::error::ErrorKind::Tag,
        ))),
    }
}

fn parse_integer_literal(input: &[Token]) -> IResult<&[Token], AST> {
    match input.first() {
        Some(Token::Literal(Literal::HexInt(id))) => Ok((
            &input[1..],
            AST::Expr(Expr::Literal(Literal::HexInt(id.clone()))),
        )),
        Some(Token::Literal(Literal::Int(id))) => Ok((
            &input[1..],
            AST::Expr(Expr::Literal(Literal::Int(id.clone()))),
        )),
        _ => Err(nom::Err::Error(Error::new(
            input,
            nom::error::ErrorKind::Tag,
        ))),
    }
}

fn parse_string_literal(input: &[Token]) -> IResult<&[Token], AST> {
    match input.first() {
        Some(Token::Literal(Literal::String(id))) => Ok((
            &input[1..],
            AST::Expr(Expr::Literal(Literal::String(id.clone()))),
        )),
        _ => Err(nom::Err::Error(Error::new(
            input,
            nom::error::ErrorKind::Tag,
        ))),
    }
}

/// Parses all literals but type string
/// For now, "-" parsed separately
fn parse_literal(input: &[Token]) -> IResult<&[Token], AST> {
    alt((
        parse_integer_literal,
        parse_long_literal,
        parse_char_bool_literal,
    )).parse(input)
}

fn parse_type(input: &[Token]) -> IResult<&[Token], AST> {
    match input.first() {
        Some(Token::Literal(Literal::String(id))) => {
            let ast_type = match id.as_str() {
                "int" => AST::Type(Type::Int),
                "long" => AST::Type(Type::Long),
                "bool" => AST::Type(Type::Bool),
                _ => return Err(nom::Err::Error(Error::new(input, nom::error::ErrorKind::Tag))),
            };
            Ok((&input[1..], ast_type))
        }
        _ => Err(nom::Err::Error(Error::new(
            input,
            nom::error::ErrorKind::Tag,
        ))),
    }
}


/// Parses a single binary operator token
fn parse_BinaryOp_token(input: &[Token]) -> IResult<&[Token], BinaryOp> {
    alt((
        // Matching order enforces `order of operations`
        map(tag_operator_gen(Operator::Multiply), |_| BinaryOp::Add),
        map(tag_operator_gen(Operator::Divide), |_| BinaryOp::Subtract),
        map(tag_operator_gen(Operator::Plus), |_| BinaryOp::Multiply),
        map(tag_operator_gen(Operator::Minus), |_| BinaryOp::Divide),
        map(tag_operator_gen(Operator::Modulo), |_| BinaryOp::Modulo),

        map(tag_operator_gen(Operator::Equal), |_| BinaryOp::Equal),
        map(tag_operator_gen(Operator::NotEqual), |_| BinaryOp::NotEqual),
        map(tag_operator_gen(Operator::Less), |_| BinaryOp::Less),
        map(tag_operator_gen(Operator::Greater), |_| BinaryOp::Greater),
        map(tag_operator_gen(Operator::LessEqual), |_| BinaryOp::LessEqual),
        map(tag_operator_gen(Operator::GreaterEqual), |_| BinaryOp::GreaterEqual),
        
        map(tag_operator_gen(Operator::LogicalAnd), |_| BinaryOp::And),
        map(tag_operator_gen(Operator::LogicalOr), |_| BinaryOp::Or),
    )).parse(input)
}

/// Parses a token of type UnaryOp
fn parse_UnaryOp_token(input: &[Token]) -> IResult<&[Token], UnaryOp> {
    alt((
        map(tag_operator_gen(Operator::Minus), |_| UnaryOp::Neg),
        map(tag_operator_gen(Operator::LogicalNot), |_| UnaryOp::Not),
    )).parse(input)
}

/// Parses a binary expression
/// binary expressions take the form: left_expression binop right_expression
fn parse_binexpr(input: &[Token]) -> IResult<&[Token], AST> {
    map(
        (parse_expr, parse_BinaryOp_token, parse_expr),
        |(lexpr, op, rexpr)| {
            AST::Expr(Expr::BinaryExpr {
                left: Box::new(lexpr),
                op,
                right: Box::new(rexpr),
            })
        },
    ).parse(input)
}

/// Parses a unary expression
/// unary expressions take the form: unaryOp
fn parse_unexpr(input: &[Token]) -> IResult<&[Token], AST> {
    map(
        // takes the form: [unary]
        (parse_UnaryOp_token, parse_expr),
        |(op, operand)| {
            AST::Expr(Expr::UnaryExpr {
                op,
                expr: Box::new(operand),
            })
        },
    ).parse(input)
}

fn parse_len(input: &[Token]) -> IResult<&[Token], AST> {
    let parse_result = ((
        tag_keyword_gen(Keyword::Len),
        tag_punctuation_gen(Punctuation::LeftParen),
        parse_identifier, // already has expected &[Token] input
        tag_punctuation_gen(Punctuation::RightParen),
    )).parse(input);

    match parse_result {
        Ok((input, (_, _, expr, _))) => {
            let id = match expr {
                AST::Identifier(name) => name,
                _ => return Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Tag))),
            };
            Ok((input, AST::Expr(Expr::Len { id })))
    
        }
        Err(e) => {
            Err(e)
        }
    }
}

fn parse_cast(input: &[Token]) -> IResult<&[Token], AST> {
    // Pattern match  of tokens: applies parsers sequentially
    let parse_result = ((
        alt((
            map(tag_keyword_gen(Keyword::Int), |_| Type::Int),
            map(tag_keyword_gen(Keyword::Long), |_| Type::Long),
        )),
        parse_parens
    ))
    .parse(input);

    match parse_result {
        Ok((input, (cast_type, expr))) => {
            // Successfully parsed, return the AST node
            Ok((input, AST::Expr(Expr::Cast { 
                target_type: cast_type, 
                expr: Box::new(expr) 
            })))
        }
        Err(e) => {
            eprintln!("Parse error in `parse_cast`: {:?}", e);
            Err(e)
        }
    }
}

fn parse_parens(input: &[Token]) -> IResult<&[Token], AST> {
    let parse_result = ((
        tag_punctuation_gen(Punctuation::LeftParen),
        parse_expr,
        tag_punctuation_gen(Punctuation::RightParen),
    ))
    .parse(input);

    match parse_result {
        Ok((input, (_, expr, _))) => {
            Ok((input, expr))
        }
        Err(e) => {
            eprintln!("Parse error in `parse_parens`: {:?}", e);
            Err(e)
        }
    }
}

fn parse_location(input: &[Token]) -> IResult<&[Token], AST> {
    let parse_result = ((
        parse_identifier, // Variable name
        opt(((
            tag_punctuation_gen(Punctuation::LeftBracket),
            parse_expr, // Array index expression
            tag_punctuation_gen(Punctuation::RightBracket),
        ))),
    ))
    .parse(input);

    match parse_result {
        Ok((input, (id, maybe_index))) => {
            let id = match id {
                AST::Identifier(name) => name,
                _ => return Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Tag))),
            };

            if let Some((_, index, _)) = maybe_index {
                // Array access
                Ok((input, AST::Expr(Expr::ArrAccess {
                    id,
                    index: Box::new(index),
                })))
            } else {
                // Variable access
                Ok((input, AST::Identifier(id)))
            }
        }
        Err(e) => Err(e),
    }
}


/// Parse an expression -- TODO complete
fn parse_expr(input: &[Token]) -> IResult<&[Token], AST> {
    alt((
        parse_location,
        parse_method_call,
        parse_literal, // don't need args for funcs that just take [Token]
        parse_cast, 
        parse_len,
        parse_binexpr,
        parse_cast,
        parse_parens
    )).parse(input)
}

fn parse_if(input: &[Token]) -> IResult<&[Token], AST> {
    // compose the parser
    let parse_result = ((
        tag_keyword_gen(Keyword::If),
        tag_punctuation_gen(Punctuation::LeftParen),
        parse_expr,
        tag_punctuation_gen(Punctuation::RightParen),
        parse_block,
        // optional else block
        opt((tag_keyword_gen(Keyword::Else), parse_block).map(|(_, block)| block)),
    )).parse(input);

    // collect only the important components of the parse and return the AST
    match parse_result {
        Ok((input, (_, _, condition, _, then_block, else_block))) => {
            Ok((input, AST::Statement(Statement::If {
                condition: Box::new(condition),
                then_block: Box::new(then_block),
                else_block: else_block.map(Box::new),
            })))
        }
        Err(e) => Err(e),
    }
}

fn parse_for_loop(input: &[Token]) -> IResult<&[Token], AST> {
    let parse_result = ((
        tag_keyword_gen(Keyword::For),
        tag_punctuation_gen(Punctuation::LeftParen),
        parse_identifier,
        tag_operator_gen(Operator::Assign),
        parse_expr,
        tag_punctuation_gen(Punctuation::Semicolon),
        parse_expr,
        tag_punctuation_gen(Punctuation::Semicolon),
        parse_expr,
        tag_punctuation_gen(Punctuation::RightParen),
        parse_block,
    )).parse(input);

    match parse_result {
        Ok((input, (_, _, id, _, init, _, cond, _, update, _, body))) => {
            let loop_var = match id {
                AST::Identifier(name) => name,
                _ => return Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Tag))),
            };
            Ok((input, AST::Statement(Statement::For {
                var: loop_var,
                init: Box::new(init),
                condition: Box::new(cond),
                update: Box::new(update),
                block: Box::new(body),
            })))
        }
        Err(e) => Err(e),
    }
}


fn parse_while_loop(input: &[Token]) -> IResult<&[Token], AST> {
    let parse_result = ((
        tag_punctuation_gen(Punctuation::LeftParen),
        parse_expr,
        tag_punctuation_gen(Punctuation::RightParen),
        parse_block
    )).parse(input);

    match parse_result {
        Ok((input, (_, expr, _, body))) => {
            Ok((input, AST::Statement(Statement::While { 
                // must be boxed as pointers since can are non-terminals
                condition: Box::new(expr),
                block: Box::new(body)
            })))
        },
        Err(e) => Err(e),
    }
}



fn parse_break_continue(input: &[Token]) -> IResult<&[Token], AST> {
    let parse_result = ((
        alt((
            tag_keyword_gen(Keyword::Break),
            tag_keyword_gen(Keyword::Continue),
        )),
        tag_punctuation_gen(Punctuation::Semicolon),
    ))
    .parse(input);

    match parse_result {
        Ok((input, _)) => Ok((input, AST::Statement(Statement::Break))),
        Err(e) => Err(e),
    }
}

fn parse_return(input: &[Token]) -> IResult<&[Token], AST> {
    let parse_result = ((
        tag_keyword_gen(Keyword::Return),
        opt(parse_expr),
        tag_punctuation_gen(Punctuation::Semicolon),
    ))
    .parse(input);

    match parse_result {
        Ok((input, (_, retval, _))) => Ok(
            (input, AST::Statement(Statement::Return 
                { 
                    expr: retval.map(Box::new)
                })
            )
        ),
        Err(e) => Err(e),
    }
}


/// Parse increment "++" which we convert into "+=1" in the AST
/// Follows the structure: location increment
fn parse_increment_decrement(input: &[Token]) -> IResult<&[Token], AST> {
    let parse_result = ((
        parse_identifier,
        alt((
            map(tag_operator_gen(Operator::Increment), |_| AssignOp::PlusAssign),
            map(tag_operator_gen(Operator::Decrement), |_| AssignOp::MinusAssign),
        )),
        tag_punctuation_gen(Punctuation::Semicolon),
    ))
    .parse(input);

    match parse_result {
        Ok((input, (id, op, _))) => {
            let id = match id {
                AST::Identifier(name) => name,
                _ => return Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Tag))),
            };

            Ok((input, AST::Statement(Statement::Assignment {
                location: Box::new(AST::Identifier(id)),
                expr: Box::new(AST::Expr(Expr::Literal(Literal::Int("1".to_string())))),
                op,
            })))
        }
        Err(e) => Err(e),
    }
}

fn parse_AssignOp_token(input: &[Token]) -> IResult<&[Token], AssignOp> {
    alt((
        map(tag_operator_gen(Operator::Assign), |_| AssignOp::Assign),
        map(tag_operator_gen(Operator::PlusAssign), |_| AssignOp::PlusAssign),
        map(tag_operator_gen(Operator::MinusAssign), |_| AssignOp::MinusAssign),
        map(tag_operator_gen(Operator::MultiplyAssign), |_| AssignOp::MultiplyAssign),
        map(tag_operator_gen(Operator::DivideAssign), |_| AssignOp::DivideAssign),
        map(tag_operator_gen(Operator::ModuloAssign), |_| AssignOp::ModuloAssign),
    ))
    .parse(input)
}

fn parse_assignexpr(input: &[Token]) -> IResult<&[Token], AST> {
    alt((
        parse_increment_decrement,
        map(
            (
                parse_AssignOp_token,
                parse_expr,
            ),
            |(op, expr)| {
                AST::Statement(Statement::Assignment {
                    location: Box::new(AST::Identifier("".to_string())),
                    expr: Box::new(expr),
                    op,
                })
            },
        ),
    ))
    .parse(input)
}

fn parse_assign_to_location(input: &[Token]) -> IResult<&[Token], AST> {
    let parse_result = ((
        parse_location,
        parse_assignexpr,
        tag_punctuation_gen(Punctuation::Semicolon),
    ))
    .parse(input);

    match parse_result {
        Ok((input, (loc, assign, _))) => {
            let location = match loc {
                AST::Identifier(name) => AST::Identifier(name),
                AST::Expr(Expr::ArrAccess { id, index }) => AST::Expr(Expr::ArrAccess { id, index }),
                _ => return Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Tag))),
            };

            let (expr, op) = match assign {
                AST::Statement(Statement::Assignment { expr, op, .. }) => (expr, op),
                _ => return Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Tag))),
            };

            Ok((
                input,
                AST::Statement(Statement::Assignment {
                    location: Box::new(location),
                    expr,
                    op,
                }),
            ))
        }

        Err(e) => Err(e),
    }
}

fn parse_for_update(input: &[Token]) -> IResult<&[Token], AST> {
    alt((
        parse_assign_to_location,
        parse_increment_decrement,
    )).parse(input)
}

fn parse_statement(input: &[Token]) -> IResult<&[Token], AST> {
    alt((
        // assigment sequence of two parsers; must manually map output here
        map(
            (parse_for_update, tag_punctuation_gen(Punctuation::Semicolon)),
            |(assign, _)| {
                match assign {
                    AST::Statement(Statement::Assignment { location, expr, op}) => {
                        AST::Statement(Statement::Assignment { location, expr, op })
                    },
                    _ => assign,
                }
            }
        ),
        parse_method_call,
        parse_if,
        parse_for_loop,
        parse_while_loop,
        parse_return,
        parse_break_continue,
    )).parse(input)
}


fn parse_import_decl(input: &[Token]) -> IResult<&[Token], AST> {
    let parse_result = ((
        tag_keyword_gen(Keyword::Import),
        parse_identifier,
        tag_punctuation_gen(Punctuation::Semicolon),
    ))
    .parse(input);

    match parse_result {
        Ok((input, (_, id, _))) => {
            let id = match id {
                AST::Identifier(name) => name,
                _ => return Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Tag))),
            };
            Ok((input, AST::ImportDecl { id }))
        }
        Err(e) => Err(e),
    }
}

fn parse_array_field_decl(input: &[Token]) -> IResult<&[Token], AST> {
    let parse_result = ((
        parse_identifier, 
        tag_punctuation_gen(Punctuation::LeftBracket),
        parse_integer_literal, 
        tag_punctuation_gen(Punctuation::RightBracket),
    ))
    .parse(input);

    match parse_result {
        Ok((input, (id, _, size, _))) => {
            let id = match id {
                AST::Identifier(name) => name,
                _ => return Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Tag))),
            };
            let size = match size {
                AST::Expr(Expr::Literal(Literal::Int(value))) => value,
                _ => return Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Tag))),
            };

            Ok((input, AST::ArrayFieldDecl { id, size }))
        }
        Err(e) => Err(e),
    }
}


// These all require parsing multiple appearances
fn parse_method_call(input: &[Token]) -> IResult<&[Token], AST> {
    todo!()
}

fn parse_method_decl(input: &[Token]) -> IResult<&[Token], AST> {
    todo!()
}

fn parse_field_decl(input: &[Token]) -> IResult<&[Token], AST> {
    todo!()
}

fn parse_param_list(input: &[Token]) -> IResult<&[Token], AST> {
    todo!()
}

fn parse_block(input: &[Token]) -> IResult<&[Token], AST> {
    todo!()
}

fn parse_program(input: &[Token]) -> IResult<&[Token], AST> {
    todo!()
}

/// Input: a sequence of tokens produced by the scanner.
/// Effects:
///    - Verifies that tokens conform to valid Decaf via the language specification
///    - Outputs a syntax tree representation of the Decaf program
pub fn parse(file: &str, filename: &str, writer: &mut Box<dyn std::io::Write>) {
    // use nom for parser
    // enum for AST
    println!("PARSING");
    let tokens: Vec<Token> = scan(file, filename, writer, false);
    println!("Tokens are {:?}\n", &tokens);

    let parse_result = parse_program(&tokens);
}
