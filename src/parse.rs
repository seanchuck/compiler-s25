/*
Parser.

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


----------ORDER OF OPERATIONS MATTERS---------------
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
fn tag_operator_gen(expected_token: Operator) -> impl Fn(TokenSlice) -> IResult<TokenSlice, Operator> {
    // Access inner slice explicitly with input.0
    move |input: TokenSlice| match input.0.split_first() {
        Some((Token::Symbol(Symbol::Operator(first_token)), rest)) if *first_token == expected_token => 
            Ok((TokenSlice(rest), first_token.clone())), // Wrap rest in TokenSlice
        _ => Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Tag))),
    }
}


/// Generates a parser that matches a specific `Punctuation`
fn tag_punctuation_gen(expected_token: Punctuation) -> impl Fn(TokenSlice) -> IResult<TokenSlice, ()> {
    move |input: TokenSlice| match input.first() {
        Some(Token::Symbol(Symbol::Punctuation(first_token))) if *first_token == expected_token => Ok((TokenSlice(&input.0[1..]), ())),
        _ => Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Tag))),
    }
}

/// Generates a parser that matches a specific `Keyword`
fn tag_keyword_gen(expected_token: Keyword) -> impl Fn(TokenSlice) -> IResult<TokenSlice, Keyword> {
    move |input: TokenSlice| match input.0.split_first() {
        Some((Token::Keyword(first_token), rest)) if *first_token == expected_token => 
            Ok((TokenSlice(rest), first_token.clone())),
        _ => Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Tag))),
    }
}

// #################################################
// 
// #################################################

/// Parses an identifier.
fn parse_identifier(input: TokenSlice) -> IResult<TokenSlice, AST> {
    // //println!("identifier now!!!");
    if let Some(Token::Identifier(id)) = input.0.first() {
        // //println!("chilll");
        // //println!("now woring with: {:?}", (TokenSlice(&input.0[1..])));
        Ok((TokenSlice(&input.0[1..]), AST::Identifier(id.clone())))
    } else {
        Err(nom::Err::Error(Error::new(
            input,
            nom::error::ErrorKind::Tag,
        )))
    }
}

fn parse_char_bool_literal(input: TokenSlice) -> IResult<TokenSlice, AST> {
    // Extract 
    match input.0.first() { 
        Some(Token::Literal(Literal::Char(id))) => Ok((
            TokenSlice(&input.0[1..]), // Wrap in TokenSlice
            AST::Expr(Expr::Literal(Literal::Char(*id))),
        )),
        Some(Token::Literal(Literal::Bool(id))) => Ok((
            TokenSlice(&input.0[1..]), // Wrap in TokenSlice
            AST::Expr(Expr::Literal(Literal::Bool(*id))),
        )),
        _ => Err(nom::Err::Error(Error::new(
            input,
            nom::error::ErrorKind::Tag,
        ))),
    }
}

fn parse_long_literal(input: TokenSlice) -> IResult<TokenSlice, AST> {
    match input.0.first() { // Access inner slice explicitly
        Some(Token::Literal(Literal::Long(id))) => Ok((
            TokenSlice(&input.0[1..]), // Wrap remaining tokens in TokenSlice
            AST::Expr(Expr::Literal(Literal::Long(id.clone()))),
        )),
        Some(Token::Literal(Literal::HexLong(id))) => Ok((
            TokenSlice(&input.0[1..]),
            AST::Expr(Expr::Literal(Literal::HexLong(id.clone()))),
        )),
        _ => Err(nom::Err::Error(Error::new(
            input,
            nom::error::ErrorKind::Tag,
        ))),
    }
}

fn parse_integer_literal(input: TokenSlice) -> IResult<TokenSlice, AST> {
    // //println!("ggimme that number");
    match input.0.first() {
        Some(Token::Literal(Literal::HexInt(id))) => Ok((
            TokenSlice(&input.0[1..]),
            AST::Expr(Expr::Literal(Literal::HexInt(id.clone()))),
        )),
        Some(Token::Literal(Literal::Int(id))) => Ok((
            TokenSlice(&input.0[1..]),
            AST::Expr(Expr::Literal(Literal::Int(id.clone()))),
        )),
        _ => Err(nom::Err::Error(Error::new(
            input,
            nom::error::ErrorKind::Tag,
        ))),
    }
}

fn parse_string_literal(input: TokenSlice) -> IResult<TokenSlice, AST> {
    match input.0.first() {
        Some(Token::Literal(Literal::String(id))) => Ok((
            TokenSlice(&input.0[1..]),
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
fn parse_literal(input: TokenSlice) -> IResult<TokenSlice, AST> {
    alt((
        parse_long_literal,
        parse_char_bool_literal,
        parse_integer_literal,
    )).parse(input)
}

fn parse_type(input: TokenSlice) -> IResult<TokenSlice, Type> {
    //println!("Parsing type: {:?}", input);
    //println!("Starting with: {:?}", input.0.first());

    match input.0.first() {
        Some(Token::Keyword(keyword)) => {
            let ast_type = match keyword {
                Keyword::Int => Type::Int,
                Keyword::Long => Type::Long,
                Keyword::Bool => Type::Bool,
                _ => return Err(nom::Err::Error(Error::new(input, nom::error::ErrorKind::Tag))),
            };
            //println!("Success parsing type: {:?}", ast_type);
            Ok((TokenSlice(&input.0[1..]), ast_type))
        }
        _ => {
            //println!("Failed to parse type");
            Err(nom::Err::Error(Error::new(input, nom::error::ErrorKind::Tag)))
        }
    }
}

// #################################################
// 
// #################################################

fn parse_expr(input: TokenSlice) -> IResult<TokenSlice, AST> {
    println!("parsing expr");
    // Parse in precedence order!
    alt((
        parse_binexpr,
        parse_unexpr,
        parse_cast, 
        parse_literal, // don't need args for funcs that just take [Token]
        parse_parens,
        parse_len,
        parse_cast,
        parse_location,
        parse_method_call,
        // parse_unexpr,
    )).parse(input)
}

/// Parses a token of type UnaryOp
fn parse_unaryop_token(input: TokenSlice) -> IResult<TokenSlice, UnaryOp> {
    alt((
        map(tag_operator_gen(Operator::Minus), |_| UnaryOp::Neg),
        map(tag_operator_gen(Operator::LogicalNot), |_| UnaryOp::Not),
    )).parse(input)
}

/// Parses a unary expression
/// unary expressions take the form: unaryOp
fn parse_unexpr(input: TokenSlice) -> IResult<TokenSlice, AST> {
    map(
        // takes the form: [unary]
        (parse_unaryop_token, parse_expr),
        |(op, operand)| {
            AST::Expr(Expr::UnaryExpr {
                op,
                expr: Box::new(operand),
            })
        },
    ).parse(input)
}

/// Parses a single binary operator token
fn parse_binaryop_token(input: TokenSlice) -> IResult<TokenSlice, BinaryOp> {
    alt((
        // Matching order enforces `order of operations` (6-level hierarchy):: see 4.7 in spec
        map(tag_operator_gen(Operator::Plus), |_| BinaryOp::Multiply),
        map(tag_operator_gen(Operator::Minus), |_| BinaryOp::Divide),
        map(tag_operator_gen(Operator::Modulo), |_| BinaryOp::Modulo),

        map(tag_operator_gen(Operator::Multiply), |_| BinaryOp::Add),
        map(tag_operator_gen(Operator::Divide), |_| BinaryOp::Subtract),

        map(tag_operator_gen(Operator::Less), |_| BinaryOp::Less),
        map(tag_operator_gen(Operator::Greater), |_| BinaryOp::Greater),
        map(tag_operator_gen(Operator::LessEqual), |_| BinaryOp::LessEqual),
        map(tag_operator_gen(Operator::GreaterEqual), |_| BinaryOp::GreaterEqual),
        
        map(tag_operator_gen(Operator::Equal), |_| BinaryOp::Equal),
        map(tag_operator_gen(Operator::NotEqual), |_| BinaryOp::NotEqual),

        map(tag_operator_gen(Operator::LogicalAnd), |_| BinaryOp::And),

        map(tag_operator_gen(Operator::LogicalOr), |_| BinaryOp::Or),
    )).parse(input)
}

/// Parses a binary expression
/// binary expressions take the form: left_expression binop right_expression
fn parse_binexpr(input: TokenSlice) -> IResult<TokenSlice, AST> {
    parse_add_expr.parse(input)
    // map(
    //     (parse_expr, parse_binaryop_token, parse_expr),
    //     |(lexpr, op, rexpr)| {
    //         AST::Expr(Expr::BinaryExpr {
    //             left: Box::new(lexpr),
    //             op,
    //             right: Box::new(rexpr),
    //         })
    //     },
    // ).parse(input)
}


/// Parses an additive expression (right-recursive)
fn parse_add_expr(input: TokenSlice) -> IResult<TokenSlice, AST> {
    let (input, left) = parse_mul_expr(input)?;
    parse_add_expr_tail(left, input)
}

fn parse_add_expr_tail(left: AST, input: TokenSlice) -> IResult<TokenSlice, AST> {
    if let Ok((input, op)) = parse_binaryop_token(input) {
        if matches!(op, BinaryOp::Add | BinaryOp::Subtract) {
            let (input, right) = parse_mul_expr(input)?;
            let new_ast = AST::Expr(Expr::BinaryExpr {
                left: Box::new(left),
                op,
                right: Box::new(right),
            });
            return parse_add_expr_tail(new_ast, input);
        }
    }
    Ok((input, left))
}


fn parse_mul_expr(input: TokenSlice) -> IResult<TokenSlice, AST> {
    let (input, left) = parse_unexpr(input)?;
    parse_mul_expr_tail(left, input)
}

fn parse_mul_expr_tail(left: AST, input: TokenSlice) -> IResult<TokenSlice, AST> {
    if let Ok((input, op)) = parse_binaryop_token(input) {
        if matches!(op, BinaryOp::Multiply | BinaryOp::Divide) {
            let (input, right) = parse_unexpr(input)?;
            let new_ast = AST::Expr(Expr::BinaryExpr {
                left: Box::new(left),
                op,
                right: Box::new(right),
            });
            return parse_mul_expr_tail(new_ast, input);
        }
    }
    Ok((input, left))
}

// #################################################
// 
// #################################################

fn parse_len(input: TokenSlice) -> IResult<TokenSlice, AST> {
    let parse_result = ((
        tag_keyword_gen(Keyword::Len),
        tag_punctuation_gen(Punctuation::LeftParen),
        parse_identifier, // already has expected TokenSlice input
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

fn parse_cast(input: TokenSlice) -> IResult<TokenSlice, AST> {
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
            //println!("Parse error in `parse_cast`: {:?}", e);
            Err(e)
        }
    }
}

fn parse_parens(input: TokenSlice) -> IResult<TokenSlice, AST> {
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
            //println!("Parse error in `parse_parens`: {:?}", e);
            Err(e)
        }
    }
}

fn parse_location(input: TokenSlice) -> IResult<TokenSlice, AST> {
    println!("parsing location");
    println!("loco giv4w: {:?}", input);
    let parse_result = ((
        parse_identifier, // Variable name
        opt((
            tag_punctuation_gen(Punctuation::LeftBracket),
            parse_expr, // Array index expression
            tag_punctuation_gen(Punctuation::RightBracket),
        )),
    ))
    .parse(input);

    match parse_result {
        Ok((input, (id, maybe_index))) => {
            let id = match id {
                AST::Identifier(name) => name,
                _ => return Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Tag))),
            };

            if let Some((_, index, _)) = maybe_index {
                println!("we boutta match e ");
                // Array access
                Ok((input, AST::Expr(Expr::ArrAccess {
                    id,
                    index: Box::new(index),
                })))
            } else {
                println!("we boutta match d");
                // Variable access
                Ok((input, AST::Identifier(id)))
            }
        }
        Err(e) => Err(e),
    }
}


// #################################################
// 
// #################################################

fn parse_if(input: TokenSlice) -> IResult<TokenSlice, AST> {
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

fn parse_for_loop(input: TokenSlice) -> IResult<TokenSlice, AST> {
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


fn parse_while_loop(input: TokenSlice) -> IResult<TokenSlice, AST> {
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



fn parse_break_continue(input: TokenSlice) -> IResult<TokenSlice, AST> {
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

fn parse_return(input: TokenSlice) -> IResult<TokenSlice, AST> {
    println!("yeppp");
    println!("before: {:?}", input);
    let parse_result = ((
        tag_keyword_gen(Keyword::Return),
        opt(parse_expr),
        tag_punctuation_gen(Punctuation::Semicolon),
    ))
    .parse(input);
    println!("afte: {:?}", input);

    println!("no goood");
    match parse_result {
        Ok((input, (_, retval, _))) => {
            println!("nice");
            Ok(
            (input, AST::Statement(Statement::Return 
                { 
                    expr: retval.map(Box::new)
                })
            )
        )},
        Err(e) => {
            println!("shooot");
            Err(e)},
    }
}


/// Parse increment "++" which we convert into "+=1" in the AST
/// Follows the structure: location increment
fn parse_increment_decrement(input: TokenSlice) -> IResult<TokenSlice, AST> {
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

fn parse_assignop_token(input: TokenSlice) -> IResult<TokenSlice, AssignOp> {
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

fn parse_assignexpr(input: TokenSlice) -> IResult<TokenSlice, AST> {
    alt((
        parse_increment_decrement,
        map(
            (
                parse_assignop_token,
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

fn parse_assign_to_location(input: TokenSlice) -> IResult<TokenSlice, AST> {
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

fn parse_for_update(input: TokenSlice) -> IResult<TokenSlice, AST> {
    alt((
        parse_assign_to_location,
        parse_increment_decrement,
    )).parse(input)
}

// #################################################
// 
// #################################################

fn parse_statement(input: TokenSlice) -> IResult<TokenSlice, AST> {
    println!("we here");
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


fn parse_import_decl(input: TokenSlice) -> IResult<TokenSlice, AST> {
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

fn parse_array_field_decl(input: TokenSlice) -> IResult<TokenSlice, AST> {
    //println!("WE WANT THIS");
    let parse_result = ((
        parse_identifier, 
        tag_punctuation_gen(Punctuation::LeftBracket),
        parse_integer_literal, 
        tag_punctuation_gen(Punctuation::RightBracket),
    ))
    .parse(input);

    //println!("GOOBOOBOBOOBOB");
    // return Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Tag)));

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


fn parse_method_call(input: TokenSlice) -> IResult<TokenSlice, AST> {
    println!("we goottttttttttt");
    let (input, method_name) = parse_identifier(input)?;
    let (input, _) = tag_punctuation_gen(Punctuation::LeftParen)(input)?;
    let (input, args) = separated_list1(tag_punctuation_gen(Punctuation::Comma), parse_expr).parse(input)?;
    let (input, _) = tag_punctuation_gen(Punctuation::RightParen)(input)?;

    Ok((
        input,
        AST::Expr(Expr::MethodCall {
            method_name: match method_name {
                AST::Identifier(name) => name,
                _ => return Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Tag))),
            },
            args: args.into_iter().map(Box::new).collect(),
        }),
    ))
}


fn parse_block(input: TokenSlice) -> IResult<TokenSlice, AST> {
    let (input, _) = tag_punctuation_gen(Punctuation::LeftBrace)(input)?; // Match '{'
    println!("eee");
    println!("cirrentl got: {:?}", input);

    let (input, field_decls) = many0(parse_field_decl).parse(input)?; // Parse field declarations
    println!("wooaaaoo");
    println!("cirrentl got: {:?}", input);

    let (input, statements) = many0(parse_statement).parse(input)?; // Parse statements
    println!("bbbbb");
    println!("cirrentl got: {:?}", input);

    let (input, _) = tag_punctuation_gen(Punctuation::RightBrace)(input)?; // Match '}'

    println!(";;;");
    Ok((
        input,
        AST::Block {
            field_decls: field_decls.into_iter().map(Box::new).collect(), // Vec<Box<T>>
            statements: statements.into_iter().map(Box::new).collect(),   // Vec<Box<T>>
        },
    ))
}

fn parse_method_decl(input: TokenSlice) -> IResult<TokenSlice, AST> {
    println!("parsing method decl");
    let (input, return_type) = alt((
        // Keyword:: Void is our none return type
        map(tag_keyword_gen(Keyword::Void), |_| Type::Void),
        parse_type,
    )).parse(input)?;

    println!("a");
    let (input, method_name) = parse_identifier(input)?; // Parse method name
    let (input, _) = tag_punctuation_gen(Punctuation::LeftParen)(input)?; // Match '('
    let (input, params) = separated_list1(
        tag_punctuation_gen(Punctuation::Comma),
        map(
            (parse_type, parse_identifier),
            |(param_type, param_name)| {
                if let AST::Identifier(name) = param_name {
                    (param_type, name)
                } else {
                    panic!("Expected an identifier, found {:?}", param_name);
                }
            },
        ),
    ).parse(input)
    .unwrap_or((input, vec![])); // Allow empty parameter list

    println!("b");
    let (input, _) = tag_punctuation_gen(Punctuation::RightParen)(input)?; // Match '('
    println!("g");
    let (input, body) = parse_block(input)?; // Parse method body
    
    println!("c");
    Ok((
        input,
        AST::MethodDecl {
            return_type,
            name: match method_name {
                AST::Identifier(name) => name, // Extract identifier correctly
                _ => return Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Tag))),
            },
            params, // Correctly returns Vec<(Type, String)>
            block: Box::new(body), // Ensure body is boxed correctly
        },
    ))
    
}

fn parse_field_decl(input: TokenSlice) -> IResult<TokenSlice, AST> {
    //println!("Parsing field declaration...");

    // Step 1: Parse the type (int, long, bool)
    let (input, field_type) = parse_type(input)?;

    // Step 2: Parse at least one `{id} | {array_field_decl}`, comma-separated
    let (input, fields) = separated_list1(
        tag_punctuation_gen(Punctuation::Comma),
        parse_id_or_array_field_decl, // Parses either an identifier or an array field declaration
    ).parse(input)?;

    //println!("okeeke");
    //println!("samller hopefilly: {:?}", input);

    // Step 3: Match the required `;` at the end
    let (input, _) = tag_punctuation_gen(Punctuation::Semicolon)(input)?;

    //println!("Parsed field declaration successfully.");
    Ok((
        input,
        AST::FieldDecl {
            typ: field_type,
            decls: fields.into_iter().map(Box::new).collect(), // Wrap in `Box` to match AST format
        },
    ))
}



fn parse_id_or_array_field_decl(input: TokenSlice) -> IResult<TokenSlice, AST> {
    //println!("we in");
    alt((
        parse_array_field_decl,
        parse_identifier,
    )).parse(input)
}



fn parse_program(input: TokenSlice) -> IResult<TokenSlice, AST> {
    // Sequentially take in input slice and update it
    let (input, import_decls) = many0(parse_import_decl).parse(input)?;
    // println!("import decls are: {:?}\n", import_decls);
    let (input, field_decls) = many0(parse_field_decl).parse(input)?;
    //println!("field decls are: {:?}\n", field_decls);
    let (input, method_decls) = many0(parse_method_decl).parse(input)?;
    println!("method decls are: {:?}\n", method_decls);

    Ok((
        input,
        AST::Program { 
            imports: import_decls.into_iter().map(Box::new).collect(),
            fields: field_decls.into_iter().map(Box::new).collect(),
            methods: method_decls.into_iter().map(Box::new).collect()
        }
    ))

}

fn parse_always_fail(input: TokenSlice) -> IResult<TokenSlice, AST> {
    Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Tag)))
}


/// Input: a sequence of tokens produced by the scanner.
/// Effects:
///    - Verifies that tokens conform to valid Decaf via the language specification
///    - Outputs a syntax tree representation of the Decaf program
pub fn parse(file: &str, filename: &str, writer: &mut Box<dyn std::io::Write>, verbose: bool) {
    //println!("PARSING");
    let tokens: Vec<Token> = scan(file, filename, writer, false);

    let parse_result = parse_program(TokenSlice(&tokens));

    match parse_result {
        Ok((rest, parse_tree)) => {
            println!("TOKENS SHOULD BE EMPTY {:?}", rest); // if not empty, throw error
            println!("PARSE TREE: {:?}", parse_tree);
            if verbose {
                let template_string = format!(
                    "SUCCESSFUL PARSE==================\n"
                );
                writeln!(writer, "{}", template_string).expect("Failed to write error to stdout!");
            }
        },

        Err(parse_error) => {
            if verbose {
                let template_string = format!(
                    "ERROR PARSE===============\n"
                );
                writeln!(writer, "{}", template_string).expect("Failed to write error to stdout!");
            }
        }
    }
}
