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
        PRECEDENCE AND PUNCTUATION
*/

use std::fmt::Binary;

use crate::ast::*;
use crate::scan::scan;
use crate::token::*;

use nom::multi::{many0, separated_list0};
use nom::sequence::{pair, preceded};
use nom::{
    branch::alt,
    bytes::complete::tag,
    combinator::{map, opt, value},
    error::Error,
    multi::{fold_many1, separated_list1},
    sequence::delimited,
    IResult, Parser,
};

// The most basic parsers are tags, which match a simple pattern
// Each parser follows th

// -----------------------------------------------------------------------------------------------------
// These generators are great because we can use them to individually
// match any grouping of operator, punctuation, identifier, or keyword.

/// Generates a parser that matches a specific `Operator`
fn tag_operator_gen(
    expected_token: Operator,
) -> impl Fn(TokenSlice) -> IResult<TokenSlice, Operator> {
    // Access inner slice explicitly with input.0
    move |input: TokenSlice| match input.0.split_first() {
        Some((Token::Symbol(Symbol::Operator(first_token)), rest))
            if *first_token == expected_token =>
        {
            Ok((TokenSlice(rest), first_token.clone()))
        } // Wrap rest in TokenSlice
        _ => Err(nom::Err::Error(nom::error::Error::new(
            input,
            nom::error::ErrorKind::Tag,
        ))),
    }
}

/// Generates a parser that matches a specific `Punctuation`
fn tag_punctuation_gen(
    expected_token: Punctuation,
) -> impl Fn(TokenSlice) -> IResult<TokenSlice, ()> {
    move |input: TokenSlice| match input.first() {
        Some(Token::Symbol(Symbol::Punctuation(first_token))) if *first_token == expected_token => {
            Ok((TokenSlice(&input.0[1..]), ()))
        }
        _ => Err(nom::Err::Error(nom::error::Error::new(
            input,
            nom::error::ErrorKind::Tag,
        ))),
    }
}

/// Generates a parser that matches a specific `Keyword`
fn tag_keyword_gen(expected_token: Keyword) -> impl Fn(TokenSlice) -> IResult<TokenSlice, Keyword> {
    move |input: TokenSlice| match input.0.split_first() {
        Some((Token::Keyword(first_token), rest)) if *first_token == expected_token => {
            Ok((TokenSlice(rest), first_token.clone()))
        }
        _ => Err(nom::Err::Error(nom::error::Error::new(
            input,
            nom::error::ErrorKind::Tag,
        ))),
    }
}

// #################################################
//
// #################################################

/// Parses an identifier.
fn parse_identifier(input: TokenSlice) -> IResult<TokenSlice, AST> {
    if let Some(Token::Identifier(id)) = input.0.first() {
        println!("got identifier: {}", id);
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
    match input.0.first() {
        // Access inner slice explicitly
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
    println!("parse integer: {:?}\n\n", input);
    match input.0.first() {
        Some(Token::Literal(Literal::HexInt(id))) => {
            println!("hex integer");
            println!("{:?}", AST::Expr(Expr::Literal(Literal::HexInt(id.clone()))));
            println!("reaming: {:?}", TokenSlice(&input.0[1..]));
            Ok((
                TokenSlice(&input.0[1..]),
                AST::Expr(Expr::Literal(Literal::HexInt(id.clone()))),
            ))
        }
        Some(Token::Literal(Literal::Int(id))) => {
            println!("integer literal");
            println!("id is {}", id);
            println!("now we have: {:?}", TokenSlice(&input.0[1..]));
            Ok((
                TokenSlice(&input.0[1..]),
                AST::Expr(Expr::Literal(Literal::Int(id.clone()))),
            ))
        }
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
    println!("before parsing literal: {:?}\n", input);
    let ps = alt((
        parse_long_literal,
        parse_char_bool_literal,
        parse_integer_literal,
    ))
    .parse(input);

    println!("after parsing literal: {:?}\n", ps);
    ps
}

fn parse_type(input: TokenSlice) -> IResult<TokenSlice, Type> {
    match input.0.first() {
        Some(Token::Keyword(keyword)) => {
            let ast_type = match keyword {
                Keyword::Int => Type::Int,
                Keyword::Long => Type::Long,
                Keyword::Bool => Type::Bool,
                _ => {
                    return Err(nom::Err::Error(Error::new(
                        input,
                        nom::error::ErrorKind::Tag,
                    )))
                }
            };
            Ok((TokenSlice(&input.0[1..]), ast_type))
        }
        _ => Err(nom::Err::Error(Error::new(
            input,
            nom::error::ErrorKind::Tag,
        ))),
    }
}

// #################################################
//
// #################################################

// Hacked Grammar:
// expr     = AND ( "||" AND )*
// AND      = EQ  ( "&&" EQ )*
// EQ       = REL ( ("==" | "!=") REL )*
// REL      = ADD ( ("<" | "<=" | ">=" | ">") ADD )*
// ADD      = MUL ( ("+" | "-") MUL )*
// MUL      = UNARY ( ("*" | "/" | "%") UNARY )*
// UNARY    = ("-" | "!")? TYPE
// TYPE     = ( "int()" | "long()" )? PRIMARY

// PRIMARY  = IDENTIFIER
//          | LITERAL
//          | "(" expr ")"
//          | "len(" IDENTIFIER ")"
//          | location
//          | method_call

// location     = IDENTIFIER ("[" expr "]")?
// method_call = IDENTIFIER "(" (expr ("," expr)*)? ")"

/// Structure operator precedence to handle ambiguity

// fn parse_binaryop_token(input: TokenSlice) -> IResult<TokenSlice, BinaryOp> {
//     alt((
//         // Matching order enforces `order of operations` (6-level hierarchy):: see 4.7 in spec
//         map(tag_operator_gen(Operator::Plus), |_| BinaryOp::Multiply),
//         map(tag_operator_gen(Operator::Minus), |_| BinaryOp::Divide),
//         map(tag_operator_gen(Operator::Modulo), |_| BinaryOp::Modulo),
//         map(tag_operator_gen(Operator::Multiply), |_| BinaryOp::Add),
//         map(tag_operator_gen(Operator::Divide), |_| BinaryOp::Subtract),
//         map(tag_operator_gen(Operator::Less), |_| BinaryOp::Less),
//         map(tag_operator_gen(Operator::Greater), |_| BinaryOp::Greater),
//         map(tag_operator_gen(Operator::LessEqual), |_| {
//             BinaryOp::LessEqual
//         }),
//         map(tag_operator_gen(Operator::GreaterEqual), |_| {
//             BinaryOp::GreaterEqual
//         }),
//         map(tag_operator_gen(Operator::Equal), |_| BinaryOp::Equal),
//         map(tag_operator_gen(Operator::NotEqual), |_| BinaryOp::NotEqual),
//         map(tag_operator_gen(Operator::LogicalAnd), |_| BinaryOp::And),
//         map(tag_operator_gen(Operator::LogicalOr), |_| BinaryOp::Or),
//     ))
//     .parse(input)
// }

fn parse_expr(input: TokenSlice) -> IResult<TokenSlice, AST> {
    parse_or.parse(input)
}

fn parse_or(input: TokenSlice) -> IResult<TokenSlice, AST> {
    let (input, left) = parse_and(input)?;
    let (input, exprs) = many0(pair(
        map(tag_operator_gen(Operator::LogicalOr), |_| BinaryOp::Or),
        parse_and,
    ))
    .parse(input)?;

    let expr = exprs.into_iter().fold(left, |acc, (op, right)| {
        AST::Expr(Expr::BinaryExpr {
            op,
            left: Box::new(acc),
            right: Box::new(right),
        })
    });

    Ok((input, expr))
}


fn parse_and(input: TokenSlice) -> IResult<TokenSlice, AST> {
    let (input, left) = parse_eq(input)?;

    let (input, exprs) = many0(pair(
        map(tag_operator_gen(Operator::LogicalAnd), |_| BinaryOp::And),
        parse_eq,
    ))
    .parse(input)?;

    let expr = exprs.into_iter().fold(left, |acc, (op, right)| {
        AST::Expr(Expr::BinaryExpr {
            op,
            left: Box::new(acc),
            right: Box::new(right),
        })
    });

    Ok((input, expr))
}

fn parse_eq(input: TokenSlice) -> IResult<TokenSlice, AST> {
    let (input, left) = parse_rel(input)?;
    let (input, exprs) = many0(pair(
        map(
            alt((
                tag_operator_gen(Operator::Equal),
                tag_operator_gen(Operator::NotEqual),
            )),
            |op| match op {
                Operator::Equal => BinaryOp::Equal,
                Operator::NotEqual => BinaryOp::NotEqual,
                _ => unreachable!(),
            },
        ),
        parse_rel,
    ))
    .parse(input)?;

    let expr = exprs.into_iter().fold(left, |acc, (op, right)| {
        AST::Expr(Expr::BinaryExpr {
            op,
            left: Box::new(acc),
            right: Box::new(right),
        })
    });

    Ok((input, expr))
}

fn parse_rel(input: TokenSlice) -> IResult<TokenSlice, AST> {
    let (input, left) = parse_add(input)?;
    let (input, exprs) = many0(pair(
        map(
            alt((
                tag_operator_gen(Operator::Less),
                tag_operator_gen(Operator::LessEqual),
                tag_operator_gen(Operator::GreaterEqual),
                tag_operator_gen(Operator::Greater),
            )),
            |op| match op {
                Operator::Less => BinaryOp::Less,
                Operator::LessEqual => BinaryOp::LessEqual,
                Operator::GreaterEqual => BinaryOp::GreaterEqual,
                Operator::Greater => BinaryOp::Greater,
                _ => unreachable!(),
            },
        ),
        parse_add,
    ))
    .parse(input)?;

    let expr = exprs.into_iter().fold(left, |acc, (op, right)| {
        AST::Expr(Expr::BinaryExpr {
            op,
            left: Box::new(acc),
            right: Box::new(right),
        })
    });

    Ok((input, expr))
}

fn parse_add(input: TokenSlice) -> IResult<TokenSlice, AST> {
    let (input, left) = parse_mul(input)?;
    let (input, exprs) = many0(pair(
        map(
            alt((
                tag_operator_gen(Operator::Plus),
                tag_operator_gen(Operator::Minus),
            )),
            |op| match op {
                Operator::Plus => BinaryOp::Add,
                Operator::Minus => BinaryOp::Subtract,
                _ => unreachable!(),
            },
        ),
        parse_mul,
    ))
    .parse(input)?;

    let expr = exprs.into_iter().fold(left, |acc, (op, right)| {
        AST::Expr(Expr::BinaryExpr {
            op,
            left: Box::new(acc),
            right: Box::new(right),
        })
    });

    Ok((input, expr))
}

fn parse_mul(input: TokenSlice) -> IResult<TokenSlice, AST> {
    let (input, left) = parse_unary(input)?;
    let (input, exprs) = many0(pair(
        map(
            alt((
                tag_operator_gen(Operator::Multiply),
                tag_operator_gen(Operator::Divide),
                tag_operator_gen(Operator::Modulo),
            )),
            |op| match op {
                Operator::Multiply => BinaryOp::Multiply,
                Operator::Divide => BinaryOp::Divide,
                Operator::Modulo => BinaryOp::Modulo,
                _ => unreachable!(),
            },
        ),
        parse_unary,
    ))
    .parse(input)?;

    let expr = exprs.into_iter().fold(left, |acc, (op, right)| {
        AST::Expr(Expr::BinaryExpr {
            op,
            left: Box::new(acc),
            right: Box::new(right),
        })
    });

    Ok((input, expr))
}

fn parse_unary(input: TokenSlice) -> IResult<TokenSlice, AST> {
    alt((
        map(
            preceded(
                map(tag_operator_gen(Operator::Minus), |_| UnaryOp::Neg),
                parse_unary,
            ),
            |expr| {
                AST::Expr(Expr::UnaryExpr {
                    op: UnaryOp::Neg,
                    expr: Box::new(expr),
                })
            },
        ),
        map(
            preceded(
                map(tag_operator_gen(Operator::LogicalNot), |_| UnaryOp::Not),
                parse_unary,
            ),
            |expr| {
                AST::Expr(Expr::UnaryExpr {
                    op: UnaryOp::Not,
                    expr: Box::new(expr),
                })
            },
        ),
        parse_cast,
    ))
    .parse(input)
}

fn parse_cast(input: TokenSlice) -> IResult<TokenSlice, AST> {
    println!("in the cast now");
    let (input, ty) = opt(map(
        alt((
            tag_keyword_gen(Keyword::Int),
            tag_keyword_gen(Keyword::Long),
        )),
        |kw| match kw {
            Keyword::Int => Type::Int,
            Keyword::Long => Type::Long,
            _ => unreachable!(),
        },
    ))
    .parse(input)?;

    let (input, expr) = parse_primary.parse(input)?;

    if let Some(target_type) = ty {
        Ok((
            input,
            AST::Expr(Expr::Cast {
                target_type,
                expr: Box::new(expr),
            }),
        ))
    } else {
        Ok((input, expr))
    }
}

fn parse_primary(input: TokenSlice) -> IResult<TokenSlice, AST> {
    println!("parse primary");
    alt((
        // parse_identifier,
        parse_literal,
        parse_len,
        parse_method_call,
        parse_location,
        delimited(
            tag_punctuation_gen(Punctuation::LeftParen),
            parse_expr,
            tag_punctuation_gen(Punctuation::RightParen),
        ),
    ))
    .parse(input)
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
    ))
        .parse(input);

    match parse_result {
        Ok((input, (_, _, expr, _))) => {
            let id = match expr {
                AST::Identifier(name) => name,
                _ => {
                    return Err(nom::Err::Error(nom::error::Error::new(
                        input,
                        nom::error::ErrorKind::Tag,
                    )))
                }
            };
            Ok((input, AST::Expr(Expr::Len { id })))
        }
        Err(e) => Err(e),
    }
}

// fn parse_cast(input: TokenSlice) -> IResult<TokenSlice, AST> {
//     // Pattern match  of tokens: applies parsers sequentially
//     let parse_result = ((
//         alt((
//             map(tag_keyword_gen(Keyword::Int), |_| Type::Int),
//             map(tag_keyword_gen(Keyword::Long), |_| Type::Long),
//         )),
//         parse_parens,
//     ))
//         .parse(input);

//     match parse_result {
//         Ok((input, (cast_type, expr))) => {
//             // Successfully parsed, return the AST node
//             Ok((
//                 input,
//                 AST::Expr(Expr::Cast {
//                     target_type: cast_type,
//                     expr: Box::new(expr),
//                 }),
//             ))
//         }
//         Err(e) => Err(e),
//     }
// }

fn parse_parens(input: TokenSlice) -> IResult<TokenSlice, AST> {
    let parse_result = ((
        tag_punctuation_gen(Punctuation::LeftParen),
        parse_expr,
        tag_punctuation_gen(Punctuation::RightParen),
    ))
        .parse(input);

    match parse_result {
        Ok((input, (_, expr, _))) => Ok((input, expr)),
        Err(e) => Err(e),
    }
}

fn parse_location(input: TokenSlice) -> IResult<TokenSlice, AST> {
    println!("Location parsing");
    let parse_result = ((
        parse_identifier, // Variable name
        opt((
            tag_punctuation_gen(Punctuation::LeftBracket),
            parse_expr, // Array index expression
            tag_punctuation_gen(Punctuation::RightBracket),
        )),
    ))
        .parse(input);

    println!("LOCATION: {:?}", parse_result);

    match parse_result {
        Ok((input, (id, maybe_index))) => {
            let id = match id {
                AST::Identifier(name) => name,
                _ => {
                    return Err(nom::Err::Error(nom::error::Error::new(
                        input,
                        nom::error::ErrorKind::Tag,
                    )))
                }
            };

            if let Some((_, index, _)) = maybe_index {
                // Array access
                Ok((
                    input,
                    AST::Expr(Expr::ArrAccess {
                        id,
                        index: Box::new(index),
                    }),
                ))
            } else {
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
    ))
        .parse(input);

    // collect only the important components of the parse and return the AST
    match parse_result {
        Ok((input, (_, _, condition, _, then_block, else_block))) => Ok((
            input,
            AST::Statement(Statement::If {
                condition: Box::new(condition),
                then_block: Box::new(then_block),
                else_block: else_block.map(Box::new),
            }),
        )),
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
        parse_for_update,
        tag_punctuation_gen(Punctuation::RightParen),
        parse_block,
    ))
        .parse(input);

    match parse_result {
        Ok((input, (_, _, id, _, init, _, cond, _, update, _, body))) => {
            let loop_var = match id {
                AST::Identifier(name) => name,
                _ => {
                    return Err(nom::Err::Error(nom::error::Error::new(
                        input,
                        nom::error::ErrorKind::Tag,
                    )))
                }
            };
            Ok((
                input,
                AST::Statement(Statement::For {
                    var: loop_var,
                    init: Box::new(init),
                    condition: Box::new(cond),
                    update: Box::new(update),
                    block: Box::new(body),
                }),
            ))
        }
        Err(e) => Err(e),
    }
}

fn parse_while_loop(input: TokenSlice) -> IResult<TokenSlice, AST> {
    println!("\n\nentering while loop {:?}\n\n", input);
    let parse_result = ((
        tag_keyword_gen(Keyword::While),
        tag_punctuation_gen(Punctuation::LeftParen),
        parse_expr,
        tag_punctuation_gen(Punctuation::RightParen),
        parse_block,
    ))
        .parse(input);
    println!("\n\nending while loop {:?}\n\n", parse_result);
    match parse_result {
        Ok((input, (_, _, expr, _, body))) => {
            Ok((
                input,
                AST::Statement(Statement::While {
                    // must be boxed as pointers since can are non-terminals
                    condition: Box::new(expr),
                    block: Box::new(body),
                }),
            ))
        }
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
    let parse_result = ((
        tag_keyword_gen(Keyword::Return),
        opt(parse_expr),
        tag_punctuation_gen(Punctuation::Semicolon),
    ))
        .parse(input);

    match parse_result {
        Ok((input, (_, retval, _))) => Ok((
            input,
            AST::Statement(Statement::Return {
                expr: retval.map(Box::new),
            }),
        )),
        Err(e) => Err(e),
    }
}

/// Parse increment "++" which we convert into "+=1" in the AST
/// Follows the structure: location increment
fn parse_increment_decrement(input: TokenSlice) -> IResult<TokenSlice, AST> {
    let parse_result = ((
        parse_identifier,
        alt((
            map(tag_operator_gen(Operator::Increment), |_| {
                AssignOp::PlusAssign
            }),
            map(tag_operator_gen(Operator::Decrement), |_| {
                AssignOp::MinusAssign
            }),
        )),
        // tag_punctuation_gen(Punctuation::Semicolon),
    ))
        .parse(input);

    match parse_result {
        Ok((input, (id, op))) => {
            let id = match id {
                AST::Identifier(name) => name,
                _ => {
                    return Err(nom::Err::Error(nom::error::Error::new(
                        input,
                        nom::error::ErrorKind::Tag,
                    )))
                }
            };

            Ok((
                input,
                AST::Statement(Statement::Assignment {
                    location: Box::new(AST::Identifier(id)),
                    expr: Box::new(AST::Expr(Expr::Literal(Literal::Int("1".to_string())))),
                    op,
                }),
            ))
        }
        Err(e) => Err(e),
    }
}

fn parse_assignop_token(input: TokenSlice) -> IResult<TokenSlice, AssignOp> {
    alt((
        map(tag_operator_gen(Operator::Assign), |_| AssignOp::Assign),
        map(tag_operator_gen(Operator::PlusAssign), |_| {
            AssignOp::PlusAssign
        }),
        map(tag_operator_gen(Operator::MinusAssign), |_| {
            AssignOp::MinusAssign
        }),
        map(tag_operator_gen(Operator::MultiplyAssign), |_| {
            AssignOp::MultiplyAssign
        }),
        map(tag_operator_gen(Operator::DivideAssign), |_| {
            AssignOp::DivideAssign
        }),
        map(tag_operator_gen(Operator::ModuloAssign), |_| {
            AssignOp::ModuloAssign
        }),
    ))
    .parse(input)
}

fn parse_assignexpr(input: TokenSlice) -> IResult<TokenSlice, AST> {
    alt((
        (
            parse_increment_decrement,
            tag_punctuation_gen(Punctuation::Semicolon),
        )
            .map(|(increment_decrement, _t)| increment_decrement),
        map((parse_assignop_token, parse_expr), |(op, expr)| {
            AST::Statement(Statement::Assignment {
                location: Box::new(AST::Identifier("".to_string())),
                expr: Box::new(expr),
                op,
            })
        }),
    ))
    .parse(input)
}

fn parse_assign_to_location(input: TokenSlice) -> IResult<TokenSlice, AST> {
    println!("assinging to location");
    let parse_result = ((
        parse_location,
        parse_assignexpr,
        // tag_punctuation_gen(Punctuation::Semicolon),
    ))
        .parse(input);

    println!("after assign: {:?}", parse_result);

    match parse_result {
        Ok((input, (loc, assign))) => {
            let location = match loc {
                AST::Identifier(name) => AST::Identifier(name),
                AST::Expr(Expr::ArrAccess { id, index }) => {
                    AST::Expr(Expr::ArrAccess { id, index })
                }
                _ => {
                    return Err(nom::Err::Error(nom::error::Error::new(
                        input,
                        nom::error::ErrorKind::Tag,
                    )))
                }
            };

            let (expr, op) = match assign {
                AST::Statement(Statement::Assignment { expr, op, .. }) => (expr, op),
                _ => {
                    return Err(nom::Err::Error(nom::error::Error::new(
                        input,
                        nom::error::ErrorKind::Tag,
                    )))
                }
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
    alt((parse_assign_to_location, parse_increment_decrement)).parse(input)
}

// #################################################
//
// #################################################

fn parse_statement(input: TokenSlice) -> IResult<TokenSlice, AST> {
    println!("parsing statement----------");
    let result = alt((
        // location assignment
        map(
            (
                parse_for_update,
                tag_punctuation_gen(Punctuation::Semicolon),
            ),
            |(assign, _)| match assign {
                AST::Statement(Statement::Assignment { location, expr, op }) => {
                    AST::Statement(Statement::Assignment { location, expr, op })
                }
                _ => assign,
            },
        ),
        // methodcall
        map(
            (
                parse_method_call,
                tag_punctuation_gen(Punctuation::Semicolon),
            ),
            |(method_call, _)| method_call,
        ),
        parse_if,
        parse_for_loop,
        parse_while_loop,
        parse_return,
        parse_break_continue,
    ))
    .parse(input);
    println!("after parse_statement {:?}", result);
    result
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
                _ => {
                    return Err(nom::Err::Error(nom::error::Error::new(
                        input,
                        nom::error::ErrorKind::Tag,
                    )))
                }
            };
            Ok((input, AST::ImportDecl { id }))
        }
        Err(e) => Err(e),
    }
}

fn parse_array_field_decl(input: TokenSlice) -> IResult<TokenSlice, AST> {
    println!("heyyyy");
    let parse_result = ((
        parse_identifier,
        tag_punctuation_gen(Punctuation::LeftBracket),
        parse_integer_literal,
        tag_punctuation_gen(Punctuation::RightBracket),
    ))
        .parse(input);
    println!("array result: {:?}", parse_result);
    // return Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Tag)));

    match parse_result {
        Ok((input, (id, _, size, _))) => {
            let id = match id {
                AST::Identifier(name) => name,
                _ => {
                    println!("c");
                    return Err(nom::Err::Error(nom::error::Error::new(
                        input,
                        nom::error::ErrorKind::Tag,
                    )))
                }
            };
            let size = match size {
                AST::Expr(Expr::Literal(Literal::Int(value))) => value,
                AST::Expr(Expr::Literal(Literal::HexInt(value))) => value,
                _ => {
                    println!("b");
                    return Err(nom::Err::Error(nom::error::Error::new(
                        input,
                        nom::error::ErrorKind::Tag,
                    )))
                }
            };
            println!("we good");
            Ok((input, AST::ArrayFieldDecl { id, size }))
        }
        Err(e) => Err(e),
    }
}

fn parse_method_call_or_string_literal(input: TokenSlice) -> IResult<TokenSlice, AST> {
    alt((parse_expr, parse_string_literal)).parse(input)
}

fn parse_method_call(input: TokenSlice) -> IResult<TokenSlice, AST> {
    let (input, method_name) = parse_identifier(input)?;
    let (input, _) = tag_punctuation_gen(Punctuation::LeftParen)(input)?;
    let (input, args) = separated_list0(
        tag_punctuation_gen(Punctuation::Comma),
        parse_method_call_or_string_literal,
    )
    .parse(input)?;
    let (input, _) = tag_punctuation_gen(Punctuation::RightParen)(input)?;

    Ok((
        input,
        AST::Expr(Expr::MethodCall {
            method_name: match method_name {
                AST::Identifier(name) => name,
                _ => {
                    return Err(nom::Err::Error(nom::error::Error::new(
                        input,
                        nom::error::ErrorKind::Tag,
                    )))
                }
            },
            args: args.into_iter().map(Box::new).collect(),
        }),
    ))
}

fn parse_block(input: TokenSlice) -> IResult<TokenSlice, AST> {
    let (input, _) = tag_punctuation_gen(Punctuation::LeftBrace)(input)?; // Match '{'
    println!("a with: {:?}\n", input);
    let (input, field_decls) = many0(parse_field_decl).parse(input)?; // Parse field declarations
    println!("b with: {:?}\n", input);
    let (input, statements) = many0(parse_statement).parse(input)?; // Parse statements
    println!("cc with: {:?}\n", input);
    let (input, _) = tag_punctuation_gen(Punctuation::RightBrace)(input)?; // Match '}'
    println!("ate it?");

    Ok((
        input,
        AST::Block {
            field_decls: field_decls.into_iter().map(Box::new).collect(), // Vec<Box<T>>
            statements: statements.into_iter().map(Box::new).collect(),   // Vec<Box<T>>
        },
    ))
}

fn parse_method_decl(input: TokenSlice) -> IResult<TokenSlice, AST> {
    println!("parse method decl");
    // println!("a: {:?}\n", input);;
    let (input, return_type) = alt((
        // Keyword:: Void is our none return type
        map(tag_keyword_gen(Keyword::Void), |_| Type::Void),
        parse_type,
    ))
    .parse(input)?;

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
    )
    .parse(input)
    .unwrap_or((input, vec![])); // Allow empty parameter list

    let (input, _) = tag_punctuation_gen(Punctuation::RightParen)(input)?; // Match '('
                                                                           // println!("d: {:?}\n", input);

    let (input, body) = parse_block(input)?; // Parse method body
    Ok((
        input,
        AST::MethodDecl {
            return_type,
            name: match method_name {
                AST::Identifier(name) => name, // Extract identifier correctly
                _ => {
                    return Err(nom::Err::Error(nom::error::Error::new(
                        input,
                        nom::error::ErrorKind::Tag,
                    )))
                }
            },
            params,                // Correctly returns Vec<(Type, String)>
            block: Box::new(body), // Ensure body is boxed correctly
        },
    ))
}

fn parse_field_decl(input: TokenSlice) -> IResult<TokenSlice, AST> {
    // Step 1: Parse the type (int, long, bool)
    println!("field _decl parse");
    let (input, field_type) = parse_type(input)?;

    // Step 2: Parse at least one `{id} | {array_field_decl}`, comma-separated
    let (input, fields) = separated_list1(
        tag_punctuation_gen(Punctuation::Comma),
        parse_id_or_array_field_decl, // Parses either an identifier or an array field declaration
    )
    .parse(input)?;

    println!("we here: {:?}", input);
    // Step 3: Match the required `;` at the end
    let (input, _) = tag_punctuation_gen(Punctuation::Semicolon)(input)?;

    Ok((
        input,
        AST::FieldDecl {
            typ: field_type,
            decls: fields.into_iter().map(Box::new).collect(), // Wrap in `Box` to match AST format
        },
    ))
}

fn parse_id_or_array_field_decl(input: TokenSlice) -> IResult<TokenSlice, AST> {
    println!("parse_id_or_array_field_decl");
    alt((parse_array_field_decl, parse_identifier)).parse(input)
}

fn parse_program(input: TokenSlice) -> IResult<TokenSlice, AST> {
    // Sequentially take in input slice and update it
    let (input, import_decls) = many0(parse_import_decl).parse(input)?;
    // println!("import decls are: {:?}\n", import_decls);
    let (input, field_decls) = many0(parse_field_decl).parse(input)?;
    //println!("field decls are: {:?}\n", field_decls);
    let (input, method_decls) = many0(parse_method_decl).parse(input)?;
    // println!("method decls are: {:?}\n", method_decls);

    Ok((
        input,
        AST::Program {
            imports: import_decls.into_iter().map(Box::new).collect(),
            fields: field_decls.into_iter().map(Box::new).collect(),
            methods: method_decls.into_iter().map(Box::new).collect(),
        },
    ))
}

fn parse_always_fail(input: TokenSlice) -> IResult<TokenSlice, AST> {
    Err(nom::Err::Error(nom::error::Error::new(
        input,
        nom::error::ErrorKind::Tag,
    )))
}

/// Input: a sequence of tokens produced by the scanner.
/// Effects:
///    - Verifies that tokens conform to valid Decaf via the language specification
///    - Outputs a syntax tree representation of the Decaf program
pub fn parse(file: &str, filename: &str, writer: &mut Box<dyn std::io::Write>, verbose: bool) {
    println!("PARSING");
    let tokens: Vec<Token> = scan(file, filename, writer, false);
    // println!("{:?}", tokens);

    let parse_result = parse_program(TokenSlice(&tokens));
    match parse_result {
        Ok((rest, parse_tree)) => {
            println!("TOKENS SHOULD BE EMPTY {:?}", rest); // if not empty, throw error
            println!("PARSE TREE: {:?}", parse_tree);
            if !rest.is_empty() {
                println!("\n");
                println!("ERRRORR-----------");
                panic!()
            }
            if verbose {
                let template_string = format!("SUCCESSFUL PARSE==================\n");
                writeln!(writer, "{}", template_string).expect("Failed to write error to stdout!");
            }
        }

        Err(parse_error) => {
            if verbose {
                let template_string = format!("ERROR PARSE===============\n");
                writeln!(writer, "{}", template_string).expect("Failed to write error to stdout!");
            }
        }
    }
}
