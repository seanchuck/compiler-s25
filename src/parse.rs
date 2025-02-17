/*
Scanner.

Idea: Use the nom parser combinator to build small parsers and 
gradually compose them with `alt` to parse the entire program. 
Some productions in the grammar don't need to be explicitly 
defined as they are covered by the other parsers.
(e.g., alpha or digit are just covered by literals).


Big game of matching.
*/


use crate::ast::*;
use crate::token::*;
// use anyhow::{anyhow, Result};
use nom::{combinator::{map, value}, branch::alt, error::Error, sequence::{tuple, delimited}, IResult, Parser, bytes::complete::tag};



// The most basic parsers are tags, which match a simple pattern

/// Builds a parser that match a specific `Operator`
fn tag_operator(expected: Operator) -> impl Fn(&[Token]) -> IResult<&[Token], Operator> {
    move |input: &[Token]| match input.split_first() {
        Some((Token::Symbol(Symbol::Operator(first)), rest)) if *first == expected => Ok((rest, first.clone())),
        _ => Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Tag))),
    }
}

fn tag_punctuation(punc: Punctuation) -> impl Fn(&[Token]) -> IResult<&[Token], ()> {
    move |input: &[Token]| match input.first() {
        Some(Token::Symbol(Symbol::Punctuation(p))) if *p == punc => Ok((&input[1..], ())),
        _ => Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Tag))),
    }
}

fn tag_keyword(expected: Keyword) -> impl Fn(&[Token]) -> IResult<&[Token], Keyword> {
    move |input: &[Token]| match input.split_first() {
        Some((Token::Keyword(first), rest)) if *first == expected => Ok((rest, first.clone())),
        _ => Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Tag))),
    }
}

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

fn parse_literal(input: &[Token]) -> IResult<&[Token], AST> {
    match input.first() {
        Some(Token::Literal(Literal::Char(id))) => Ok((
            &input[1..],
            AST::Expr(Expr::Literal(Literal::Char(id.clone()))),
        )),
        Some(Token::Literal(Literal::String(id))) => Ok((
            &input[1..],
            AST::Expr(Expr::Literal(Literal::String(id.clone()))),
        )),
        Some(Token::Literal(Literal::Int(id))) => Ok((
            &input[1..],
            AST::Expr(Expr::Literal(Literal::Int(id.clone()))),
        )),
        Some(Token::Literal(Literal::Long(id))) => Ok((
            &input[1..],
            AST::Expr(Expr::Literal(Literal::Long(id.clone()))),
        )),
        Some(Token::Literal(Literal::HexInt(id))) => Ok((
            &input[1..],
            AST::Expr(Expr::Literal(Literal::HexInt(id.clone()))),
        )),
        Some(Token::Literal(Literal::HexLong(id))) => Ok((
            &input[1..],
            AST::Expr(Expr::Literal(Literal::HexLong(id.clone()))),
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

fn parse_expr(input: &[Token]) -> IResult<&[Token], AST> {
    alt((
        parse_literal, // don't need args for funcs that just take [Token]
        parse_builtin
    )).parse(input)
}

/// Attempts to match the next token to an Operator
fn parse_binop(input: &[Token]) -> IResult<&[Token], BinOp> {
    alt((
        // Matches parser that accepts the given operator; Returns the appropriate BinOp type
        // Must enforce precedence order of ops
        map(tag_operator(Operator::Multiply), |_| BinOp::Add),
        map(tag_operator(Operator::Divide), |_| BinOp::Subtract),
        map(tag_operator(Operator::Modulo), |_| BinOp::Modulo),
        map(tag_operator(Operator::Plus), |_| BinOp::Multiply),
        map(tag_operator(Operator::Minus), |_| BinOp::Divide),

        map(tag_operator(Operator::Equal), |_| BinOp::Equal),
        map(tag_operator(Operator::NotEqual), |_| BinOp::NotEqual),
        map(tag_operator(Operator::Less), |_| BinOp::Less),
        map(tag_operator(Operator::Greater), |_| BinOp::Greater),
        map(tag_operator(Operator::LessEqual), |_| BinOp::LessEqual),
        map(tag_operator(Operator::GreaterEqual), |_| BinOp::GreaterEqual),
        
        map(tag_operator(Operator::LogicalAnd), |_| BinOp::And),
        map(tag_operator(Operator::LogicalOr), |_| BinOp::Or),
    )).parse(input)
    
}

fn parse_UnaryExpr(input: &[Token]) -> IResult<&[Token], UnaryExpr> {
    alt((
        map(tag_operator(Operator::Minus), |_| UnaryExpr::Neg),
        map(tag_operator(Operator::LogicalNot), |_| UnaryExpr::Not),
    )).parse(input)
    
}

fn parse_binexpr(input: &[Token]) -> IResult<&[Token], AST> {
    map(
        tuple((parse_expr, parse_binop, parse_expr)),
        |(lexpr, op, rexpr)| {
            AST::Expr(Expr::BinaryExpr {
                left: Box::new(lexpr),
                op,
                right: Box::new(rexpr),
            })
        },
    ).parse(input)
}

fn parse_unexpr(input: &[Token]) -> IResult<&[Token], AST> {
    map(
        tuple((parse_UnaryExpr, parse_expr)),
        |(op, operand)| {
            AST::Expr(Expr::UnOp {
                op,
                expr: Box::new(operand),
            })
        },
    ).parse(input)
}

fn parse_len(input: &[Token]) -> IResult<&[Token], AST> {
    let (input, (_, _, expr, _)) = tuple((
        tag_keyword(Keyword::Len),
        tag_punctuation(Punctuation::LeftParen),
        parse_identifier, // already has expected &[Token] input
        tag_punctuation(Punctuation::RightParen),
    )).parse(input)?;

    // Extract just the identifier name 
    let id = match expr {
        AST::Identifier(name) => name,
        _ => return Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Tag))),
    };

    Ok((input, AST::Expr(Expr::Len { id }))) 
}

fn parse_cast(input: &[Token]) -> IResult<&[Token], AST> {
    // Pattern match tuple of tokens: applies parsers sequentially
    let (input, (cast_type, _, expr, _)) = tuple((
        alt((
            map(tag_keyword(Keyword::Int), |_| Type::Int),
            map(tag_keyword(Keyword::Long), |_| Type::Long),
        )),
        tag_punctuation(Punctuation::LeftParen),
        parse_expr,
        tag_punctuation(Punctuation::RightParen),
    )).parse(input)?;

    Ok((input, AST::Expr(Expr::Cast { 
        target_type: cast_type, 
        expr: Box::new(expr) 
    })))
}


fn parse_builtin(input: &[Token]) -> IResult<&[Token], AST> {
    alt((
        parse_cast,
        parse_binexpr
    )).parse(input)
}


fn parse_program(input: &[Token]) -> IResult<&[Token], AST> {
    unreachable!()
}

/// Input: a sequence of tokens produced by the scanner.
/// Effects:
///    - Verifies that tokens conform to valid Decaf via the language specification
///    - Outputs a syntax tree representation of the Decaf program
pub fn parse(file: &str, filename: &str, writer: &mut Box<dyn std::io::Write>) {
    // use nom for parser
    // enum for AST
    println!("PARSING");
}
