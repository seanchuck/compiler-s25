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

Each parser follows the same pattern:
    1. attempt to match something
    2. either build AST or throw error based on match result

nom's IResult types either return
    - succesful parse : Ok(rest_tokens, AST)
    - unsuccesfful parse : Err

Had to hack the grammar to deal with left recursion of expression.
Ran into issues regarding precedence and redundant punctuation
during debugging.

Update: Span tracking
    - each literal has a span taken directly from the associated token
    - spans of compound structures (i.e., expression) are generated
    by combining the span of the first and last tokens involved
*/
use crate::ast::*;
use crate::scan::scan;
use crate::token::*;
// use crate::utils::print::save_dot_file;

use nom::combinator::all_consuming;
use nom::multi::{many0, separated_list0};
use nom::sequence::{pair, preceded};
use nom::{
    branch::alt,
    combinator::{map, opt},
    error::Error,
    multi::separated_list1,
    IResult, Parser,
};

// #################################################
// HELPERS
// #################################################

/// Extracts the span from an AST node.
fn get_span(ast: &AST) -> Span {
    match ast {
        AST::Expr(expr) => match expr {
            Expr::BinaryExpr { span, .. }
            | Expr::UnaryExpr { span, .. }
            | Expr::Cast { span, .. }
            | Expr::ArrAccess { span, .. }
            | Expr::MethodCall { span, .. }
            | Expr::Len { span, .. }
            | Expr::Literal { span, .. } => span.clone(),
        },
        AST::Statement(stmt) => match stmt {
            Statement::Assignment { span, .. }
            | Statement::MethodCall { span, .. }
            | Statement::If { span, .. }
            | Statement::For { span, .. }
            | Statement::Update { span, .. }
            | Statement::While { span, .. }
            | Statement::Return { span, .. }
            | Statement::Break { span }
            | Statement::Continue { span } => span.clone(),
        },
        AST::Block { span, .. }
        | AST::Program { span, .. }
        | AST::ImportDecl { span, .. }
        | AST::FieldDecl { span, .. }
        | AST::ArrayFieldDecl { span, .. }
        | AST::MethodDecl { span, .. } => span.clone(),
        AST::Identifier { span, .. } => span.clone(),
        AST::Type { span, .. } => span.clone(),
        _ => panic!("type has no span!"),
    }
}

/// Merge two spans to create a new span covering both.
fn merge_spans(left: &Span, right: &Span) -> Span {
    Span {
        sline: left.sline,
        scol: left.scol,
        eline: right.eline,
        ecol: right.ecol,
    }
}

/// Constructs a binary expression and merges spans automatically.
fn build_binary_expr(left: AST, op: BinaryOp, right: AST) -> AST {
    let span = merge_spans(&get_span(&left), &get_span(&right));

    AST::Expr(Expr::BinaryExpr {
        op,
        left: Box::new(left),
        right: Box::new(right),
        span,
    })
}

fn extract_span_from_token(token: &Token) -> Span {
    match token {
        Token::Keyword { span, .. }
        | Token::Identifier { span, .. }
        | Token::Symbol { span, .. }
        | Token::Literal { span, .. } => span.clone(),
    }
}

fn get_token_display(token: &Token) -> String {
    match token {
        Token::Keyword { display, .. }
        | Token::Identifier { display, .. }
        | Token::Symbol { display, .. }
        | Token::Literal { display, .. } => display.clone(),
    }
}

/// Always errors for testing
fn _parse_always_fail(input: TokenSlice) -> IResult<TokenSlice, AST> {
    Err(nom::Err::Error(nom::error::Error::new(
        input,
        nom::error::ErrorKind::Tag,
    )))
}



// #################################################
// TAG GENERATORS
// #################################################
// The most basic parser is a tag that matches very simple tokens.
// These generators are great because we can use them to individually
// match any grouping of operator, punctuation, identifier, or keyword.

/// Generates a parser that matches a specific `Operator`
fn tag_operator_gen(
    expected_token: Operator,
) -> impl Fn(TokenSlice) -> IResult<TokenSlice, (Operator, Span)> {
    move |input: TokenSlice| match input.0.split_first() {
        Some((
            Token::Symbol {
                value: Symbol::Operator(first_token),
                span,
                ..
            },
            rest,
        )) if *first_token == expected_token => {
            Ok((TokenSlice(rest), (first_token.clone(), span.clone())))
        }
        _ => Err(nom::Err::Error(nom::error::Error::new(
            input,
            nom::error::ErrorKind::Tag,
        ))),
    }
}

/// Generates a parser that matches a specific `Punctuation`
fn tag_punctuation_gen(
    expected_token: Punctuation,
) -> impl Fn(TokenSlice) -> IResult<TokenSlice, (Punctuation, Span)> {
    move |input: TokenSlice| match input.0.split_first() {
        Some((
            Token::Symbol {
                value: Symbol::Punctuation(first_token),
                span,
                ..
            },
            rest,
        )) if *first_token == expected_token => {
            Ok((TokenSlice(rest), (first_token.clone(), span.clone())))
        }
        _ => Err(nom::Err::Error(nom::error::Error::new(
            input,
            nom::error::ErrorKind::Tag,
        ))),
    }
}

/// Generates a parser that matches a specific `Keyword`
fn tag_keyword_gen(
    expected_token: Keyword,
) -> impl Fn(TokenSlice) -> IResult<TokenSlice, (Keyword, Span)> {
    move |input: TokenSlice| match input.0.split_first() {
        Some((
            Token::Keyword {
                value: first_token,
                span,
                ..
            },
            rest,
        )) if *first_token == expected_token => {
            Ok((TokenSlice(rest), (first_token.clone(), span.clone())))
        }
        _ => Err(nom::Err::Error(nom::error::Error::new(
            input,
            nom::error::ErrorKind::Tag,
        ))),
    }
}

// #################################################
// LITERALS
// (only these change with token Rep)
// #################################################

/// Parses an identifier.
fn parse_identifier(input: TokenSlice) -> IResult<TokenSlice, AST> {
    if let Some(Token::Identifier { value, span, .. }) = input.0.first() {
        Ok((
            TokenSlice(&input.0[1..]),
            AST::Identifier {
                id: value.clone(),
                span: span.clone(),
            },
        ))
    } else {
        Err(nom::Err::Error(nom::error::Error::new(
            input,
            nom::error::ErrorKind::Tag,
        )))
    }
}

fn parse_char_bool_literal(input: TokenSlice) -> IResult<TokenSlice, AST> {
    match input.0.first() {
        Some(Token::Literal {
            value: Literal::Char(id),
            span,
            ..
        }) => Ok((
            TokenSlice(&input.0[1..]),
            AST::Expr(Expr::Literal {
                lit: Literal::Char(*id),
                span: span.clone(),
            }),
        )),
        Some(Token::Literal {
            value: Literal::Bool(id),
            span,
            ..
        }) => Ok((
            TokenSlice(&input.0[1..]),
            AST::Expr(Expr::Literal {
                lit: Literal::Bool(*id),
                span: span.clone(),
            }),
        )),
        _ => Err(nom::Err::Error(nom::error::Error::new(
            input,
            nom::error::ErrorKind::Tag,
        ))),
    }
}

fn parse_long_literal(input: TokenSlice) -> IResult<TokenSlice, AST> {
    match input.0.first() {
        Some(Token::Literal {
            value: Literal::Long(id),
            span,
            ..
        }) => Ok((
            TokenSlice(&input.0[1..]),
            AST::Expr(Expr::Literal {
                lit: Literal::Long(id.clone()),
                span: span.clone(),
            }),
        )),
        Some(Token::Literal {
            value: Literal::HexLong(id),
            span,
            ..
        }) => Ok((
            TokenSlice(&input.0[1..]),
            AST::Expr(Expr::Literal {
                lit: Literal::HexLong(id.clone()),
                span: span.clone(),
            }),
        )),
        _ => Err(nom::Err::Error(nom::error::Error::new(
            input,
            nom::error::ErrorKind::Tag,
        ))),
    }
}

fn parse_integer_literal(input: TokenSlice) -> IResult<TokenSlice, AST> {
    match input.0.first() {
        Some(Token::Literal {
            value: Literal::HexInt(id),
            span,
            ..
        }) => Ok((
            TokenSlice(&input.0[1..]),
            AST::Expr(Expr::Literal {
                lit: Literal::HexInt(id.clone()),
                span: span.clone(),
            }),
        )),
        Some(Token::Literal {
            value: Literal::Int(id),
            span,
            ..
        }) => Ok((
            TokenSlice(&input.0[1..]),
            AST::Expr(Expr::Literal {
                lit: Literal::Int(id.clone()),
                span: span.clone(),
            }),
        )),
        _ => Err(nom::Err::Error(nom::error::Error::new(
            input,
            nom::error::ErrorKind::Tag,
        ))),
    }
}

fn parse_string_literal(input: TokenSlice) -> IResult<TokenSlice, AST> {
    match input.0.first() {
        Some(Token::Literal {
            value: Literal::String(id),
            span,
            ..
        }) => Ok((
            TokenSlice(&input.0[1..]),
            AST::Expr(Expr::Literal {
                lit: Literal::String(id.clone()),
                span: span.clone(),
            }),
        )),
        _ => Err(nom::Err::Error(nom::error::Error::new(
            input,
            nom::error::ErrorKind::Tag,
        ))),
    }
}

/// Parses all literals, including string literals
fn parse_literal(input: TokenSlice) -> IResult<TokenSlice, AST> {
    alt((
        parse_long_literal,
        parse_char_bool_literal,
        parse_integer_literal,
        parse_string_literal,
    ))
    .parse(input)
}

/// Parse a type
/// Parse a type and return an `AST::Type` with a span.
fn parse_type(input: TokenSlice) -> IResult<TokenSlice, AST> {
    match input.0.first() {
        Some(Token::Keyword { value, span, .. }) => {
            let ast_type = match value {
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
            Ok((
                TokenSlice(&input.0[1..]),
                AST::Type {
                    typ: ast_type,
                    span: span.clone(), // Preserve the span from the token
                },
            ))
        }
        _ => Err(nom::Err::Error(Error::new(
            input,
            nom::error::ErrorKind::Tag,
        ))),
    }
}

// #################################################
// EXPRESSIONS
// #################################################

// Hacked Grammar: implies precedence to deal with amiguities
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

fn parse_expr(input: TokenSlice) -> IResult<TokenSlice, AST> {
    // An expression is a `logical or` of `and`
    parse_or.parse(input)
}

fn parse_or(input: TokenSlice) -> IResult<TokenSlice, AST> {
    let (input, left) = parse_and(input)?;
    let (input, exprs) = many0(pair(
        map(tag_operator_gen(Operator::LogicalOr), |_| BinaryOp::Or),
        parse_and,
    ))
    .parse(input)?;

    let expr = exprs
        .into_iter()
        .fold(left, |acc, (op, right)| build_binary_expr(acc, op, right));

    Ok((input, expr))
}

fn parse_and(input: TokenSlice) -> IResult<TokenSlice, AST> {
    let (input, left) = parse_eq(input)?;
    let (input, exprs) = many0(pair(
        map(tag_operator_gen(Operator::LogicalAnd), |_| BinaryOp::And),
        parse_eq,
    ))
    .parse(input)?;

    let expr = exprs
        .into_iter()
        .fold(left, |acc, (op, right)| build_binary_expr(acc, op, right));

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
            |(op, _)| match op {
                Operator::Equal => BinaryOp::Equal,
                Operator::NotEqual => BinaryOp::NotEqual,
                _ => unreachable!(),
            },
        ),
        parse_rel,
    ))
    .parse(input)?;

    let expr = exprs
        .into_iter()
        .fold(left, |acc, (op, right)| build_binary_expr(acc, op, right));

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
            |(op, _)| match op {
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

    let expr = exprs
        .into_iter()
        .fold(left, |acc, (op, right)| build_binary_expr(acc, op, right));

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
            |(op, _)| match op {
                Operator::Plus => BinaryOp::Add,
                Operator::Minus => BinaryOp::Subtract,
                _ => unreachable!(),
            },
        ),
        parse_mul,
    ))
    .parse(input)?;

    let expr = exprs
        .into_iter()
        .fold(left, |acc, (op, right)| build_binary_expr(acc, op, right));

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
            |(op, _)| match op {
                Operator::Multiply => BinaryOp::Multiply,
                Operator::Divide => BinaryOp::Divide,
                Operator::Modulo => BinaryOp::Modulo,
                _ => unreachable!(),
            },
        ),
        parse_unary,
    ))
    .parse(input)?;

    let expr = exprs
        .into_iter()
        .fold(left, |acc, (op, right)| build_binary_expr(acc, op, right));

    Ok((input, expr))
}

fn parse_unary(input: TokenSlice) -> IResult<TokenSlice, AST> {
    alt((
        map(
            preceded(tag_operator_gen(Operator::Minus), parse_unary),
            |expr| {
                let span = merge_spans(&get_span(&expr), &get_span(&expr));
                AST::Expr(Expr::UnaryExpr {
                    op: UnaryOp::Neg,
                    expr: Box::new(expr),
                    span,
                })
            },
        ),
        map(
            preceded(tag_operator_gen(Operator::LogicalNot), parse_unary),
            |expr| {
                let span = merge_spans(&get_span(&expr), &get_span(&expr));
                AST::Expr(Expr::UnaryExpr {
                    op: UnaryOp::Not,
                    expr: Box::new(expr),
                    span,
                })
            },
        ),
        parse_cast,
    ))
    .parse(input)
}

fn parse_cast(input: TokenSlice) -> IResult<TokenSlice, AST> {
    let (input, ty) = opt(map(
        alt((
            tag_keyword_gen(Keyword::Int),
            tag_keyword_gen(Keyword::Long),
        )),
        |(kw, span)| match kw {
            Keyword::Int => (Type::Int, span),
            Keyword::Long => (Type::Long, span),
            _ => unreachable!(),
        },
    ))
    .parse(input)?;

    let (input, expr) = parse_primary(input)?;

    if let Some((target_type, ty_span)) = ty {
        let span = merge_spans(&ty_span, &get_span(&expr));

        Ok((
            input,
            AST::Expr(Expr::Cast {
                target_type,
                expr: Box::new(expr),
                span,
            }),
        ))
    } else {
        Ok((input, expr))
    }
}

fn parse_primary(input: TokenSlice) -> IResult<TokenSlice, AST> {
    alt((
        parse_literal,
        parse_len,
        parse_method_call,
        parse_location,
        map(
            (
                tag_punctuation_gen(Punctuation::LeftParen),
                parse_expr,
                tag_punctuation_gen(Punctuation::RightParen),
            ),
            |((_, start_span), mut expr, (_, end_span))| {
                let span = merge_spans(&start_span, &end_span);
                if let AST::Expr(ref mut e) = expr {
                    e.set_span(span);
                }
                expr
            },
        ),
    ))
    .parse(input)
}

// #################################################
// INTERMEDIATE-LEVEL PARSERS
// #################################################
fn parse_len(input: TokenSlice) -> IResult<TokenSlice, AST> {
    let (input, ((_, len_span), (_, _), id_ast, (_, end_span))) = (
        tag_keyword_gen(Keyword::Len),
        tag_punctuation_gen(Punctuation::LeftParen),
        parse_identifier,
        tag_punctuation_gen(Punctuation::RightParen),
    )
        .parse(input)?;

    let id = match id_ast {
        AST::Identifier { id, span: id_span } => Box::new(AST::Identifier { id, span: id_span }),
        _ => {
            return Err(nom::Err::Error(nom::error::Error::new(
                input,
                nom::error::ErrorKind::Tag,
            )))
        }
    };

    let span = merge_spans(&len_span, &end_span);
    Ok((input, AST::Expr(Expr::Len { id, span })))
}

fn parse_location(input: TokenSlice) -> IResult<TokenSlice, AST> {
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
            if let AST::Identifier { id, span } = id {
                if let Some((_, index, _)) = maybe_index {
                    // Array access
                    let new_span = merge_spans(&span, &get_span(&index)); // Merge identifier and index spans
                    Ok((
                        input,
                        AST::Expr(Expr::ArrAccess {
                            id,
                            index: Box::new(index),
                            span: new_span,
                        }),
                    ))
                } else {
                    // Variable access (no array indexing)
                    Ok((input, AST::Identifier { id, span }))
                }
            } else {
                Err(nom::Err::Error(nom::error::Error::new(
                    input,
                    nom::error::ErrorKind::Tag,
                )))
            }
        }
        Err(e) => Err(e),
    }
}
fn parse_if(input: TokenSlice) -> IResult<TokenSlice, AST> {
    let (input, (_, if_span)) = tag_keyword_gen(Keyword::If)(input)?;
    let (input, _) = tag_punctuation_gen(Punctuation::LeftParen)(input)?;
    let (input, condition) = parse_expr(input)?;
    let (input, _) = tag_punctuation_gen(Punctuation::RightParen)(input)?;
    let (input, then_block) = parse_block(input)?;
    let (input, else_block) =
        opt(preceded(tag_keyword_gen(Keyword::Else), parse_block)).parse(input)?;

    // Merge spans: `if` to the end of the last block (either `else` or `then`)
    let end_span = else_block
        .as_ref()
        .map(|blk| get_span(blk))
        .unwrap_or_else(|| get_span(&then_block));
    let full_span = merge_spans(&if_span, &end_span);

    Ok((
        input,
        AST::Statement(Statement::If {
            condition: Box::new(condition),
            then_block: Box::new(then_block),
            else_block: else_block.map(Box::new),
            span: full_span,
        }),
    ))
}

fn parse_for_loop(input: TokenSlice) -> IResult<TokenSlice, AST> {
    let (input, (_, for_span)) = tag_keyword_gen(Keyword::For)(input)?;
    let (input, _) = tag_punctuation_gen(Punctuation::LeftParen)(input)?;
    let (input, id) = parse_identifier(input)?;
    let (input, _) = tag_operator_gen(Operator::Assign)(input)?;
    let (input, init) = parse_expr(input)?;
    let (input, _) = tag_punctuation_gen(Punctuation::Semicolon)(input)?;
    let (input, condition) = parse_expr(input)?;
    let (input, _) = tag_punctuation_gen(Punctuation::Semicolon)(input)?;
    let (input, update) = parse_for_update(input)?;
    let (input, _) = tag_punctuation_gen(Punctuation::RightParen)(input)?;
    let (input, body) = parse_block(input)?;

    // Merge spans: `for` to `}` of the loop body
    let full_span = merge_spans(&for_span, &get_span(&body));

    Ok((
        input,
        AST::Statement(Statement::For {
            var: match id {
                AST::Identifier { id, .. } => id,
                _ => panic!("Expected identifier"),
            },
            init: Box::new(init),
            condition: Box::new(condition),
            update: Box::new(update),
            block: Box::new(body),
            span: full_span,
        }),
    ))
}

fn parse_while_loop(input: TokenSlice) -> IResult<TokenSlice, AST> {
    let (input, (_, while_span)) = tag_keyword_gen(Keyword::While)(input)?;
    let (input, _) = tag_punctuation_gen(Punctuation::LeftParen)(input)?;
    let (input, condition) = parse_expr(input)?;
    let (input, _) = tag_punctuation_gen(Punctuation::RightParen)(input)?;
    let (input, body) = parse_block(input)?;

    // Merge spans: `while` to `}` of the loop body
    let full_span = merge_spans(&while_span, &get_span(&body));

    Ok((
        input,
        AST::Statement(Statement::While {
            condition: Box::new(condition),
            block: Box::new(body),
            span: full_span,
        }),
    ))
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
        Ok((input, ((kw, kw_span), _))) => {
            let stmt = match kw {
                Keyword::Break => Statement::Break { span: kw_span },
                Keyword::Continue => Statement::Continue { span: kw_span },
                _ => unreachable!(),
            };

            Ok((input, AST::Statement(stmt)))
        }
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
        Ok((input, ((_, ret_span), retval, _))) => {
            let span = match &retval {
                Some(expr) => merge_spans(&ret_span, &get_span(expr)),
                None => ret_span,
            };

            Ok((
                input,
                AST::Statement(Statement::Return {
                    expr: retval.map(Box::new),
                    span,
                }),
            ))
        }
        Err(e) => Err(e),
    }
}

/// Parse increment "++" which we convert into "+=1" in the AST
/// Supports: `x++`, `arr[i]++` (converted to `x += 1`)
fn parse_increment_decrement(input: TokenSlice) -> IResult<TokenSlice, AST> {
    let parse_result = ((
        parse_location, // Allows identifiers and array accesses
        alt((
            map(tag_operator_gen(Operator::Increment), |_| {
                AssignOp::PlusAssign
            }),
            map(tag_operator_gen(Operator::Decrement), |_| {
                AssignOp::MinusAssign
            }),
        )),
    ))
        .parse(input);

    match parse_result {
        Ok((input, (location, op))) => {
            let location_span = get_span(&location);
            let literal_span = location_span.clone();

            // Merge spans to cover the entire statement
            let span = merge_spans(&location_span, &literal_span);

            Ok((
                input,
                AST::Statement(Statement::Assignment {
                    location: Box::new(location),
                    expr: Box::new(AST::Expr(Expr::Literal {
                        lit: Literal::Int("1".to_string()),
                        span: literal_span,
                    })),
                    op,
                    span,
                }),
            ))
        }
        Err(e) => Err(e),
    }
}

/// Parse a single assignment token and return an `AssignOp` (without a span).
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

/// Parse an assignment expression, which consists of an incr/decr
/// or an assignment token and expression.
fn parse_assignexpr(input: TokenSlice) -> IResult<TokenSlice, AST> {
    alt((
        map(
            (
                parse_increment_decrement,
                tag_punctuation_gen(Punctuation::Semicolon),
            ),
            |(increment_decrement, _)| increment_decrement,
        ),
        map((parse_assignop_token, parse_expr), |(op, expr)| {
            let span = get_span(&expr);
            AST::Statement(Statement::Assignment {
                location: Box::new(AST::Empty), // Placeholder, since location is handled outside
                expr: Box::new(expr),
                op,
                span,
            })
        }),
    ))
    .parse(input)
}
fn parse_assign_to_location(input: TokenSlice) -> IResult<TokenSlice, AST> {
    let (input, location) = parse_location(input)?;
    let (input, assignment) = parse_assignexpr(input)?;

    let (expr, op, assign_span) = match assignment {
        AST::Statement(Statement::Assignment { expr, op, span, .. }) => (expr, op, span),
        _ => panic!("Expected assignment statement"),
    };

    // Ensure span extends from `location` to `assignment`
    let full_span = merge_spans(&get_span(&location), &assign_span);

    Ok((
        input,
        AST::Statement(Statement::Assignment {
            location: Box::new(location),
            expr,
            op,
            span: full_span,
        }),
    ))
}

fn parse_for_update(input: TokenSlice) -> IResult<TokenSlice, AST> {
    alt((parse_assign_to_location, parse_increment_decrement)).parse(input)
}

// #################################################
// TOP-LEVEL STRUCTURES
// #################################################

fn parse_statement(input: TokenSlice) -> IResult<TokenSlice, AST> {
    alt((
        // location assignment
        map(
            (
                parse_for_update,
                tag_punctuation_gen(Punctuation::Semicolon),
            ),
            |(assign, _)| match assign {
                AST::Statement(Statement::Assignment {
                    location,
                    expr,
                    op,
                    span,
                }) => {
                    AST::Statement(Statement::Assignment {
                        location,
                        expr,
                        op,
                        span,
                    }) // Preserve span
                }
                _ => assign, // Pass through if it's something else
            },
        ),
        // method call
        map(
            (
                parse_method_call,
                tag_punctuation_gen(Punctuation::Semicolon),
            ),
            |(method_call, _)| match method_call {
                AST::Expr(Expr::MethodCall {
                    method_name,
                    args,
                    span,
                }) => {
                    AST::Statement(Statement::MethodCall {
                        method_name,
                        args,
                        span,
                    }) // Preserve span
                }
                _ => method_call, // Pass through if it's something else
            },
        ),
        parse_if,
        parse_for_loop,
        parse_while_loop,
        parse_return,
        parse_break_continue,
    ))
    .parse(input)
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
            if let AST::Identifier { id, span } = id {
                Ok((input, AST::ImportDecl { id, span })) // Now tracks span!
            } else {
                Err(nom::Err::Error(nom::error::Error::new(
                    input,
                    nom::error::ErrorKind::Tag,
                )))
            }
        }
        Err(e) => Err(e),
    }
}
fn parse_array_field_decl(input: TokenSlice) -> IResult<TokenSlice, AST> {
    let parse_result = ((
        parse_identifier,
        tag_punctuation_gen(Punctuation::LeftBracket),
        parse_integer_literal,
        tag_punctuation_gen(Punctuation::RightBracket),
    ))
        .parse(input);

    match parse_result {
        Ok((input, (id, _, size, _))) => {
            if let AST::Identifier { id, span: id_span } = id {
                if let AST::Expr(Expr::Literal {
                    lit,
                    span: size_span,
                }) = size
                {
                    let size_value = match lit {
                        Literal::Int(value) | Literal::HexInt(value) => value, // Handle both cases
                        _ => {
                            return Err(nom::Err::Error(nom::error::Error::new(
                                input,
                                nom::error::ErrorKind::Tag,
                            )))
                        }
                    };

                    let full_span = merge_spans(&id_span, &size_span); // Merge the span

                    return Ok((
                        input,
                        AST::ArrayFieldDecl {
                            id,
                            size: size_value,
                            span: full_span,
                        },
                    ));
                }
            }

            Err(nom::Err::Error(nom::error::Error::new(
                input,
                nom::error::ErrorKind::Tag,
            )))
        }
        Err(e) => Err(e),
    }
}

fn parse_method_call_or_string_literal(input: TokenSlice) -> IResult<TokenSlice, AST> {
    alt((parse_expr, parse_string_literal)).parse(input)
}

fn parse_method_call(input: TokenSlice) -> IResult<TokenSlice, AST> {
    let (input, method_name) = parse_identifier(input)?;
    let (input, (_, _)) = tag_punctuation_gen(Punctuation::LeftParen)(input)?;
    let (input, args) = separated_list0(
        tag_punctuation_gen(Punctuation::Comma),
        parse_method_call_or_string_literal,
    )
    .parse(input)?;
    let (input, (_, end_span)) = tag_punctuation_gen(Punctuation::RightParen)(input)?;

    // Ensure span extends from method name to `)`
    let full_span = merge_spans(&get_span(&method_name), &end_span);

    Ok((
        input,
        AST::Expr(Expr::MethodCall {
            method_name: match method_name {
                AST::Identifier { id, .. } => id,
                _ => panic!("Expected identifier for method name"),
            },
            args: args.into_iter().map(Box::new).collect(),
            span: full_span,
        }),
    ))
}

fn parse_block(input: TokenSlice) -> IResult<TokenSlice, AST> {
    let (input, (_, left_span)) = tag_punctuation_gen(Punctuation::LeftBrace)(input)?; // `{`
    let (input, field_decls) = many0(parse_field_decl).parse(input)?;
    let (input, statements) = many0(parse_statement).parse(input)?;
    let (input, (_, right_span)) = tag_punctuation_gen(Punctuation::RightBrace)(input)?; // `}`

    // Ensure the span always extends from `{` to `}`
    let full_span = merge_spans(&left_span, &right_span);

    Ok((
        input,
        AST::Block {
            field_decls: field_decls.into_iter().map(Box::new).collect(),
            statements: statements.into_iter().map(Box::new).collect(),
            span: full_span, // Ensures span correctly covers `{ ... }`
        },
    ))
}
fn parse_method_decl(input: TokenSlice) -> IResult<TokenSlice, AST> {
    let (input, return_type) = alt((
        map(tag_keyword_gen(Keyword::Void), |(_, span)| AST::Type {
            typ: Type::Void,
            span,
        }),
        parse_type,
    ))
    .parse(input)?;

    let (input, method_name) = parse_identifier(input)?;
    let (input, _) = tag_punctuation_gen(Punctuation::LeftParen)(input)?;
    let (input, params) = separated_list0(
        tag_punctuation_gen(Punctuation::Comma),
        map(
            (parse_type, parse_identifier),
            |(param_type, param_name)| {
                let span = get_span(&param_name);
                Param {
                    typ: match param_type {
                        AST::Type { typ, .. } => typ,
                        _ => panic!("Expected AST::Type, found {:?}", param_type),
                    },
                    name: Box::new(param_name),
                    span,
                }
            },
        ),
    )
    .parse(input)?;
    let (input, _) = tag_punctuation_gen(Punctuation::RightParen)(input)?;
    let (input, body) = parse_block(input)?;

    // Ensure span extends from method return type to `}` of body
    let full_span = merge_spans(&get_span(&return_type), &get_span(&body));

    Ok((
        input,
        AST::MethodDecl {
            return_type: match return_type {
                AST::Type { typ, .. } => typ,
                _ => panic!("Expected AST::Type, found {:?}", return_type),
            },
            name: match method_name {
                AST::Identifier { id, .. } => id,
                _ => panic!("Expected identifier for method name"),
            },
            params,
            block: Box::new(body),
            span: full_span,
        },
    ))
}

fn parse_field_decl(input: TokenSlice) -> IResult<TokenSlice, AST> {
    let (input, field_type) = parse_type(input)?;
    let (input, fields) = separated_list1(
        tag_punctuation_gen(Punctuation::Comma),
        parse_id_or_array_field_decl,
    )
    .parse(input)?;
    let (input, (_, semicolon_span)) = tag_punctuation_gen(Punctuation::Semicolon)(input)?;

    // Ensure span extends from field type to `;`
    let full_span = merge_spans(&get_span(&field_type), &semicolon_span);

    Ok((
        input,
        AST::FieldDecl {
            typ: match field_type {
                AST::Type { typ, .. } => typ,
                _ => panic!("Expected AST::Type, found {:?}", field_type),
            },
            decls: fields.into_iter().map(Box::new).collect(),
            span: full_span,
        },
    ))
}

fn parse_id_or_array_field_decl(input: TokenSlice) -> IResult<TokenSlice, AST> {
    alt((parse_array_field_decl, parse_identifier)).parse(input)
}
fn parse_program(input: TokenSlice) -> IResult<TokenSlice, AST> {
    let (input, import_decls) = many0(parse_import_decl).parse(input)?;
    let (input, field_decls) = many0(parse_field_decl).parse(input)?;
    let (input, method_decls) = many0(parse_method_decl).parse(input)?;

    // Determine the overall span from first to last token
    let first_span = import_decls
        .first()
        .map(get_span)
        .or_else(|| field_decls.first().map(get_span))
        .or_else(|| method_decls.first().map(get_span));

    let last_span = method_decls
        .last()
        .map(get_span)
        .or_else(|| field_decls.last().map(get_span))
        .or_else(|| import_decls.last().map(get_span));

    let full_span = match (first_span, last_span) {
        (Some(start), Some(end)) => merge_spans(&start, &end),
        _ => Span {
            sline: 0,
            scol: 0,
            eline: 0,
            ecol: 0,
        },
    };

    Ok((
        input,
        AST::Program {
            imports: import_decls.into_iter().map(Box::new).collect(),
            fields: field_decls.into_iter().map(Box::new).collect(),
            methods: method_decls.into_iter().map(Box::new).collect(),
            span: full_span,
        },
    ))
}

/// Input: a sequence of tokens produced by the scanner.
/// Effects:
///    - Verifies that tokens conform to valid Decaf via the language specification
///    - Outputs a syntax tree representation of the Decaf program
pub fn parse(
    file: &str,
    filename: &str,
    writer: &mut dyn std::io::Write,
    debug: bool,
) -> Option<AST> {
    let tokens: Vec<Token> = scan(file, filename, writer, false);
    let parse_result = all_consuming(parse_program).parse(TokenSlice(&tokens));

    match parse_result {
        Ok((_rest, parse_tree)) => {
            if debug {
                // run `dot -Tpng ast.dot -o ast.png` from the CLI to generate a GraphViz of the AST
                // save_dot_file(&parse_tree, "parse_ast.dot");
                let template_string = format!(
                    "===============SUCCESSFUL PARSE!===============\n Parse tree: {:#?}",
                    parse_tree
                );
                writeln!(writer, "{}", template_string).expect("Failed to write output!");
            }
            Some(parse_tree)
        }

        // Print parse error message
        // TODO: make error messages better
        Err(_parse_error) => {
            if let Some((first_token, last_token)) = tokens.first().zip(tokens.last()) {
                let first_span = extract_span_from_token(first_token);
                let last_span = extract_span_from_token(last_token);
        
                let start_line = first_span.sline;
                let start_col = first_span.scol;
                let end_line = last_span.eline;
                let end_col = last_span.ecol;
        
                // Reconstruct the invalid section from token `display` values
                let invalid_code = tokens
                    .iter()
                    .map(|token| get_token_display(token)) // Get readable token representation
                    .collect::<Vec<_>>()
                    .join(" ");
        
                let error_message = format!(
                    "~~~{} parsing error: Invalid syntax from (line {}, column {}) to (line {}, column {}):\n|\t{}\n",
                    filename, start_line, start_col, end_line, end_col,
                    invalid_code,
                );
        
                writeln!(writer, "{}", error_message).expect("Failed to write error to stdout!");
                panic!("Parsing failed.");
            } else {
                // Case where no tokens are found (completely empty or malformed input)
                writeln!(writer, "\nPARSING ERROR! No valid tokens found. Unable to determine error location.")
                    .expect("Failed to write error to stdout!");
                panic!("Parsing failed.");
            }
        }
        

    }
}
