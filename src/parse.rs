use nom;
use super::ast::*;

/*
 Input: a sequence of tokens produced by the scanner.

 Effects:
 - Verifies that tokens conform to valid Decaf via the language specification
 - Outputs a syntax tree representation of the Decaf program
*/

pub fn parse(file: &str, filename: &str, writer: &mut Box<dyn std::io::Write>) {
    // use nom for parser
    // enum for AST
    println!("PARSING");
}
