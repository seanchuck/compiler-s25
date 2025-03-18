use crate::semcheck::semcheck;
use crate::ast::*;
use crate::parse::parse;
use crate::symtable::*;
use crate::token::Symbol;
use crate::cfg::*;





fn destruct_statement(statement: SymStatement) -> DestructedNode {
    DestructedNode::new()

    // unary expr

    // binary expr
    //  if left and right both literal/identifier/location: return destructed node
    // otherwise: recurse on non-literal operands

}



fn destruct_program(program: SymProgram) -> DestructedNode {
    let main = program.methods.get("main").expect("Holy fuck bruh.");
    let mut output: DestructedNode::new();


    // iterate over statements, and destruct
    // update CFG output
    for statement in main.body.statements.clone() {
        let destruct_output = destruct_statement((*statement).clone());

    }

    output


}



pub fn assemble(
    file: &str,
    filename: &str,
    writer: &mut dyn std::io::Write,
    debug: bool
) {
    // Create symbol table IR
    let sym_tree: SymProgram = semcheck(file, filename, writer, debug);

    destruct_program(sym_tree);

}
