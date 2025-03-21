/*
Linearize the Parse Tree to create a linear 
IR that can be used for code generation.

The linear IR is an ordered vector of instructions,
which follow similarly to the three-address code format.
*/

use crate::symtable::{SymProgram, SymStatement};
use crate::linear_ir::Instruction;
use crate::semcheck::semcheck;


fn destruct_statement(statement: SymStatement) -> Vec<Instruction> {
    let mut output: Vec<Instruction> = vec![];

    // unary expr

    // binary expr
    //  if left and right both literal/identifier/location: return destructed node
    // otherwise: recurse on non-literal operands

    output

}



fn destruct_program(program: SymProgram) -> Vec<Instruction> {
    let main = program.methods.get("main").expect("Holy fuck bruh.");
    let mut output: Vec<Instruction> = vec![];

    // iterate over statements, and destruct
    // update CFG output
    for statement in main.body.statements.clone() {

        // Create IR for all children nodes before creating IR for parent
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
