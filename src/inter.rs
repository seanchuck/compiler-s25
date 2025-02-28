use crate::ast::*;
use crate::parse::parse;
use crate::symtable::*;

// /// Global fields into variable declarations and methods into functions.
// pub fn ast_to_ir(ast: &AST) -> IR {
//     match ast {
//         AST::Program { imports: _, fields, methods } => {
//             let mut globals = Vec::new();
//             let mut functions = Vec::new();

//             // Process global field declarations.
//             for field in fields {
//                 if let AST::FieldDecl { typ, decls } = field.as_ref() {
//                     let datatype = convert_type(typ);
//                     for decl in decls {
//                         if let AST::Identifier(name) = decl.as_ref() {
//                             globals.push(IR::VarDecl {
//                                 name: name.clone(),
//                                 datatype: datatype.clone(),
//                             });
//                         }
//                     }
//                 }
//             }

//             // Process methods as functions.
//             for method in methods {
//                 if let AST::MethodDecl { return_type, name, params, block } = method.as_ref() {
//                     let ret_type = convert_type(return_type);
//                     let mut ir_params = Vec::new();
//                     for (ptype, pname) in params {
//                         let param_type = convert_type(ptype);
//                         ir_params.push((pname.clone(), param_type));
//                     }
//                     let body_ir = block_to_ir(block);
//                     // Create a new scope for the function.
//                     let scope = Scope::new();
//                     functions.push(IR::Function {
//                         name: name.clone(),
//                         return_type: ret_type,
//                         params: ir_params,
//                         body: body_ir,
//                         scope,
//                     });
//                 }
//             }

//             IR::Program { globals, functions }
//         },
//         _ => unimplemented!("AST to IR conversion is only implemented for Program nodes."),
//     }
// }

// /// Helper function to convert an AST Type into our IR Datatype.
// // fn convert_type(typ: &Type) -> Datatype {
// //     match typ {
// //         Type::Int => Datatype::Int,
// //         Type::Long => Datatype::Long,
// //         Type::Bool => Datatype::Bool,
// //         Type::Void => Datatype::Void, //TODO: ADD MORE TYPES LIKE CHAR AND SO ON
// //     }
// // }

// /// Convert an AST block (a scope in the AST) into a vector of IR nodes.
// fn block_to_ir(block: &AST) -> Vec<IR> {
//     match block {
//         AST::Block { field_decls, statements } => {
//             let mut ir_nodes = Vec::new();

//             // Process local variable declarations.
//             for decl in field_decls {
//                 if let AST::FieldDecl { typ, decls } = decl.as_ref() {
//                     let datatype = convert_type(typ);
//                     for d in decls {
//                         if let AST::Identifier(name) = d.as_ref() {
//                             ir_nodes.push(IR::VarDecl {
//                                 name: name.clone(),
//                                 datatype: datatype.clone(),
//                             });
//                         }
//                     }
//                 }
//             }

//             // Process statements.
//             for stmt in statements {
//                 if let AST::Statement(s) = stmt.as_ref() {
//                     let ir_stmt = statement_to_ir(s);
//                     ir_nodes.push(ir_stmt);
//                 }
//             }

//             ir_nodes
//         },
//         _ => vec![],
//     }
// }

// /// Convert an AST statement into its IR equivalent.
// /// This function handles assignments and returns; additional cases (if, while, etc.)
// /// can be added similarly.
// fn statement_to_ir(stmt: &Statement) -> IR {
//     use Statement::*;
//     match stmt {
//         Assignment { location, expr, op: _ } => {
//             // For simplicity, assume the assignment target is an identifier.
//             if let AST::Identifier(name) = location.as_ref() {
//                 let ir_expr = expr_to_ir(expr.as_ref());
//                 IR::Assign {
//                     target: name.clone(),
//                     expr: Box::new(IR::Expression(ir_expr)),
//                 }
//             } else {
//                 unimplemented!("Complex assignment locations are not implemented")
//             }
//         },
//         Return { expr } => {
//             let ir_expr = expr.as_ref().map(|e| Box::new(IR::Expression(expr_to_ir(e))));
//             IR::Return { expr: ir_expr }
//         },
//         // Additional statement kinds (If, While, For, etc.) can be handled here.
//         _ => unimplemented!("Statement conversion not implemented for this variant"),
//     }
// }

// /// Convert an AST expression into an IR expression.
// fn expr_to_ir(expr: &AST) -> IRExpr {
//     match expr {
//         AST::Expr(Expr::Literal(lit)) => IRExpr::Literal(format!("{:?}", lit)),
//         AST::Identifier(name) => IRExpr::Identifier(name.clone()),
//         AST::Expr(Expr::BinaryExpr { op, left, right }) => {
//             let left_ir = expr_to_ir(left.as_ref());
//             let right_ir = expr_to_ir(right.as_ref());
//             IRExpr::Binary {
//                 op: op.clone(),
//                 left: Box::new(left_ir),
//                 right: Box::new(right_ir),
//             }
//         },
//         AST::Expr(Expr::UnaryExpr { op, expr }) => {
//             let inner_ir = expr_to_ir(expr.as_ref());
//             IRExpr::Unary {
//                 op: op.clone(),
//                 expr: Box::new(inner_ir),
//             }
//         },
//         // Additional expression variants (e.g., method calls, casts) can be added here.
//         _ => unimplemented!("Expression conversion not implemented for this variant"),
//     }
// }

pub fn augment_ast(
    file: &str,
    filename: &str,
    writer: &mut Box<dyn std::io::Write>,
    verbose: bool,
) {
    todo!()
    // let ast: AST = parse(file, filename, writer, false).expect("inter.rs: parse failed");
    // let new_ast = ast_to_ir(&ast);
    // println!("{:#?}", new_ast);
}
