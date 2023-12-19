pub mod ast;
pub mod constant;
pub mod infer;
pub mod program;
pub mod types;

use ast::*;
use infer::Infer;
use program::ProgramContext;
use types::BuiltinType;

use std::cell::RefCell;
use std::rc::Rc;

fn main() {
    let mut ast = Ast::new();

    let num = ast.append(Node::Number(711.into()));
    let _ = ast.declare(Decl::with_expr("ITEM".into(), num));

    let lhs = ast.append(Node::Ident("ITEM".into()));
    let rhs = ast.append(Node::Number(2.into()));
    let bin = ast.append(Node::Binary(Binary::new(Oper::Mul, lhs, rhs)));

    let int_ty = ast.append(Node::BuiltinType(BuiltinType::Float));
    let proto = ast.append(Node::FnProto(FnProto::new(Vec::new(), int_ty)));
    let ret = ast.append(Node::Return(Some(bin)));
    let block = ast.append(Node::Block(Block::new(vec![ret])));
    let fun = ast.append(Node::FnLiteral(FnLiteral::new(proto, block)));
    let _ = ast.declare(Decl::with_expr("fizz".into(), fun));

    for decl in &ast.decls {
        println!("{} :: {}", &decl.name, ast.display(decl.root_expr));
    }

    println!();

    let program_context = Rc::new(RefCell::new(ProgramContext::new()));
    program_context.borrow_mut().insert_ast(&ast);

    let _ = Infer::infer(program_context, &ast).map_err(|err| {
        eprintln!("ERROR: {err:?}");
    });
}
