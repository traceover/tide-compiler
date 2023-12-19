pub mod ast;
pub mod constant;
pub mod infer;
pub mod types;
pub mod program;

use ast::*;
use types::BuiltinType;
use infer::Infer;
use program::ProgramContext;

use std::cell::RefCell;
use std::rc::Rc;

fn main() {
    let mut ast = Ast::new();

    let num = ast.append(Node::Number(711));
    let _ = ast.declare(Decl::with_expr("ITEM".into(), num));

    let lhs = ast.append(Node::Ident("ITEM".into()));
    let rhs = ast.append(Node::Number(2));
    let bin = ast.append(Node::Binary(Binary::new(Oper::Mul, lhs, rhs)));

    let int_ty = ast.append(Node::BuiltinType(BuiltinType::Int));
    let proto = ast.append(Node::FnProto(FnProto::new(Vec::new(), int_ty)));
    let block = ast.append(Node::Block(vec![bin]));
    let fun = ast.append(Node::FnLiteral(FnLiteral::new(proto, block)));
    let _ = ast.declare(Decl::with_expr("fizz".into(), fun));

    let program_context = Rc::new(RefCell::new(ProgramContext::new()));
    program_context.borrow_mut().insert_ast(&ast);

    let _ = Infer::infer(program_context, &ast);
}
