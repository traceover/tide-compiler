pub mod ast;
pub mod constant;
pub mod infer;
pub mod program;
pub mod types;
pub mod bytecode;

use ast::*;
use infer::Infer;
use bytecode::{Inst, Opcode, Interp};
use program::ProgramContext;
use types::BuiltinType;

use std::cell::RefCell;
use std::rc::Rc;

fn main() {
    let mut ast = Ast::new();

    let a = ast.append(Node::Number(1.into()));
    let b = ast.append(Node::Number(2.into()));
    let c = ast.append(Node::Number(3.into()));

    let a_b = ast.append(Node::Binary(Binary::new(Oper::Add, a, b)));
    let a_b_c = ast.append(Node::Binary(Binary::new(Oper::Add, a_b, c)));

    ast.declare(Decl::with_expr("ITEM".into(), a_b_c));

    let lhs = ast.append(Node::Ident("ITEM".into()));
    let rhs = ast.append(Node::Number(2.into()));
    let bin = ast.append(Node::Binary(Binary::new(Oper::Mul, lhs, rhs)));

    let int_ty = ast.append(Node::BuiltinType(BuiltinType::Float));
    let proto = ast.append(Node::FnProto(FnProto::new(Vec::new(), int_ty)));
    let ret = ast.append(Node::Return(Some(bin)));
    let block = ast.append(Node::Block(Block::new(vec![ret])));
    let fun = ast.append(Node::FnLiteral(FnLiteral::new(proto, block)));
    ast.declare(Decl::with_expr("fizz".into(), fun));

    for decl in &ast.decls {
        println!("{} :: {}", &decl.name, ast.display(decl.root_expr));
    }
    println!();

    let program_context = Rc::new(RefCell::new(ProgramContext::new()));
    program_context.borrow_mut().insert_ast(&ast);

    let infer = match Infer::infer(program_context, &ast) {
        Ok(infer) => infer,
        Err(err) => {
            eprintln!("ERROR: {err:?}");
            std::process::exit(1);
        }
    };

    let mut interp = Interp::new(Vec::new());
    interp.program.push(Inst::new(Opcode::Call, 0, 0, 2));
    interp.program.push(Inst::new(Opcode::Exit, 0, 0, 0));
    interp.add_decls(&infer);

    for (name, label) in &interp.labels {
        println!("{} = {:?}", name, label);
    }
    println!();

    for inst in &interp.program {
        println!("{inst}");
    }
    println!();

    interp.run();

    for (reg, value) in &interp.registers {
        println!("{} = {}", reg, value);
    }
}
