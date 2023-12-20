pub mod ast;
pub mod constant;
pub mod infer;
pub mod program;
pub mod types;
pub mod bytecode;
pub mod lexer;

use ast::*;
use infer::Infer;
use bytecode::{Inst, Opcode, Interp};
use program::ProgramContext;
use types::BuiltinType;
use lexer::{Token, Lexer};

use std::fs;
use std::env;
use std::cell::RefCell;
use std::rc::Rc;
use std::process::exit;
use std::path::Path;

fn main() {
    let program_context = Rc::new(RefCell::new(ProgramContext::new()));

    let mut interp = Interp::new(Vec::new());
    interp.program.push(Inst::new(Opcode::Call, 0, 0, 2));
    interp.program.push(Inst::new(Opcode::Exit, 0, 0, 0));

    for file_path in env::args().skip(1) {
        let contents = fs::read_to_string(&file_path).expect(&format!("Failed to read file: {file_path}"));
        let mut ast = Ast::new();
        let mut node = ast.append(Node::Number(0.into()));

        for (token, loc) in Lexer::new(contents.chars().collect(), Some(file_path.clone())) {
            println!("{loc}: {token:?}");
            match token {
                Token::IntLit(x) => {
                    let n = ast.append(Node::Number(x));
                    node = ast.append(Node::Binary(Binary::new(Oper::Add, node, n)));
                },
                _ => {},
            }
        }

        let int_ty = ast.append(Node::BuiltinType(BuiltinType::Int));
        let proto = ast.append(Node::FnProto(FnProto::new(Vec::new(), int_ty)));
        let ret = ast.append(Node::Return(Some(node)));
        let block = ast.append(Node::Block(Block::new(vec![ret])));
        let fun = ast.append(Node::FnLiteral(FnLiteral::new(proto, block)));
        ast.declare(Decl::with_expr(get_file_name(Path::new(&file_path)).expect("Invalid file path"), fun));

        program_context.borrow_mut().insert_ast(&ast);

        let infer = match Infer::infer(program_context.clone(), &ast) {
            Ok(infer) => infer,
            Err(err) => {
                eprintln!("ERROR: {err:?}");
                exit(1);
            }
        };
        interp.add_decls(&infer);
    }

    interp.run();

    for (reg, value) in &interp.registers {
        println!("{} = {}", reg, value);
    }
}

fn get_file_name(path: &Path) -> Option<String> {
    path.file_stem().map(|path| path.to_string_lossy().to_string())
}
