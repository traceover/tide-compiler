pub mod ast;
pub mod bytecode;
pub mod constant;
pub mod infer;
pub mod lexer;
pub mod parser;
pub mod program;
pub mod types;

use bytecode::{Inst, Interp, Opcode};
use infer::Infer;
use lexer::Lexer;
use parser::Parser;
use program::ProgramContext;

extern crate derive_more;
use std::cell::RefCell;
use std::env;
use std::process::exit;
use std::rc::Rc;

fn main() {
    let program_context = Rc::new(RefCell::new(ProgramContext::new()));

    let mut interp = Interp::new(Vec::new());
    interp.program.push(Inst::new(Opcode::Call, 0, 0, 2));
    interp.program.push(Inst::new(Opcode::Exit, 0, 0, 0));

    for file_path in env::args().skip(1) {
        let lexer = Lexer::from_file_path(file_path).expect("Failed to read file: {file_path}");

        let ast = Parser::parse(lexer).unwrap_or_else(|err| {
            eprintln!("ERROR: {err:?}");
            exit(1);
        });
        program_context.borrow_mut().insert_ast(&ast);

        let infer = Infer::infer(program_context.clone(), &ast).unwrap_or_else(|err| {
            eprintln!("ERROR: {err:?}");
            exit(1);
        });

        interp.add_decls(&infer);
    }

    interp.run();

    for (reg, value) in &interp.registers {
        println!("{} = {}", reg, value);
    }
}
