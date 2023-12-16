pub mod ast;
pub mod infer;
pub mod types;

use ast::*;

fn main() {
    let mut ast = Ast::new();

    let defn = ast.append(Node::Number(69));
    let _ = ast.declare(Decl::with_expr("int".into(), defn));

    let num = ast.append(Node::Number(711));
    let _ = ast.declare(Decl::with_expr("ITEM".into(), num));

    let lhs = ast.append(Node::Ident("ITEM".into()));
    let rhs = ast.append(Node::Number(2));
    let bin = ast.append(Node::Binary(Binary::new(Oper::Mul, lhs, rhs)));

    let int_name = ast.append(Node::Ident("int".into()));
    let proto = ast.append(Node::FnProto(Vec::new(), int_name));
    let block = ast.append(Node::Block(vec![bin]));
    let fun = ast.append(Node::FnLiteral(proto, block));
    let _ = ast.declare(Decl::with_expr("fizz".into(), fun));

    let for_typing = topological_sort(&ast.decls, |decl| decl.get_type_deps(&ast));
    for decl_index in for_typing {
        println!("{}", ast.decls[decl_index].name);
    }

    /*
    for decl in &ast.decls {
        let deps = decl.get_type_deps(&ast);
        println!("{} :: {}", decl.name, ast.display(decl.root_expr));
        for dep in deps {
            println!("    {}", ast.decls[dep].name);
        }
    }
    */
}
