use std::collections::HashMap;
use crate::ast::Ast;
use crate::types::{TypeId, TypeTable};

/// A program context is a list of every declaration from a
/// list of all imported modules compiled together for the
/// process of linking and looking up identifiers.
pub struct ProgramContext {
    pub table: TypeTable,
    pub global_decls: HashMap<String, GlobalDecl>,
}

impl ProgramContext {
    pub fn new() -> Self {
        Self {
            table: TypeTable::new(),
            global_decls: HashMap::new(),
        }
    }

    /// Imports all declarations from the ast into
    /// the program context.
    pub fn insert_ast(&mut self, ast: &Ast) {
        for decl in &ast.decls {
            self.global_decls.insert(decl.name.clone(), GlobalDecl {
                type_id: None,
            });
        }
    }

    /// Look up an identifier in the global program context
    /// and return a reference to the ast it corresponds to
    /// and its index in the ast's list of declarations.
    pub fn lookup_decl(&self, name: &str) -> Option<&GlobalDecl> {
        self.global_decls.get(name)
    }

    pub fn lookup_decl_mut(&mut self, name: &str) -> Option<&mut GlobalDecl> {
        self.global_decls.get_mut(name)
    }
}

/// A global reference to a declaration in one of the
/// files imported by the program.
#[derive(Debug, Copy, Clone, PartialEq)]
pub struct GlobalDecl {
    pub type_id: Option<TypeId>,
}
