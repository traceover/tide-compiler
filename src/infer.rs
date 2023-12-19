use crate::ast::*;
use crate::constant::Const;
use crate::types::*;
use crate::program::ProgramContext;

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use std::result;

/// A specialized Result that returns InferError.
pub type Result<T> = result::Result<T, InferError>;

pub struct Infer<'a> {
    ast: &'a Ast,
    types: HashMap<usize, TypeId>,
    constants: HashMap<usize, Const>,
    context: Rc<RefCell<ProgramContext>>,
}

impl<'a> Infer<'a> {
    pub fn new(context: Rc<RefCell<ProgramContext>>, ast: &'a Ast) -> Self {
        Self {
            ast,
            context,
            types: HashMap::new(),
            constants: HashMap::new(),
        }
    }

    pub fn infer(context: Rc<RefCell<ProgramContext>>, ast: &'a Ast) -> Result<Self> {
        let mut infer = Self::new(context, ast);
        let for_typing = topological_sort(&ast.decls, |decl| decl.get_type_deps(&ast));
        for decl_index in for_typing {
            infer.infer_decl(decl_index)?;
        }
        Ok(infer)
    }

    /// The type of an identifier is the type of the declaration it refers to.
    pub fn infer_ident(&self, ident: &String) -> Result<TypeId> {
        let context = self.context.borrow();
        let decl = context.lookup_decl(ident).expect("Undeclared identifier");
        Ok(decl.type_id.expect("Declaration not finished"))
    }

    /// All number literals get the provisional type `int`, but the process
    /// of inferring surrounding nodes can overwrite it. For example, if you
    /// write a procedure that returns the type `s32`, the expression `return 5`
    /// would compile, even though `5` would default to `int`.
    pub fn infer_number(&self, node_index: usize) -> Result<TypeId> {
        Ok(*self
            .types
            .get(&node_index)
            .unwrap_or(&(BuiltinType::Int as TypeId)))
    }

    pub fn infer_binary(&self, bin: &Binary) -> Result<TypeId> {
        match bin.op {
            Oper::Add | Oper::Sub | Oper::Mul | Oper::Div => {
                // All number types (int, float, enum?) are allowed
                if self.both_match(bin.lhs, bin.rhs, TypeInfo::is_number) {
                    self.get_matching_binary_type(bin)
                } else {
                    Err(InferError::InvalidArithmetic)
                }
            }
            Oper::Gtr | Oper::Lss | Oper::Geq | Oper::Leq => {
                // Only ordered types can be compared
                if self.both_match(bin.lhs, bin.rhs, TypeInfo::is_number) {
                    self.get_matching_binary_type(bin)
                } else {
                    Err(InferError::InvalidComparison)
                }
            }
            Oper::Rem | Oper::Shl | Oper::Shr => {
                // Only integer types are allowed
                if self.both_match(bin.lhs, bin.rhs, TypeInfo::is_integer) {
                    self.get_matching_binary_type(bin)
                } else {
                    Err(InferError::InvalidIntOp)
                }
            }
            Oper::Eql | Oper::Neq => {
                // Any type can be compared for equality
                self.get_matching_binary_type(bin)?;
                Ok(BuiltinType::Bool as TypeId)
            }
        }
    }

    /// Returns true if any side that has a type matches
    /// the specified predicate. If a side has no type,
    /// it is ignored.
    fn both_match<F>(&self, lhs: usize, rhs: usize, pred: F) -> bool
    where
        F: Fn(&TypeInfo) -> bool,
    {
        [lhs, rhs]
            .iter()
            .filter_map(|id| self.types.get(id))
            .all(|&type_id| self.context.borrow().table.get(type_id).map(&pred).unwrap_or(false))
    }

    /// Compares the sides of a binary node, returning an error
    /// if they do not match or if both have no type or the type
    /// of both sides if they match.
    fn get_matching_binary_type(&self, bin: &Binary) -> Result<TypeId> {
        match (self.types.get(&bin.lhs), self.types.get(&bin.rhs)) {
            (Some(a), Some(b)) if a != b => Err(InferError::BinaryMismatch),
            (Some(&type_id), _) => self.match_types(bin.rhs, type_id).map(|_| type_id),
            (_, Some(&type_id)) => self.match_types(bin.lhs, type_id).map(|_| type_id),
            _ => Err(InferError::CannotInferBinary),
        }
    }

    /// The type of a function literal does not depend on its body.
    /// Maybe at some point we would allow that for lambda expressions,
    /// so that the return type does not have to be specified. But,
    /// lambdas already cannot be recursive so there is no circular problem
    /// if we just block on both the signature and body expression.
    pub fn infer_fn_literal(&self, fn_lit: &FnLiteral) -> Result<TypeId> {
        Ok(self.get_type_defn_type_id(fn_lit.type_defn).unwrap())
    }

    pub fn infer_node(&mut self, node_index: usize) -> Result<TypeId> {
        // Check if we've already inferred the node before
        if let Some(type_id) = self.types.get(&node_index) {
            return Ok(*type_id);
        }

        let node = self.ast.nodes[node_index].clone();

        match node {
            Node::Ident(name) => self.infer_ident(&name),
            Node::Number(x) => {
                self.constants.insert(node_index, Const::Int(x));
                self.infer_number(node_index)
            },
            Node::Binary(inner) => self.infer_binary(&inner),
            Node::FnProto(inner) => {
                let info = self.check_fn_proto(&inner)?;
                self.set_info_for_type_defn(node_index, info);
                Ok(BuiltinType::Type as TypeId)
            }
            Node::FnLiteral(inner) => self.infer_fn_literal(&inner),
            Node::Block(stmts) => {
                for stmt in stmts {
                    self.infer_node(stmt)?;
                }
                Ok(BuiltinType::Void as TypeId)
            }
            Node::BuiltinType(builtin) => {
                self.constants.insert(node_index, Const::Type(builtin as TypeId));
                Ok(BuiltinType::Type as TypeId)
            }
        }
    }

    pub fn infer_decl(&mut self, decl_index: usize) -> Result<()> {
        let decl = self.ast.decls[decl_index].clone();
        for &node_index in &decl.nodes {
            let type_id = self.infer_node(node_index)?;
            println!(
                "{} => {}",
                self.ast.display(node_index),
                self.context.borrow().table.display(type_id)
            );
            self.types.insert(node_index, type_id);
        }
        let type_id = self.types.get(&decl.root_expr).ok_or(InferError::CannotInferDecl)?;
        self.context.borrow_mut().lookup_decl_mut(&decl.name).expect("Decl must exist").type_id = Some(*type_id);
        Ok(())
    }

    /// A function proto node is always of type `Type`,
    /// but additional validation is required to ensure
    /// the types of its parameters evaluate to type `Type`
    /// as well.
    pub fn check_fn_proto(&self, proto: &FnProto) -> Result<TypeInfo> {
        let mut params = Vec::new();
        for param in &proto.params {
            self.match_types(param.type_defn, BuiltinType::Type as TypeId)?;
            let id = self
                .get_type_defn_type_id(param.type_defn)
                .ok_or(InferError::InvalidTypeDefn)?;
            params.push((param.name.clone(), id));
        }
        let result = {
            self.match_types(proto.result, BuiltinType::Type as TypeId)?;
            self.get_type_defn_type_id(proto.result)
                .ok_or(InferError::InvalidTypeDefn)?
        };
        Ok(TypeInfo::Procedure(ProcType::new(params, result)))
    }

    /// @Document
    pub fn set_info_for_type_defn(&mut self, node_index: usize, info: TypeInfo) {
        let type_id = self.context.borrow_mut().table.append(info);
        self.constants.insert(node_index, Const::Type(type_id));
    }

    pub fn match_types(&self, expr: usize, expect: TypeId) -> Result<()> {
        if let Some(&actual) = self.types.get(&expr) {
            if actual == expect {
                return Ok(());
            }

            let table = &self.context.borrow().table;
            match (&table[actual], &table[expect]) {
                (
                    TypeInfo::Integer(IntegerType { signed: a, .. }),
                    TypeInfo::Integer(IntegerType { signed: b, .. }),
                ) if a != b => {
                    return Err(InferError::SignednessMismatch);
                }
                (TypeInfo::Integer(_), TypeInfo::Bool) => {
                    return Err(InferError::ImplicitIntToBool)
                }
                _ => {}
            }
        }

        Err(InferError::TypeMismatch)
    }

    /// Maps an type definition expression into the type
    /// id that it corresponds to. For example, if you have
    /// an expression `struct { x: int }` this function will
    /// return the type id of that struct's type info.
    pub fn get_type_defn_type_id(&self, expr: usize) -> Option<TypeId> {
        self.constants.get(&expr).and_then(|x| match x {
            Const::Type(type_id) => Some(*type_id),
            _ => None,
        })
    }
}

#[derive(Debug)]
pub enum InferError {
    TypeMismatch,
    SignednessMismatch,
    BinaryMismatch,
    InvalidArithmetic,
    InvalidComparison,
    InvalidIntOp,
    CannotInferBinary,
    CannotInferDecl,
    ImplicitIntToBool,
    InvalidTypeDefn,
}
