use crate::ast::*;
use crate::constant::Const;
use crate::program::ProgramContext;
use crate::types::*;

use num_traits::ToPrimitive;
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
    pub fn infer_ident(&mut self, node_index: NodeId, ident: &String) -> Result<TypeId> {
        let context = self.context.borrow();
        let decl = context.lookup_decl(ident).expect("Undeclared identifier");
        if let Some(constant) = &decl.constant {
            self.constants.insert(node_index, constant.clone());
        }
        Ok(decl.type_id.expect("Declaration not finished"))
    }

    /// All number literals get the provisional type `int`, but the process
    /// of inferring surrounding nodes can overwrite it. For example, if you
    /// write a procedure that returns the type `s32`, the expression `return 5`
    /// would compile, even though `5` would default to `int`.
    pub fn infer_number(&self, node_index: NodeId) -> Result<TypeId> {
        Ok(self
            .types
            .get(&node_index)
            .cloned()
            .unwrap_or(BuiltinType::Int as TypeId))
    }

    pub fn infer_binary(&mut self, bin: &Binary) -> Result<TypeId> {
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

    fn eval_binary(&self, op: Oper, lhs: Const, rhs: Const) -> result::Result<Const, &str> {
        match op {
            Oper::Add => lhs + rhs,
            Oper::Sub => lhs - rhs,
            Oper::Mul => lhs * rhs,
            Oper::Div => lhs / rhs,
            Oper::Rem => lhs % rhs,
            _ => todo!(),
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
            .all(|&type_id| {
                self.context
                    .borrow()
                    .table
                    .get(type_id)
                    .map(&pred)
                    .unwrap_or(false)
            })
    }

    /// Compares the sides of a binary node, returning an error
    /// if they do not match or if both have no type or the type
    /// of both sides if they match.
    fn get_matching_binary_type(&mut self, bin: &Binary) -> Result<TypeId> {
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
    pub fn infer_fn_literal(&mut self, fn_lit: &FnLiteral) -> Result<TypeId> {
        let type_id = self.get_type_defn_type_id(fn_lit.type_defn).unwrap();

        let proc_info = match &self.context.borrow().table[type_id] {
            TypeInfo::Procedure(proc_info) => proc_info.clone(),
            _ => return Err(InferError::InvalidFnProto),
        };

        let mut return_values = Vec::new();

        self.ast.visit(fn_lit.block, |node_index| {
            match &self.ast.nodes[node_index] {
                Node::Return(result) => return_values.push(*result),
                _ => {}
            }
        });

        for result in return_values {
            match result {
                Some(result) => self.match_types(result, proc_info.result)?,
                None => {
                    // If the procedure is not void, detect empty return
                    if !self.context.borrow().table[proc_info.result].is_void() {
                        return Err(InferError::MissingReturnValue {
                            expect: proc_info.result,
                        });
                    }
                }
            }
        }

        Ok(type_id)
    }

    /// If a block is used as an expression, its type is either void
    /// or the type of the last statement in the block (if no semicolon).
    pub fn infer_block(&self, block: &Block) -> Result<TypeId> {
        Ok(block
            .stmts
            .last()
            .and_then(|stmt| match &self.ast.nodes[*stmt] {
                Node::ImplicitReturn(result) => self.types.get(result).cloned(),
                _ => None,
            })
            .unwrap_or(BuiltinType::Void as TypeId))
    }

    pub fn infer_node(&mut self, node_index: NodeId) -> Result<TypeId> {
        // Check if we've already inferred the node before
        if let Some(type_id) = self.types.get(&node_index) {
            return Ok(*type_id);
        }

        let node = self.ast.nodes[node_index].clone(); // Clone is cheap because we use indices

        match node {
            Node::Ident(name) => self.infer_ident(node_index, &name),
            Node::Number(x) => {
                self.constants.insert(node_index, Const::Int(x.into()));
                self.infer_number(node_index)
            }
            Node::Binary(inner) => self.infer_binary(&inner).map(|res| {
                if let (Some(lhs), Some(rhs)) = (
                    self.constants.get(&inner.lhs),
                    self.constants.get(&inner.rhs),
                ) {
                    let constant = self
                        .eval_binary(inner.op, lhs.clone(), rhs.clone())
                        .unwrap();
                    self.constants.insert(node_index, constant);
                }
                res
            }),
            Node::FnProto(inner) => {
                let info = self.check_fn_proto(&inner)?;
                self.set_info_for_type_defn(node_index, info);
                Ok(BuiltinType::Type as TypeId)
            }
            Node::FnLiteral(inner) => self.infer_fn_literal(&inner),
            Node::Block(block) => self.infer_block(&block),
            Node::BuiltinType(builtin) => {
                self.constants
                    .insert(node_index, Const::Type(builtin as TypeId));
                Ok(BuiltinType::Type as TypeId)
            }
            Node::Return(_) => Ok(BuiltinType::Void as TypeId),
            Node::ImplicitReturn(x) => Ok(*self.types.get(&x).unwrap()),
        }
    }

    pub fn infer_decl(&mut self, decl_index: usize) -> Result<()> {
        let decl = self.ast.decls[decl_index].clone();
        for &node_index in &decl.nodes {
            let type_id = self.infer_node(node_index)?;
            println!(
                "INFO: {} => {}",
                self.ast.display(node_index),
                self.context.borrow().table.display(type_id)
            );
            self.types.insert(node_index, type_id);
        }
        let type_id = self
            .types
            .get(&decl.root_expr)
            .ok_or(InferError::CannotInferDecl)?;
        self.context
            .borrow_mut()
            .lookup_decl_mut(&decl.name)
            .map(|global_decl| {
                global_decl.type_id = Some(*type_id);
                global_decl.constant = self.constants.get(&decl.root_expr).cloned();
            })
            .expect("Decl must exist");
        println!(
            "INFO: {} => {}",
            decl.name,
            self.context.borrow().table.display(*type_id)
        );
        Ok(())
    }

    /// A function proto node is always of type `Type`,
    /// but additional validation is required to ensure
    /// the types of its parameters evaluate to type `Type`
    /// as well.
    pub fn check_fn_proto(&mut self, proto: &FnProto) -> Result<TypeInfo> {
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

    pub fn match_types(&mut self, node: NodeId, expect: TypeId) -> Result<()> {
        if let Some(&actual) = self.types.get(&node) {
            if actual == expect {
                return Ok(());
            }

            use TypeInfo::*;

            let table = &self.context.borrow().table;
            return match (&table[actual], &table[expect]) {
                (Integer(a), Integer(b)) if a.signed != b.signed => {
                    Err(InferError::SignednessMismatch)
                }
                (Integer(_), Bool) => {
                    Err(InferError::ImplicitIntToBool)
                }
                (Float(_), Integer(_)) => {
                    Err(InferError::ImplicitFloatToInt)
                }
                _ => {
                    if let Some(constant) = self.constants.get(&node) {
                        let constant = self.match_const_to_type(constant, expect)?;
                        self.constants.insert(node, constant);
                        Ok(())
                    } else {
                        Err(InferError::TypeMismatch { actual, expect })
                    }
                }
            };
        }

        if let Some(constant) = self.constants.get(&node) {
            let constant = self.match_const_to_type(constant, expect)?;
            self.constants.insert(node, constant);
            return Ok(());
        }

        Err(InferError::CannotInferExpr { expect })
    }

    pub fn match_const_to_type(&self, constant: &Const, expect: TypeId) -> Result<Const> {
        let info = &self.context.borrow().table[expect];
        match (constant, info) {
            (Const::Int(x), TypeInfo::Integer(IntegerType { num_bytes, signed })) => {
                let bits = x.bits()
                    + if *signed && x.sign() == num_bigint::Sign::Minus {
                        1
                    } else {
                        0
                    };
                let max_bits = (num_bytes * 8) as u64;
                if bits > max_bits {
                    return Err(InferError::IntPrecisionLoss { bits, max_bits });
                } else {
                    Ok(constant.clone())
                }
            }
            (Const::Int(x), TypeInfo::Float(_)) => Ok(Const::Float(x.to_f64().unwrap())),
            (Const::Int(_), _) => Err(InferError::ImplicitIntCast { expect }), // Int cannot cast to any other type
            (Const::Float(_), TypeInfo::Float(_)) => Ok(constant.clone()),
            (Const::Bool(_), _) => Err(InferError::ImplicitBoolCast { expect }), // Bool cannot cast to any other type
            (Const::Type(_), _) => Err(InferError::ImplicitTypeCast { expect }), // Type cannot cast to any other type
            (Const::Float(_), _) => Err(InferError::ImplicitFloatCast { expect }), // Float cannot cast to any other type
            (Const::Struct(fields), _) => {
                if let TypeInfo::Struct(struct_info) = info {
                    if fields.len() != struct_info.fields.len() {
                        return Err(InferError::ArityMismatch {
                            actual: fields.len(),
                            expect: struct_info.fields.len(),
                        });
                    }

                    fields
                        .iter()
                        .zip(struct_info.fields.iter())
                        .map(|(field, (name, expect))| {
                            self.match_const_to_type(field, *expect).map_err(|err| {
                                InferError::FieldMismatch {
                                    name: name.clone(),
                                    expect: *expect,
                                    reason: Box::new(err),
                                }
                            })
                        })
                        .collect::<Result<Vec<_>>>()
                        .map(|fields| Const::Struct(fields))
                } else {
                    Err(InferError::InvalidStructLiteral { expect })
                }
            }
        }
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
    TypeMismatch {
        actual: TypeId,
        expect: TypeId,
    },
    SignednessMismatch,
    BinaryMismatch,
    FieldMismatch {
        name: String,
        expect: TypeId,
        reason: Box<InferError>,
    },
    ArityMismatch {
        actual: usize,
        expect: usize,
    },
    InvalidArithmetic,
    InvalidComparison,
    InvalidIntOp,
    InvalidStructLiteral {
        expect: usize,
    }, // Struct literal on non-struct type
    CannotInferBinary,
    CannotInferDecl,
    CannotInferExpr {
        expect: usize,
    },
    ImplicitIntToBool,
    ImplicitFloatToInt,
    ImplicitBoolCast {
        expect: TypeId,
    },
    ImplicitFloatCast {
        expect: TypeId,
    },
    ImplicitTypeCast {
        expect: TypeId,
    },
    ImplicitIntCast {
        expect: TypeId,
    },
    IntPrecisionLoss {
        bits: u64,
        max_bits: u64,
    },
    InvalidTypeDefn,
    InvalidFnProto,
    MissingReturnValue {
        expect: TypeId,
    },
}
