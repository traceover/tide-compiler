use crate::ast::*;
use crate::types::*;

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use std::result;

/// A specialized Result that returns InferError.
pub type Result<T> = result::Result<T, InferError>;

pub struct Infer<'a> {
    ast: &'a Ast,
    types: HashMap<usize, TypeId>,
    table: Rc<RefCell<TypeTable>>,
}

impl<'a> Infer<'a> {
    pub fn infer_binary(&self, bin: &Binary) -> Result<TypeId> {
        match bin.op {
            Oper::Add | Oper::Sub | Oper::Mul | Oper::Div => {
                // All number types (int, float, enum?) are allowed
                if self.both_match(bin.lhs, bin.rhs, TypeInfo::is_number) {
                    self.get_matching_binary_type(bin)
                } else {
                    Err(InferError::InvalidArithmetic)
                }
            },
            Oper::Gtr | Oper::Lss | Oper::Geq | Oper::Leq => {
                // Only ordered types can be compared
                if self.both_match(bin.lhs, bin.rhs, TypeInfo::is_number) {
                    self.get_matching_binary_type(bin)
                } else {
                    Err(InferError::InvalidComparison)
                }
            },
            Oper::Rem | Oper::Shl | Oper::Shr => {
                // Only integer types are allowed
                if self.both_match(bin.lhs, bin.rhs, TypeInfo::is_integer) {
                    self.get_matching_binary_type(bin)
                } else {
                    Err(InferError::InvalidIntOp)
                }
            },
            Oper::Eql | Oper::Neq => {
                // Any type can be compared for equality
                self.get_matching_binary_type(bin)?;
                Ok(self.table.borrow().get_builtin("bool"))
            },
        }
    }

    /// Returns true if any side that has a type matches
    /// the specified predicate. If a side has no type,
    /// it is ignored.
    fn both_match<F>(&self, lhs: usize, rhs: usize, pred: F) -> bool
    where
        F: Fn(&TypeInfo) -> bool,
    {
        [lhs, rhs].iter()
            .filter_map(|id| self.types.get(id))
            .all(|&type_id| {
                self.table.borrow().get(type_id)
                    .map(&pred)
                    .unwrap_or(false)
            })
    }

    /// Compares the sides of a binary node, returning an error
    /// if they do not match or if both have no type or the type
    /// of both sides if they match.
    fn get_matching_binary_type(&self, bin: &Binary) -> Result<TypeId> {
        match (self.types.get(&bin.lhs), self.types.get(&bin.rhs)) {
            (Some(a), Some(b)) if a != b => Err(InferError::BinaryMismatch),
            (Some(&type_id), _) => self.match_types(bin.rhs, type_id).map(|_| type_id),
            (_, Some(&type_id)) => self.match_types(bin.lhs, type_id).map(|_| type_id),
            _ => Err(InferError::CannotDetermineBinaryType),
        }
    }

    pub fn match_types(&self, expr: usize, expect: TypeId) -> Result<()> {
        if let Some(&actual) = self.types.get(&expr) {
            if actual == expect {
                return Ok(());
            }

            let table = self.table.borrow();
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
}

pub enum InferError {
    TypeMismatch,
    SignednessMismatch,
    BinaryMismatch,
    InvalidArithmetic,
    InvalidComparison,
    InvalidIntOp,
    CannotDetermineBinaryType,
    ImplicitIntToBool,
}
