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
            '+' | '-' | '*' | '/' => {
                // Check that both sides are numbers
                let lhs = self.types.get(&bin.lhs);
                let rhs = self.types.get(&bin.rhs);

                let lhs_is_number = lhs
                    .and_then(|&id| self.table.borrow().get(id).map(TypeInfo::is_number))
                    .unwrap_or(false);
                let rhs_is_number = rhs
                    .and_then(|&id| self.table.borrow().get(id).map(TypeInfo::is_number))
                    .unwrap_or(false);

                if !lhs_is_number || !rhs_is_number {
                    return Err(InferError::InvalidArithmetic);
                }

                match (lhs, rhs) {
                    (Some(a), Some(b)) if a != b => Err(InferError::BinaryMismatch),
                    (Some(&type_id), _) => self.match_types(bin.rhs, type_id).map(|_| type_id),
                    (_, Some(&type_id)) => self.match_types(bin.lhs, type_id).map(|_| type_id),
                    _ => Err(InferError::CannotDetermineBinaryType),
                }
            }
            '%' => {
                // self.ensure_both_match(bin.lhs, bin.rhs, |a| matches!(a, TypeInfo::Integer(_)))?;
                todo!()
            }
            _ => todo!(),
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
    CannotDetermineBinaryType,
    ImplicitIntToBool,
}
