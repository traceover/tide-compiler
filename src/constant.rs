use crate::types::TypeId;
use derive_more::{IsVariant, Unwrap};
use num_bigint::BigInt;

#[derive(Debug, Clone, IsVariant, Unwrap)]
pub enum Const {
    Int(BigInt),
    Float(f64),
    Bool(bool),
    Struct(Vec<Const>),
    Type(TypeId),
}
