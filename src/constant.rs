use crate::types::TypeId;
use derive_more::{IsVariant, Unwrap};

#[derive(Debug, Clone, IsVariant, Unwrap)]
pub enum Const {
    Int(i64),
    Float(f64),
    Bool(bool),
    Struct(Vec<Const>),
    Type(TypeId),
}
