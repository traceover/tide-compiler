use crate::types::TypeId;
use std::ops::{Add, Sub, Mul, Div, Rem, Shl, Shr};
use derive_more::{IsVariant, Unwrap};
use num_bigint::BigInt;
use num_traits::ToPrimitive;

#[derive(Debug, Clone, IsVariant, Unwrap)]
pub enum Const {
    Int(BigInt),
    Float(f64),
    Bool(bool),
    Struct(Vec<Const>),
    Type(TypeId),
}

impl Add for Const {
    type Output = Result<Const, &'static str>;

    fn add(self, rhs: Self) -> Self::Output {
        use Const::*;
        match (self, rhs) {
            (Int(x), Int(y)) => Ok(Int(x.add(y))),
            (Float(x), Float(y)) => Ok(Float(x.add(y))),
            (Bool(_), Bool(_)) => Err("Cannot add booleans"),
            (Struct(_), Struct(_)) => Err("Cannot add structs"),
            (Type(_), Type(_)) => Err("Cannot add types"),
            _ => Err("Trying to add mismatched enum variants"),
        }
    }
}

impl Sub for Const {
    type Output = Result<Const, &'static str>;

    fn sub(self, rhs: Self) -> Self::Output {
        use Const::*;
        match (self, rhs) {
            (Int(x), Int(y)) => Ok(Int(x.sub(y))),
            (Float(x), Float(y)) => Ok(Float(x.sub(y))),
            (Bool(_), Bool(_)) => Err("Cannot subtract booleans"),
            (Struct(_), Struct(_)) => Err("Cannot subtract structs"),
            (Type(_), Type(_)) => Err("Cannot subtract types"),
            _ => Err("Trying to subtract mismatched enum variants"),
        }
    }
}

impl Mul for Const {
    type Output = Result<Const, &'static str>;

    fn mul(self, rhs: Self) -> Self::Output {
        use Const::*;
        match (self, rhs) {
            (Int(x), Int(y)) => Ok(Int(x.mul(y))),
            (Float(x), Float(y)) => Ok(Float(x.mul(y))),
            (Bool(_), Bool(_)) => Err("Cannot multiply booleans"),
            (Struct(_), Struct(_)) => Err("Cannot multiply structs"),
            (Type(_), Type(_)) => Err("Cannot multiply types"),
            _ => Err("Trying to multiply mismatched enum variants"),
        }
    }
}

impl Div for Const {
    type Output = Result<Const, &'static str>;

    fn div(self, rhs: Self) -> Self::Output {
        use Const::*;
        match (self, rhs) {
            (Int(x), Int(y)) => Ok(Int(x.div(y))),
            (Float(x), Float(y)) => Ok(Float(x.div(y))),
            (Bool(_), Bool(_)) => Err("Cannot divide booleans"),
            (Struct(_), Struct(_)) => Err("Cannot divide structs"),
            (Type(_), Type(_)) => Err("Cannot divide types"),
            _ => Err("Trying to divide mismatched enum variants"),
        }
    }
}

impl Rem for Const {
    type Output = Result<Const, &'static str>;

    fn rem(self, rhs: Self) -> Self::Output {
        use Const::*;
        match (self, rhs) {
            (Int(x), Int(y)) => Ok(Int(x.rem(y))),
            (Float(x), Float(y)) => Ok(Float(x.rem(y))),
            (Bool(_), Bool(_)) => Err("Cannot divide booleans"),
            (Struct(_), Struct(_)) => Err("Cannot divide structs"),
            (Type(_), Type(_)) => Err("Cannot divide types"),
            _ => Err("Trying to divide mismatched enum variants"),
        }
    }
}

impl Shl for Const {
    type Output = Result<Const, &'static str>;

    fn shl(self, rhs: Self) -> Self::Output {
        use Const::*;
        match (self, rhs) {
            (Int(x), Int(y)) => Ok(Int(x.shl(y.to_i128().expect("Value is too big")))),
            (Float(_), Float(_)) => Err("Cannot bit shift floats"),
            (Bool(_), Bool(_)) => Err("Cannot bit shift booleans"),
            (Struct(_), Struct(_)) => Err("Cannot bit shift structs"),
            (Type(_), Type(_)) => Err("Cannot bit shift types"),
            _ => Err("Trying to bit shift mismatched enum variants"),
        }
    }
}

impl Shr for Const {
    type Output = Result<Const, &'static str>;

    fn shr(self, rhs: Self) -> Self::Output {
        use Const::*;
        match (self, rhs) {
            (Int(x), Int(y)) => Ok(Int(x.shr(y.to_i128().expect("Value is too big")))),
            (Float(_), Float(_)) => Err("Cannot bit shift floats"),
            (Bool(_), Bool(_)) => Err("Cannot bit shift booleans"),
            (Struct(_), Struct(_)) => Err("Cannot bit shift structs"),
            (Type(_), Type(_)) => Err("Cannot bit shift types"),
            _ => Err("Trying to bit shift mismatched enum variants"),
        }
    }
}
