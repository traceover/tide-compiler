/// TypeId is the underlying value of a Type.
/// It is an index into the runtime type table
/// and can be used to compare types.
pub type TypeId = usize;

/// TypeInfo stores information about a Type
/// in the binary executable's runtime type table.
/// The builtin procedure `type_info` provides a way
/// to access type info from a TypeId.
pub enum TypeInfo {
    Int(IntType),
    Float(FloatType),
    Bool,
    String,
    Void,
    Procedure(ProcType),
    Pointer(PointerType),
    Array(ArrayType),
    Type,
}

/// Represents common types such as `int` or `i64`.
pub struct IntType {
    num_bytes: u32,
    signed: bool,
}

impl IntType {
    pub fn new(num_bytes: u32, signed: bool) -> Self {
        Self { num_bytes, signed }
    }
}

/// Unlike `IntType`, we do not allow an arbitrary
/// number of bytes for precision, and instead have
/// a predefined list of allowed widths.
pub enum FloatType {
    Half,
    Float,
    Double,
    Fp128,
}

/// A procedure type is the calling contract of
/// a procedure, defining the arity and types
/// of arguments to be passed, as well as the result.
pub struct ProcType {
    params: Vec<(String, TypeId)>,
    result: TypeId,
}

impl ProcType {
    pub fn new(params: Vec<(String, TypeId)>, result: TypeId) -> Self {
        Self { params, result }
    }

    /// Constructs a new ProcType using parallel arrays
    /// as the list of parameters instead of a vector of tuples.
    pub fn from_arrays(names: &[String], types: &[TypeId], result: TypeId) -> Self {
        let params = names.iter().cloned().zip(types.iter().copied()).collect();
        Self::new(params, result)
    }
}

/// A pointer type defines the element or
/// `pointee` type of a memory address.
/// It can also represent the type of a slice
/// into an array.
pub struct PointerType {
    element: TypeId,
    slice: bool,
}

/// A fixed-size list of items with the same type.
/// If `count` is zero the type represents a
/// dynamic array.
pub struct ArrayType {
    element: TypeId,
    count: u64,
}
