use std::ops::Index;

/// TypeId is the underlying value of a Type.
/// It is an index into the runtime type table
/// and can be used to compare types.
pub type TypeId = usize;

/// TypeInfo stores information about a Type
/// in the binary executable's runtime type table.
/// The builtin procedure `type_info` provides a way
/// to access type info from a TypeId.
pub enum TypeInfo {
    Integer(IntegerType),
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
pub struct IntegerType {
    pub num_bytes: u32,
    pub signed: bool,
}

impl IntegerType {
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
    pub params: Vec<(String, TypeId)>,
    pub result: TypeId,
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
    pub element: TypeId,
    pub slice: bool,
}

/// A fixed-size list of items with the same type.
/// If `count` is zero the type represents a
/// dynamic array.
pub struct ArrayType {
    pub element: TypeId,
    pub count: u64,
}

impl TypeInfo {
    pub fn is_number(&self) -> bool {
        matches!(self, TypeInfo::Integer(_) | TypeInfo::Float(_))
    }
}

/// Stores type information for every type in the program.
pub struct TypeTable {
    infos: Vec<TypeInfo>,
}

impl TypeTable {
    pub fn new() -> Self {
        Self { infos: Vec::new() }
    }

    pub fn get(&self, id: TypeId) -> Option<&TypeInfo> {
        self.infos.get(id)
    }

    pub fn get_builtin(&self, _name: &str) -> TypeInfo {
        todo!()
    }
}

impl Index<TypeId> for TypeTable {
    type Output = TypeInfo;

    fn index(&self, type_id: TypeId) -> &Self::Output {
        &self.infos[type_id]
    }
}
