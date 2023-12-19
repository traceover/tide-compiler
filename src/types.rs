use std::collections::HashMap;
use std::convert::TryFrom;
use std::fmt;
use std::ops::Index;
use std::str::FromStr;

use derive_more::{Constructor, Display, IsVariant, Unwrap};
use num_enum::TryFromPrimitive;

/// TypeId is the underlying value of a Type.
/// It is an index into the runtime type table
/// and can be used to compare types.
pub type TypeId = usize;

/// TypeInfo stores information about a Type
/// in the binary executable's runtime type table.
/// The builtin procedure `type_info` provides a way
/// to access type info from a TypeId.
#[derive(Debug, Clone, IsVariant, Unwrap)]
pub enum TypeInfo {
    Integer(IntegerType),
    Float(FloatType),
    Bool,
    String,
    Void,
    Procedure(ProcType),
    Pointer(TypeId),
    Slice(TypeId),
    Array(ArrayType),
    Struct(StructType),
    Type,
}

/// Represents common types such as `int` or `i64`.
#[derive(Debug, Copy, Clone, Constructor, PartialEq)]
pub struct IntegerType {
    pub num_bytes: u32,
    pub signed: bool,
}

impl fmt::Display for IntegerType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let prefix = if self.signed { "s" } else { "u" };
        write!(f, "{}{}", prefix, self.num_bytes * 8)
    }
}

/// Unlike `IntType`, we do not allow an arbitrary
/// number of bytes for precision, and instead have
/// a predefined list of allowed widths.
#[derive(Debug, Copy, Clone, PartialEq)]
pub enum FloatType {
    Half,
    Float,
    Double,
    Fp128,
}

/// A procedure type is the calling contract of
/// a procedure, defining the arity and types
/// of arguments to be passed, as well as the result.
#[derive(Debug, Clone, Constructor)]
pub struct ProcType {
    pub params: Vec<(String, TypeId)>,
    pub result: TypeId,
}

impl ProcType {
    /// Constructs a new ProcType using parallel arrays
    /// as the list of parameters instead of a vector of tuples.
    pub fn from_arrays(names: &[String], types: &[TypeId], result: TypeId) -> Self {
        let params = names.iter().cloned().zip(types.iter().copied()).collect();
        Self::new(params, result)
    }
}

/// A fixed-size list of items with the same type.
/// If `count` is zero the type represents a
/// dynamic array.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct ArrayType {
    pub element: TypeId,
    pub count: u64,
}

/// A struct is a collection of named items grouped
/// together to encapsulate shared data or state.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct StructType {
    pub fields: Vec<(String, TypeId)>,
}

impl TypeInfo {
    pub fn is_number(&self) -> bool {
        matches!(self, TypeInfo::Integer(_) | TypeInfo::Float(_))
    }
}

/// Stores type information for every type in the program.
pub struct TypeTable {
    infos: Vec<TypeInfo>,

    // All of these are used for deduplication:
    pointer_types: HashMap<TypeId, TypeId>, // Maps element type to pointer type id
    slice_types: HashMap<TypeId, TypeId>,   // Maps element type to slice type id
    array_types: HashMap<ArrayType, TypeId>, // Maps element type and array length to array type id
}

impl TypeTable {
    pub fn new() -> Self {
        Self {
            infos: Vec::from(BUILTIN_TYPE_INFOS),
            pointer_types: HashMap::new(),
            slice_types: HashMap::new(),
            array_types: HashMap::new(),
        }
    }

    /// Adds type info to the table and returns the new id,
    /// deduplicating pointer, slice, array, and procedure types.
    pub fn append(&mut self, info: TypeInfo) -> TypeId {
        use TypeInfo::*;

        // Check if the type already exists in the table
        let dup_type_id = match &info {
            Pointer(element) => self.pointer_types.get(element),
            Slice(element) => self.slice_types.get(element),
            Array(element) => self.array_types.get(element),
            _ => None,
        };

        if let Some(&type_id) = dup_type_id {
            return type_id;
        }

        // Type doesn't exist in the maps, so insert it
        let new_type_id = self.insert(info.clone());

        match info {
            Pointer(element) => self.pointer_types.insert(element, new_type_id),
            Slice(element) => self.slice_types.insert(element, new_type_id),
            Array(array) => self.array_types.insert(array, new_type_id),
            _ => None,
        };

        new_type_id
    }

    /// Inserts a new type info into the table without
    /// performing any deduplication as is performed by
    /// the public api function `append`.
    fn insert(&mut self, info: TypeInfo) -> TypeId {
        let result = self.infos.len() as TypeId;
        self.infos.push(info);
        result
    }

    /// Lookup a type by its id in the table.
    pub fn get(&self, id: TypeId) -> Option<&TypeInfo> {
        self.infos.get(id)
    }

    pub fn display(&self, id: TypeId) -> String {
        if let Ok(builtin) = BuiltinType::try_from(id) {
            return format!("{builtin}");
        }

        match &self.infos[id] {
            TypeInfo::Integer(x) => format!("{}", x),
            TypeInfo::Float(f) => match f {
                FloatType::Half => "float16".into(),
                FloatType::Float => "float".into(),
                FloatType::Double => "float64".into(),
                FloatType::Fp128 => "float128".into(),
            },
            TypeInfo::Bool => "bool".into(),
            TypeInfo::String => "string".into(),
            TypeInfo::Void => "void".into(),
            TypeInfo::Procedure(proc) => {
                let params = proc
                    .params
                    .iter()
                    .map(|(name, type_id)| format!("{}: {}", name, self.display(*type_id)))
                    .collect::<Vec<_>>()
                    .join(", ");
                let result = self.display(proc.result);
                format!("({}) -> {}", params, result)
            }
            TypeInfo::Pointer(element) => format!("*{}", self.display(*element)),
            TypeInfo::Slice(element) => format!("[]{}", self.display(*element)),
            TypeInfo::Array(arr) => match arr.count {
                0 => format!("[..]{}", self.display(arr.element)),
                _ => format!("[{}]{}", arr.count, self.display(arr.element)),
            },
            TypeInfo::Struct(struc) => {
                let fields = struc
                    .fields
                    .iter()
                    .map(|(name, type_id)| format!("{}: {}", name, self.display(*type_id)))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("struct {{ {} }}", fields)
            }
            TypeInfo::Type => "Type".into(),
        }
    }
}

impl Index<TypeId> for TypeTable {
    type Output = TypeInfo;

    fn index(&self, type_id: TypeId) -> &Self::Output {
        &self.infos[type_id]
    }
}

/// A list of all the primitive, predefined types
/// available in the language without any imports.
#[derive(Debug, Copy, Clone, PartialEq, Display, TryFromPrimitive)]
#[repr(usize)]
pub enum BuiltinType {
    #[display(fmt = "int")]
    Int,
    #[display(fmt = "uint")]
    Uint,
    #[display(fmt = "float")]
    Float,
    #[display(fmt = "string")]
    String,
    #[display(fmt = "bool")]
    Bool,
    #[display(fmt = "void")]
    Void,
    #[display(fmt = "Type")]
    Type,
    #[display(fmt = "s8")]
    S8,
    #[display(fmt = "s16")]
    S16,
    #[display(fmt = "s32")]
    S32,
    #[display(fmt = "s64")]
    S64,
    #[display(fmt = "u8")]
    U8,
    #[display(fmt = "u16")]
    U16,
    #[display(fmt = "u32")]
    U32,
    #[display(fmt = "u64")]
    U64,
}

const BUILTIN_TYPE_INFOS: [TypeInfo; 15] = [
    TypeInfo::Integer(IntegerType {
        num_bytes: 8,
        signed: true,
    }),
    TypeInfo::Integer(IntegerType {
        num_bytes: 8,
        signed: false,
    }),
    TypeInfo::Float(FloatType::Float),
    TypeInfo::String,
    TypeInfo::Bool,
    TypeInfo::Void,
    TypeInfo::Type,
    TypeInfo::Integer(IntegerType {
        num_bytes: 1,
        signed: true,
    }),
    TypeInfo::Integer(IntegerType {
        num_bytes: 2,
        signed: true,
    }),
    TypeInfo::Integer(IntegerType {
        num_bytes: 4,
        signed: true,
    }),
    TypeInfo::Integer(IntegerType {
        num_bytes: 8,
        signed: true,
    }),
    TypeInfo::Integer(IntegerType {
        num_bytes: 1,
        signed: false,
    }),
    TypeInfo::Integer(IntegerType {
        num_bytes: 2,
        signed: false,
    }),
    TypeInfo::Integer(IntegerType {
        num_bytes: 4,
        signed: false,
    }),
    TypeInfo::Integer(IntegerType {
        num_bytes: 8,
        signed: false,
    }),
];

impl FromStr for BuiltinType {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "int" => Ok(Self::Int),
            "uint" => Ok(Self::Uint),
            "float" => Ok(Self::Float),
            "string" => Ok(Self::String),
            "bool" => Ok(Self::Bool),
            "void" => Ok(Self::Void),
            "s8" => Ok(Self::S8),
            "s16" => Ok(Self::S16),
            "s32" => Ok(Self::S32),
            "s64" => Ok(Self::S64),
            "u8" => Ok(Self::U8),
            "u16" => Ok(Self::U16),
            "u32" => Ok(Self::U32),
            "u64" => Ok(Self::U64),
            _ => Err(()),
        }
    }
}
