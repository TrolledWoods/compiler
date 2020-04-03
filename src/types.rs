use crate::string_pile::TinyString;

pub enum Type {
    Primitive(PrimitiveKind),
    UserDefined(TinyString),
}

pub struct FunctionHeader {
    pub args: Vec<Type>,
    pub return_type: Option<Type>,
}

pub struct Trait {
}

pub enum PrimitiveKind {
    U8,
    U16,
    U32,
    U64,
    I8,
    I16,
    I32,
    I64,
    F32,
    F64,
}
