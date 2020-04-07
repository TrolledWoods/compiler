use crate::string_pile::TinyString;

pub enum Type {
    Primitive(PrimitiveKind),
    Function(FunctionHeader),
}

pub struct FunctionHeader {
    pub inputs: Vec<Type>,
    pub returns: Option<Box<Type>>,
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
