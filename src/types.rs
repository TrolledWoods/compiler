use crate::compilation_manager::{Identifier, NamedTypeId, ResolvedTypeId};
use crate::lexer::SourcePos;
use crate::misc::collect_to_vec_if_ok;
use crate::string_pile::TinyString;
use std::fmt::{self, Debug, Display, Formatter};

#[derive(Debug)]
pub struct TypeDef {
    pub pos: SourcePos,
    pub kind: TypeDefKind,
}

impl Display for TypeDef {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.kind)
    }
}
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum ResolvedTypeKind {
    Offload(NamedTypeId),
    Tuple(Vec<(usize, ResolvedTypeId)>),
    FunctionPtr(Vec<(usize, ResolvedTypeId)>, Vec<(usize, ResolvedTypeId)>),
    Struct(Vec<(usize, TinyString, ResolvedTypeId)>),
    Primitive(PrimitiveKind),
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub enum PrimitiveKind {
    I8,
    I16,
    I32,
    I64,
    U8,
    U16,
    U32,
    U64,
    F32,
    F64,
}

impl PrimitiveKind {
    pub fn get_size_and_align(self) -> (usize, usize) {
        use PrimitiveKind::*;
        match self {
            I8 => (1, 1),
            I16 => (2, 2),
            I32 => (4, 4),
            I64 => (8, 8),
            U8 => (1, 1),
            U16 => (2, 2),
            U32 => (4, 4),
            U64 => (8, 8),
            F32 => (4, 4),
            F64 => (8, 8),
        }
    }
}

pub enum TypeDefKind {
    Offload(TinyString),
    Tuple(Vec<TypeDef>),
    FunctionPtr(FunctionHeader<TypeDef>),
    Struct(Vec<(TinyString, TypeDef)>),
    Primitive(PrimitiveKind),
}

impl Debug for TypeDefKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self)
    }
}

impl Display for TypeDefKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        use TypeDefKind::*;
        match self {
            Offload(reference) => {
                write!(f, "{}<", &reference)?;

                write!(f, ">")?;
            }
            Primitive(kind) => {
                write!(f, "{:?}", kind)?;
            }
            Tuple(members) => {
                write!(f, "(")?;
                for (i, member) in members.iter().enumerate() {
                    if i != 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", member)?;
                }
                write!(f, ")")?;
            }
            FunctionPtr(header) => write!(f, "{}", header)?,
            Struct(members) => {
                write!(f, "struct (")?;
                for (i, (ident, type_def)) in members.iter().enumerate() {
                    if i != 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}: {}", ident, type_def)?;
                }
                write!(f, ")")?;
            }
        }

        Ok(())
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct FunctionHeader<T> {
    pub args: Vec<T>,
    pub returns: Vec<T>,
}

impl<T: Display> Display for FunctionHeader<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "(")?;
        for (i, member) in self.args.iter().enumerate() {
            if i != 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}", member)?;
        }
        write!(f, ") -> (")?;
        for (i, member) in self.returns.iter().enumerate() {
            if i != 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}", member)?;
        }
        write!(f, ")")?;

        Ok(())
    }
}
