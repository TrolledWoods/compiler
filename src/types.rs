use crate::compilation_manager::Identifier;
use crate::lexer::SourcePos;
use std::fmt::{self, Display, Formatter};

#[derive(Debug)]
pub struct TypeDef {
    pub pos: SourcePos,
    pub kind: TypeKind,
}

impl Display for TypeDef {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.kind)
    }
}

#[derive(Debug)]
pub enum TypeKind {
    Offload {
        name: Identifier,
        generics: Vec<TypeDef>,
    },
    Tuple(Vec<TypeDef>),
    FunctionPtr(FunctionHeader),
}

impl Display for TypeKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        use TypeKind::*;
        match self {
            Offload { name, generics } => {
                write!(f, "{}<", name.data)?;

                for (i, generic) in generics.iter().enumerate() {
                    if i != 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", generic)?;
                }

                write!(f, ">")?;
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
        }

        Ok(())
    }
}

#[derive(Debug)]
pub struct FunctionHeader {
    pub args: Vec<TypeDef>,
    pub returns: Vec<TypeDef>,
}

impl Display for FunctionHeader {
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

#[derive(Clone, Copy, Hash, PartialEq, Eq, Debug)]
pub enum TypeId {
    Struct(StructId),
}

create_id!(StructId);

#[derive(Debug)]
pub struct DefinedStruct {
    pub pos: SourcePos,
    pub members: Vec<(Identifier, TypeDef)>,
}

impl Display for DefinedStruct {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "struct (")?;
        for (i, (ident, type_def)) in self.members.iter().enumerate() {
            if i != 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}: {}", ident.data, type_def)?;
        }
        write!(f, ")")?;

        Ok(())
    }
}

pub struct ResolvedStruct {
    pub members: Vec<(Identifier, TypeId)>,
}
