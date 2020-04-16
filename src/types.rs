use crate::compilation_manager::Identifier;
use crate::lexer::SourcePos;
use crate::misc::collect_to_vec_if_ok;
use crate::string_pile::TinyString;
use std::fmt::{self, Debug, Display, Formatter};

#[derive(Debug)]
pub struct TypeDef {
    pub pos: SourcePos,
    pub kind: TypeKind<TypeDef, TinyString>,
}

impl Display for TypeDef {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.kind)
    }
}

pub enum TypeKind<T, Extern> {
    Offload { reference: Extern, generics: Vec<T> },
    Tuple(Vec<T>),
    FunctionPtr(FunctionHeader<T>),
    Struct(Vec<(TinyString, T)>),
}

impl<T, Extern> TypeKind<T, Extern> {
    pub fn convert<NewT, NewExtern, E>(
        self,
        convert_type: impl Fn(T) -> Result<NewT, E>,
        convert_extern: impl Fn(Extern) -> Result<NewExtern, E>,
    ) -> Result<TypeKind<NewT, NewExtern>, E> {
        use TypeKind::*;
        match self {
            Offload {
                reference,
                generics,
            } => Ok(Offload {
                reference: convert_extern(reference)?,
                generics: collect_to_vec_if_ok(generics.into_iter().map(convert_type))?,
            }),
            Tuple(vec) => Ok(Tuple(collect_to_vec_if_ok(
                vec.into_iter().map(convert_type),
            )?)),
            FunctionPtr(header) => Ok(FunctionPtr(FunctionHeader {
                args: collect_to_vec_if_ok(header.args.into_iter().map(&convert_type))?,
                returns: collect_to_vec_if_ok(header.returns.into_iter().map(convert_type))?,
            })),
            Struct(_) => unimplemented!(),
        }
    }
}

impl<T: Display, Extern: Display> Debug for TypeKind<T, Extern> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self)
    }
}

impl<T: Display, Extern: Display> Display for TypeKind<T, Extern> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        use TypeKind::*;
        match self {
            Offload {
                reference,
                generics,
            } => {
                write!(f, "{}<", &reference)?;

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

#[derive(Debug)]
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
