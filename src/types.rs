use crate::compilation_manager::{
    CompilationUnit, CompilationUnitId, CompileManager, CompileManagerError, Identifier,
};
use crate::lexer::SourcePos;
use crate::misc::collect_to_vec_if_ok;
use crate::namespace::NamespaceId;
use crate::string_pile::TinyString;
use std::fmt::{self, Debug, Display, Formatter};

#[derive(Debug)]
pub struct TypeDef {
    pub pos: SourcePos,
    pub kind: TypeDefKind,
}

impl TypeDef {
    pub fn get_dependencies<E>(
        &self,
        mut on_find_dependency: &mut impl FnMut(TinyString) -> Result<(), E>,
    ) -> Result<(), E> {
        use TypeDefKind::*;
        match &self.kind {
            Offload(name) => on_find_dependency(*name)?,
            Tuple(members) => {
                for member in members {
                    member.get_dependencies(on_find_dependency)?;
                }
            }
            FunctionPtr(header) => unimplemented!(),
            Struct(members) => {
                for (_, member) in members {
                    member.get_dependencies(on_find_dependency)?;
                }
            }
            Primitive(kind) => (),
        }

        Ok(())
    }
}

impl Display for TypeDef {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.kind)
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

create_id!(ResolvedTypeId);
create_id!(NamedTypeId);

pub enum TypeId {
    Named(NamedTypeId),
    Resolved(ResolvedTypeId),
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct ResolvedTypeDef {
    pub size: usize,
    pub align: usize,
    pub kind: ResolvedTypeKind,
}

/// Inserts a resolved and "finished" type
/// into a compilation manager and returns an
/// id to it. If there already exists an identical
/// type in the manager though, the id of that
/// will be returned instead
pub fn insert_resolved_type(
    manager: &CompileManager,
    definition: ResolvedTypeDef,
) -> ResolvedTypeId {
    // Look for an existing version
    let mut lock = manager.unnamed_types.lock().unwrap();
    if let Some(id) = lock.get(&definition) {
        return *id;
    }

    println!("Inserting new type");
    println!("    def: {:?}", &definition);
    let key = definition.clone();
    let id = manager.resolved_types.insert(definition);
    lock.insert(key, id);

    println!("    id: {:?}", id);

    id
}

/// Figures out the size and align of a type
pub fn size_type_def(
    manager: &CompileManager,
    namespace_id: NamespaceId,
    type_def: &TypeDef,
) -> Result<(usize, usize, ResolvedTypeId), CompileManagerError> {
    match &type_def.kind {
        TypeDefKind::Offload(reference) => {
            let (member_pos, id) = manager
                .namespace_manager
                .get_member(namespace_id, *reference)
                .unwrap();

            let (named_type_id, named_type) =
                manager.get_named_type_or(id, |window| CompileManagerError::ExpectedType {
                    pos: type_def.pos.clone(),
                    reference_pos: window.pos,
                })?;

            let (size, align, _) = named_type.fully_sized.done().expect(
                "Cannot size a type definition if the types it depends on aren't resolved either",
            );

            let id = insert_resolved_type(
                manager,
                ResolvedTypeDef {
                    size: *size,
                    align: *align,
                    kind: ResolvedTypeKind::Offload(named_type_id),
                },
            );

            Ok((*size, *align, id))
        }
        TypeDefKind::Tuple(members) => {
            let mut resolved_members = Vec::with_capacity(members.len());
            let mut size = 0;
            let mut align = 0;
            for member in members {
                let (member_size, member_align, new_member) =
                    size_type_def(manager, namespace_id, member)?;
                // :^> best align system ever!!!
                if member_align != 0 {
                    if member_align >= align {
                        align = member_align;
                    }

                    while size % member_align != 0 {
                        size += 1;
                    }
                }
                resolved_members.push((size, new_member));
                size += member_size;
            }

            let id = insert_resolved_type(
                manager,
                ResolvedTypeDef {
                    size,
                    align,
                    kind: ResolvedTypeKind::Tuple(resolved_members),
                },
            );

            Ok((size, align, id))
        }
        TypeDefKind::Struct(members) => {
            let mut resolved_members = Vec::with_capacity(members.len());
            let mut size = 0;
            let mut align = 0;

            for (name, member) in members {
                let (member_size, member_align, new_member) =
                    size_type_def(manager, namespace_id, member)?;
                // :^> best align system ever!!!
                if member_align != 0 {
                    if member_align >= align {
                        align = member_align;
                    }

                    while size % member_align != 0 {
                        size += 1;
                    }
                }
                resolved_members.push((size, *name, new_member));
                size += member_size;
            }

            let id = insert_resolved_type(
                manager,
                ResolvedTypeDef {
                    size,
                    align,
                    kind: ResolvedTypeKind::Struct(resolved_members),
                },
            );

            Ok((size, align, id))
        }
        TypeDefKind::Primitive(primitive_kind) => {
            let (size, align) = primitive_kind.get_size_and_align();

            let id = insert_resolved_type(
                manager,
                ResolvedTypeDef {
                    size,
                    align,
                    kind: ResolvedTypeKind::Primitive(*primitive_kind),
                },
            );

            Ok((size, align, id))
        }
        _ => unimplemented!(),
    }
}
