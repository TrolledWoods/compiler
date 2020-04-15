use crate::id::CIdMap;
use crate::lexer::{SourcePos, Token};
use crate::namespace::{AllowAmbiguity, NamespaceError, NamespaceId, NamespaceManager};
use crate::string_pile::TinyString;
// TODO: Move the type stuff into types.rs
use crate::types::TypeDef;
use chashmap::CHashMap;
use std::collections::HashSet;
use std::fmt::{self, Display, Formatter};
use std::num::NonZeroU32;
use std::sync::atomic::{AtomicU32, Ordering};

pub struct CompileManager {
    pub namespace_manager: NamespaceManager,

    pub structs: CIdMap<StructId, StructCompilationUnit>,
}

impl CompileManager {
    pub fn new() -> CompileManager {
        CompileManager {
            namespace_manager: NamespaceManager::new(),
            structs: CIdMap::new(),
        }
    }

    pub fn insert_struct(
        &self,
        namespace_id: NamespaceId,
        identifier: Identifier,
        def: DefinedStruct,
    ) -> Result<StructId, NamespaceError> {
        let struct_id = self
            .structs
            .insert(StructCompilationUnit::Defined(def, Vec::new()));

        self.namespace_manager.insert_member(
            namespace_id,
            identifier,
            Id::Type(TypeId::Struct(struct_id)),
            AllowAmbiguity::Deny,
        )?;

        Ok(struct_id)
    }
}

#[derive(Clone, Copy, Hash, PartialEq, Eq, Debug)]
pub enum Id {
    Type(TypeId),
}

#[derive(Clone, Copy, Hash, PartialEq, Eq, Debug)]
pub enum TypeId {
    Struct(StructId),
}

pub enum StructCompilationUnit {
    Defined(DefinedStruct, Vec<Id>),
    Resolved(ResolvedStruct),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Identifier {
    pub pos: SourcePos,
    pub data: TinyString,
}

create_id!(StructId);

#[derive(Debug)]
pub struct DefinedStruct {
    pub head: Token,
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
