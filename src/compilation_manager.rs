use crate::id::CIdMap;
use crate::lexer::SourcePos;
use crate::namespace::{AllowAmbiguity, NamespaceError, NamespaceId, NamespaceManager};
use crate::string_pile::TinyString;
use crate::types::{DefinedStruct, ResolvedStruct, StructId, TypeId};

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

pub enum StructCompilationUnit {
    Defined(DefinedStruct, Vec<Id>),
    Resolved(ResolvedStruct),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Identifier {
    pub pos: SourcePos,
    pub data: TinyString,
}
