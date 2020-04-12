use crate::lexer::Token;
use crate::namespace::{InsertContentError, NamespaceID, NamespaceManager, Publicity};
use crate::string_pile::TinyString;
use chashmap::CHashMap;
use std::collections::HashSet;
use std::num::NonZeroU32;
use std::sync::atomic::{AtomicU32, Ordering};

#[derive(Clone, Copy, Hash, PartialEq, Eq, Debug)]
pub enum ID {
    Function(CompileUnitID),
    Struct(CompileUnitID),
    Type(CompileUnitID),
}

pub enum CompileStages<U, F> {
    Untouched(U),
    // If the counter is down at zero,
    // we know that it's ready to be finished
    WaitingOnDependencies(u32, U),
    Finished(F),
}

type CompileUnitID = NonZeroU32;

pub struct CompileStageType<U, F> {
    id_ctr: AtomicU32,
    values: CHashMap<NonZeroU32, CompileStages<U, F>>,
}

impl<U, F> CompileStageType<U, F> {
    pub fn new() -> CompileStageType<U, F> {
        CompileStageType {
            id_ctr: AtomicU32::new(1),
            values: CHashMap::new(),
        }
    }

    pub fn insert(&self, value: U) -> CompileUnitID {
        let id = self.id_ctr.fetch_add(1, Ordering::SeqCst);
        let id = NonZeroU32::new(id).unwrap();

        let val = self.values.insert(id, CompileStages::Untouched(value));
        // If the id_ctr works, there should always
        // be a unique id for every value, so no old
        // value should exist here!
        assert!(matches!(val, None));

        id
    }
}

pub struct CompileManager {
    pub namespace: NamespaceManager<ID>,

    pub structs: CompileStageType<StructDef, Struct>,
}

impl CompileManager {
    pub fn new() -> CompileManager {
        CompileManager {
            namespace: NamespaceManager::new(),
            structs: CompileStageType::new(),
        }
    }

    pub fn insert_struct(
        &self,
        loc: NamespaceID,
        publicity: Publicity,
        name: TinyString,
        def: StructDef,
    ) -> Result<ID, InsertContentError<ID>> {
        let id = self.structs.insert(def);

        self.namespace
            .insert_member(loc, name, ID::Struct(id), publicity)?;

        Ok(ID::Struct(id))
    }
}

#[derive(Debug)]
pub struct Identifier {
    pub definition: Token,
    pub data: TinyString,
}

#[derive(Debug)]
pub struct StructDef {
    pub head: Token,
    pub members: Vec<(Identifier, Identifier)>,
}

pub struct Struct {
    pub members: Vec<(TinyString, ID)>,
}