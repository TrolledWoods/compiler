use crate::namespace::{NamespaceID, NamespaceManager};
use crate::string_pile::TinyString;
use chashmap::CHashMap;
use std::sync::atomic::{AtomicU32, Ordering};
use std::sync::Mutex;

#[derive(Clone, Copy, Hash, PartialEq, Eq, Debug)]
pub struct FunctionID(u32);

#[derive(Clone, Copy, Hash, PartialEq, Eq, Debug)]
pub struct TypeID(u32);

#[derive(Clone, Copy, Hash, PartialEq, Eq, Debug)]
pub enum ID {
    Function(FunctionID),
    Type(TypeID),
}

pub struct CompilationManager {
    pub namespaces: NamespaceManager<ID>,
}

impl CompilationManager {
    pub fn new() -> CompilationManager {
        let mut manager = CompilationManager {
            namespaces: NamespaceManager::new(),
        };

        manager
    }
}
