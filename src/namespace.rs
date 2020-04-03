use crate::string_pile::TinyString;
use chashmap::CHashMap;
use std::sync::atomic::{ AtomicU32, Ordering };
use std::num::NonZeroU32;

#[derive(Clone, Copy, Hash, PartialEq, Eq, Debug)]
pub struct NamespaceID(NonZeroU32);

enum NamespaceContent<T> {
    Namespace(NamespaceID),
    Other(T),
}

pub struct Namespace<T> {
    parent: Option<NamespaceID>,
    contents: CHashMap<TinyString, (Publicity, NamespaceContent<T>)>,

    named_exports: CHashMap<TinyString, (ExportMode, Publicity, NamespaceID)>,
    wildcard_exports: CHashMap<TinyString, (ExportMode, Publicity, NamespaceID)>,
}

pub struct NamespaceManager<T> {
    namespace_id_ctr: AtomicU32,
    namespaces: CHashMap<NamespaceID, Namespace<T>>,
}

impl<T> NamespaceManager<T> {
    pub fn new() -> NamespaceManager<T> {
        NamespaceManager {
            namespace_id_ctr: AtomicU32::new(1),
            namespaces: CHashMap::new(),
        }
    }

    fn allocate_id(&self) -> NamespaceID {
        let id = self.namespace_id_ctr.fetch_add(1, Ordering::SeqCst);

        // Pretty sure that the id won't be the wrong type
        NamespaceID(NonZeroU32::new(id).unwrap())
    }

    pub fn create_root(&self) -> NamespaceID {
        let id = self.allocate_id();

        let namespace = Namespace {
            parent: None,
            contents: CHashMap::new(),
            named_exports: CHashMap::new(),
            wildcard_exports: CHashMap::new(),
        };

        if self.namespaces.insert(id, namespace).is_some() {
            panic!("allocate_id allocated an id twice");
        }
        id
    }

    /// Inserts a member into the namespace.
    /// If you need to insert a namespace, there are methods to doing that.
    /// That's because inserting a namespace has more things that may
    /// happen, and we want those things to be convenient.
    pub fn insert_member(&self, namespace_id: NamespaceID, 
                                member_name: TinyString,
                                member: T, 
                                publicity: Publicity) -> bool {
        let namespace = match self.namespaces.get(&namespace_id) {
            Some(n) => n,
            None => unreachable!(
                "Invalid NamespaceID, maybe you mixed\
                    ids between namespace managers?"),
        };

        let old_member = namespace.contents.insert(member_name,
                                (publicity, NamespaceContent::Other(member)));
                                
        // If there was an old member with this name,
        // we have a name collision!
        old_member.is_some()
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum ExportMode {
    All,
    Public,
}

impl ExportMode {
    pub fn can_export(&self, publicity: Publicity) -> bool {
        use ExportMode::*;
        match self {
            All => true,
            Public => publicity == Publicity::Public,
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Publicity {
    Public,
    Private,
}

impl Publicity {
    // If we have a chained import, what
    pub fn pipe(self, other: Publicity) -> Publicity {
        if self == Publicity::Public && other == Publicity::Public {
            Publicity::Public
        }else {
            Publicity::Private
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn insertion() {
        let manager = NamespaceManager::new();

        let root = manager.create_root();
        manager.insert_member(root, 
                              "member".into(), 
                              "Hello world!",
                              Publicity::Public);
    }
}
