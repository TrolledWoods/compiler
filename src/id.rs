use std::sync::atomic::{ AtomicU32, Ordering };
use std::hash::Hash;
use std::num::NonZeroU32;
use chashmap::{CHashMap, ReadGuard, WriteGuard};

pub trait Id: Hash + Copy + PartialEq {
    fn into_raw(self) -> NonZeroU32;
    fn from_raw(other: NonZeroU32) -> Self;
}

macro_rules! create_id {
    ($name:ident) => {
        #[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
        pub struct $name(std::num::NonZeroU32);

        impl $name {
            pub fn new(value: u32) -> $name {
                use std::num::NonZeroU32;

                let id = NonZeroU32::new(value).expect("Cannot create an id with value 0");
                $name(id)
            }
        }

        impl crate::id::Id for $name {
            fn into_raw(self) -> std::num::NonZeroU32 { self.0 }
            fn from_raw(other: std::num::NonZeroU32) -> Self { $name(other) }
        }
    }
}

// TODO: Make a non concurrent element map?

pub struct CIdMap<I: Id, D> {
    ctr: AtomicU32,
    data: CHashMap<I, D>, 
}

impl<I: Id, D> CIdMap<I, D> {
    pub fn new() -> CIdMap<I, D> {
        CIdMap {
            ctr: AtomicU32::new(1),
            data: CHashMap::new(),
        }
    }

    fn allocate_id(&self) -> I {
        let id = self.ctr.fetch_add(1, Ordering::SeqCst);
        I::from_raw(NonZeroU32::new(id).expect("Invariant wasn't upheld"))
    }

    pub fn insert(&self, element: D) -> I {
        let id = self.allocate_id();
        let old_element = self.data.insert(id, element);
        assert!(matches!(old_element, None));
        id
    }

    pub fn get<'a>(&'a self, id: I) -> Option<ReadGuard<'a, I, D>> {
        self.data.get(&id)
    }

    pub fn get_mut<'a>(&'a mut self, id: I) -> Option<WriteGuard<'a, I, D>> {
        self.data.get_mut(&id)
    }
}
