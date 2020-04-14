use crate::lexer::SourcePos;
use crate::compilation_manager::{Identifier, Id};
use crate::string_pile::TinyString;

#[macro_use]
use crate::id::CIdMap;
use chashmap::CHashMap;

#[derive(Clone, Debug, PartialEq)]
pub enum NamespaceError {
    DuplicateName {
        inserting: (Identifier, Id), 
        old_member: NamespaceElement,
    },
    InvalidAmbiguity {
        inserting: (Identifier, Id),
        old_member: NamespaceElement,
    }
}

pub enum NamespaceAccessError {
    DoesNotExist,
    Ambiguous(Vec<(Identifier, Id)>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum NamespaceElement {
    Singular(Identifier, Id, AllowAmbiguity),
    Ambiguous(Vec<(Identifier, Id)>),
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum AllowAmbiguity {
    Allow,
    Deny,
}

pub struct NamespaceManager {
    // namespaces: CIdMap<NamespaceId, Namespace>,
    name_map: CHashMap<TinyString, NamespaceElement>,
}

impl NamespaceManager {
    pub fn new() -> NamespaceManager {
        NamespaceManager {
            // namespaces: CIdMap::new(),
            name_map: CHashMap::new(),
        }
    }

    pub fn insert_member(
        &self, 
        _parent: NamespaceId, 
        name: Identifier, 
        member_id: Id,
        is_ambiguity_allowed: AllowAmbiguity, 
    ) -> Result<(), NamespaceError> {
        match self.name_map.get_mut(&name.data) {
            // Something already exists there,
            // this is ambiguous
            Some(mut old_member) => {
                // Figure out if the old member_id allows ambiguity
                // PERFORMANCE: Only do this if the is_ambiguity_allowed
                // value is set to Allow, i.e. turn this into a lazy or.
                // That will make the code look a lot worse though
                let old_ambiguity = match &old_member as &NamespaceElement {
                    NamespaceElement::Singular(_, _, ambiguity) => *ambiguity,
                    NamespaceElement::Ambiguous(_) => AllowAmbiguity::Allow,
                };

                if is_ambiguity_allowed == AllowAmbiguity::Deny || old_ambiguity == AllowAmbiguity::Deny { 
                    return Err(NamespaceError::DuplicateName {
                        inserting: (name, member_id),
                        old_member: old_member.clone(),
                    });
                }

                // Modify the new member_id to match
                // the ambiguity
                match &mut old_member as &mut NamespaceElement {
                    NamespaceElement::Singular(old_pos, old_id, _) => {
                        *old_member = NamespaceElement::Ambiguous(vec![(old_pos.clone(), *old_id), (name, member_id)]);
                    },
                    NamespaceElement::Ambiguous(members) => {
                        members.push((name, member_id));
                    },
                }

                Ok(())
            },
            // Nothing existed there before, 
            // so a new member is inserted
            None => {
                let old_member = self.name_map.insert(name.data, NamespaceElement::Singular(name, member_id, is_ambiguity_allowed));
                assert!(matches!(old_member, None));

                Ok(())
            },
        }
    }

    pub fn get_member(&self, _parent: NamespaceId, name: TinyString) -> Result<(SourcePos, Id), NamespaceAccessError> {
        match self.name_map.get(&name) {
            Some(element) => {
                match &element as &NamespaceElement {
                    NamespaceElement::Singular(name, value, _) => Ok((name.pos.clone(), value.clone())),
                    NamespaceElement::Ambiguous(values) => Err(NamespaceAccessError::Ambiguous(values.clone()))
                }
            },
            None => Err(NamespaceAccessError::DoesNotExist), 
        }
    }

    pub fn insert_root(&self) -> NamespaceId {
        // Right now no namespaces actually exist
        NamespaceId::new(1)
    }

    pub fn insert_namespace(&self, _parent: NamespaceId, _name: Option<TinyString>, _pos: SourcePos) -> Result<NamespaceId, NamespaceError> {
        Ok(NamespaceId::new(1))
    }
} 

create_id!(NamespaceId);
