use crate::compilation_manager::{CompilationUnitId, Identifier};
use crate::error::{CompileError, ErrorPrintingData};
use crate::lexer::SourcePos;
use crate::string_pile::TinyString;
use chashmap::CHashMap;

#[derive(Clone, Debug, PartialEq)]
pub enum NamespaceError {
	DuplicateName {
		inserting: (Identifier, CompilationUnitId),
		old_member: NamespaceElement,
	},
}

impl CompileError for NamespaceError {
	fn get_printing_data(self) -> ErrorPrintingData {
		match self {
			NamespaceError::DuplicateName {
				inserting: (inserting_name, _inserting_id),
				old_member,
			} => {
				let mut printing_data = ErrorPrintingData::new(format!(
					"{} already exists in namespace",
					inserting_name.data
				))
				.problem(
					inserting_name.pos.clone(),
					format!("Tried adding this to the namespace"),
				);

				match old_member {
					NamespaceElement::Singular(name, _id, _) => {
						printing_data.problem(name.pos, format!("Already defined here"))
					}
					NamespaceElement::Ambiguous(elements) => {
						for (name, _id) in elements {
							printing_data =
								printing_data.problem(name.pos, format!("Already defined here"))
						}

						printing_data
					}
				}
			}
		}
	}
}

#[derive(Debug)]
pub enum NamespaceAccessError {
	DoesNotExist,
	Ambiguous(Vec<(Identifier, CompilationUnitId)>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum NamespaceElement {
	Singular(Identifier, CompilationUnitId, AllowAmbiguity),
	Ambiguous(Vec<(Identifier, CompilationUnitId)>),
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
		member_id: CompilationUnitId,
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

				if is_ambiguity_allowed == AllowAmbiguity::Deny
					|| old_ambiguity == AllowAmbiguity::Deny
				{
					return Err(NamespaceError::DuplicateName {
						inserting: (name, member_id),
						old_member: old_member.clone(),
					});
				}

				// Modify the new member_id to match
				// the ambiguity
				match &mut old_member as &mut NamespaceElement {
					NamespaceElement::Singular(old_pos, old_id, _) => {
						*old_member = NamespaceElement::Ambiguous(vec![
							(old_pos.clone(), *old_id),
							(name, member_id),
						]);
					}
					NamespaceElement::Ambiguous(members) => {
						members.push((name, member_id));
					}
				}

				Ok(())
			}
			// Nothing existed there before,
			// so a new member is inserted
			None => {
				let old_member = self.name_map.insert(
					name.data,
					NamespaceElement::Singular(name, member_id, is_ambiguity_allowed),
				);
				assert!(matches!(old_member, None));

				Ok(())
			}
		}
	}

	pub fn get_member(
		&self,
		_parent: NamespaceId,
		name: TinyString,
	) -> Result<(SourcePos, CompilationUnitId), NamespaceAccessError> {
		match self.name_map.get(&name) {
			Some(element) => match &element as &NamespaceElement {
				NamespaceElement::Singular(name, value, _) => Ok((name.pos.clone(), value.clone())),
				NamespaceElement::Ambiguous(values) => {
					Err(NamespaceAccessError::Ambiguous(values.clone()))
				}
			},
			None => Err(NamespaceAccessError::DoesNotExist),
		}
	}

	pub fn insert_root(&self) -> NamespaceId {
		// Right now no namespaces actually exist
		NamespaceId::new(1)
	}

	pub fn create_anonymous_namespace(&self, parent: NamespaceId) -> NamespaceId {
		parent
	}

	pub fn insert_namespace(
		&self,
		_parent: NamespaceId,
		_name: Option<TinyString>,
		_pos: SourcePos,
	) -> Result<NamespaceId, NamespaceError> {
		Ok(NamespaceId::new(1))
	}
}

create_id!(NamespaceId);
