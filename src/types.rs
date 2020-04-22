use crate::compilation_manager::{
	CompilationUnitId, CompileManager, CompileManagerError, Identifier,
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
	pub fn get_unsized_dependencies<E>(
		&self,
		mut on_find_dependency: &mut impl FnMut(Identifier) -> Result<(), E>,
	) -> Result<(), E> {
		use TypeDefKind::*;
		match &self.kind {
			Offload(name) => on_find_dependency(Identifier {
				data: *name,
				pos: self.pos.clone(),
			})?,
			Tuple(members) => {
				for member in members {
					member.get_unsized_dependencies(on_find_dependency)?;
				}
			}
			Pointer(data) => data.get_unsized_dependencies(on_find_dependency)?,
			FunctionPtr(header) => {
				for member in &header.args {
					member.get_unsized_dependencies(on_find_dependency)?;
				}

				for member in &header.returns {
					member.get_unsized_dependencies(on_find_dependency)?;
				}
			}
			Struct(members) => {
				for (_, member) in members {
					member.get_unsized_dependencies(on_find_dependency)?;
				}
			}
			Primitive(kind) => (),
			StaticArray(_, content) => content.get_unsized_dependencies(on_find_dependency)?,
			ArrayWindow(content) => content.get_unsized_dependencies(on_find_dependency)?,
			DynamicArray(content) => content.get_unsized_dependencies(on_find_dependency)?,
		}

		Ok(())
	}

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
			Pointer(data) => (),
			FunctionPtr(header) => (),
			Struct(members) => {
				for (_, member) in members {
					member.get_dependencies(on_find_dependency)?;
				}
			}
			Primitive(kind) => (),
			StaticArray(_, content) => content.get_dependencies(on_find_dependency)?,
			ArrayWindow(_) => (),
			DynamicArray(_) => (),
		}

		Ok(())
	}
}

impl Display for TypeDef {
	fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
		write!(f, "{}", self.kind)
	}
}

pub enum CollectionDefKind {
	Named(Vec<(TinyString, TypeDef)>),
	Unnamed(Vec<TypeDef>),
}

pub enum TypeDefKind {
	Offload(TinyString),
	Pointer(Box<TypeDef>),
	Tuple(Vec<TypeDef>),
	FunctionPtr(FunctionHeader<TypeDef>),
	Struct(Vec<(TinyString, TypeDef)>),
	Primitive(PrimitiveKind),
	StaticArray(usize, Box<TypeDef>),
	ArrayWindow(Box<TypeDef>),
	DynamicArray(Box<TypeDef>),
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
			Pointer(data) => {
				write!(f, "*{}", data)?;
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
			StaticArray(count, content) => write!(f, "[{}] {}", count, content)?,
			ArrayWindow(content) => write!(f, "[-] {}", content)?,
			DynamicArray(content) => write!(f, "[?] {}", content)?,
		}

		Ok(())
	}
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum ResolvedTypeKind {
	Offload(NamedTypeId),
	Tuple(Vec<(usize, ResolvedTypeId)>),
	Pointer,
	FunctionPtr,
	Struct(Vec<(usize, TinyString, ResolvedTypeId)>),
	Primitive(PrimitiveKind),
	StaticArray(usize, ResolvedTypeId),
	ArrayWindow,
	DynamicArray,
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

/// Figures out the size and align of a type.
/// Expects that any named type in the definition
/// is sized.
/// It may add more ResolvedTypes(should probably rename that to SizedType or smth)
pub fn size_type_def(
	manager: &CompileManager,
	namespace_id: NamespaceId,
	type_def: &TypeDef,
	recursion_guard: NamedTypeId,
) -> Result<(usize, usize, ResolvedTypeId), CompileManagerError> {
	match &type_def.kind {
		TypeDefKind::Offload(reference) => {
			let (member_pos, id) = manager
				.namespace_manager
				.get_member(namespace_id, *reference)
				.unwrap();

			let (named_type_id, named_type) = match id {
				CompilationUnitId::NamedType(named_type_id) => {
					if named_type_id == recursion_guard {
						unimplemented!("Recursion while sizing a type!");
					}

					(
						named_type_id,
						manager.named_types.get(named_type_id).unwrap(),
					)
				}
			};

			let (size, align, _) = named_type.fully_sized.done().expect(
				"Cannot size a type definition if the types it depends on aren't sized either",
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
		TypeDefKind::Pointer(_)
		| TypeDefKind::FunctionPtr(_)
		| TypeDefKind::ArrayWindow(_)
		| TypeDefKind::DynamicArray(_) => {
			let id = insert_resolved_type(
				manager,
				ResolvedTypeDef {
					size: 8,
					align: 8,
					kind: match &type_def.kind {
						TypeDefKind::Pointer(_) => ResolvedTypeKind::Pointer,
						TypeDefKind::FunctionPtr(_) => ResolvedTypeKind::FunctionPtr,
						TypeDefKind::ArrayWindow(_) => ResolvedTypeKind::ArrayWindow,
						TypeDefKind::DynamicArray(_) => ResolvedTypeKind::DynamicArray,
						_ => unreachable!(),
					},
				},
			);

			Ok((8, 8, id))
		}
		TypeDefKind::StaticArray(count, contents) => {
			let (content_size, content_align, content_id) =
				size_type_def(manager, namespace_id, contents, recursion_guard)?;

			let (size, align) = {
				if content_size == 0 {
					(0, 0)
				} else {
					// It shouldn't be zero when the size isn't zero, right?
					assert_ne!(content_align, 0);
					let mut size = content_size;

					while size % content_align != 0 {
						size += 1;
					}

					(size * count, content_align)
				}
			};

			let id = insert_resolved_type(
				manager,
				ResolvedTypeDef {
					size,
					align,
					kind: ResolvedTypeKind::StaticArray(*count, content_id),
				},
			);

			Ok((size, align, id))
		}
		TypeDefKind::Tuple(members) => {
			let mut resolved_members = Vec::with_capacity(members.len());
			let mut size = 0;
			let mut align = 0;
			for member in members {
				let (member_size, member_align, new_member) =
					size_type_def(manager, namespace_id, member, recursion_guard)?;
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
					size_type_def(manager, namespace_id, member, recursion_guard)?;
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
	}
}
