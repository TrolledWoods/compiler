use crate::ast;
use crate::error::{CompileError, ErrorPrintingData};
use crate::id::CIdMap;
use crate::lexer::SourcePos;
use crate::namespace::{
	AllowAmbiguity, NamespaceAccessError, NamespaceError, NamespaceId, NamespaceManager,
};
use crate::string_pile::TinyString;
use crate::types::{
	self, FunctionHeader, NamedTypeId, ResolvedTypeDef, ResolvedTypeId, ResolvedTypeKind, TypeDef,
	TypeDefKind,
};
use chashmap::{ReadGuard, WriteGuard};
use std::collections::{BTreeSet, HashMap};
use std::sync::Mutex;

pub type Dependencies = Vec<(TinyString, Vec<SourcePos>)>;

#[derive(Debug)]
pub enum CompileManagerError {
	UndefinedDependency {
		dependant: SourcePos,
		dependency: Identifier,
	},
	AmbiguousDependency {
		dependant: SourcePos,
		dependency: Identifier,
		defined: Vec<Identifier>,
	},
	ExpectedType {
		pos: SourcePos,
		reference_pos: SourcePos,
	},
	DependingOnSelf {
		dependant: Identifier,
		dependency: (TinyString, Vec<SourcePos>),
	},
	Poisoned,
}

impl CompileManagerError {
	pub fn dependency_not_in_namespace(
		error: NamespaceAccessError,
		dependant: SourcePos,
		dependency: Identifier,
	) -> CompileManagerError {
		match error {
			NamespaceAccessError::DoesNotExist => CompileManagerError::UndefinedDependency {
				dependant,
				dependency,
			},
			NamespaceAccessError::Ambiguous(defined) => CompileManagerError::AmbiguousDependency {
				dependant,
				dependency,
				defined: defined.into_iter().map(|(v, _)| v).collect(),
			},
		}
	}
}

impl CompileError for CompileManagerError {
	fn get_printing_data(self) -> ErrorPrintingData {
		use CompileManagerError::*;
		fn add_dependencies(printer: &mut ErrorPrintingData, dependencies: Vec<SourcePos>) {
			for dependency in dependencies {
				printer.push_problem(dependency, format!("Dependency here"));
			}
		}

		match self {
			UndefinedDependency {
				dependant,
				dependency: Identifier {
					data: depending_on,
					pos: dep_pos,
				},
			} => {
				let mut error = ErrorPrintingData::new(format!(
					"Item cannot be compiled because it depends on '{}', which doesn't exist",
					depending_on
				))
				.problem(dep_pos, format!("This value doesn't exist"));
				error
			}
			AmbiguousDependency {
				dependant,
				dependency: Identifier {
					data: depending_on,
					pos: dep_pos,
				},
				defined,
			} => {
				let mut error = ErrorPrintingData::new(format!(
					"Item cannot be compiled because it depends on '{}', which is ambiguous",
					depending_on
				))
				.problem(dep_pos, format!("This value doesn't exist"));
				error
			}
			ExpectedType { pos, reference_pos } => ErrorPrintingData::new(format!(
				"Expected a type, but the value in the namespace wasn't a type"
			))
			.problem(pos, format!("Expected type"))
			.problem(reference_pos, format!("The value isn't defined as a type")),
			DependingOnSelf {
				dependant,
				dependency: (depending_on, deps),
			} => {
				let mut error = ErrorPrintingData::new(
                    format!(
                        "'{}' depends on itself, which isn't allowed. It's only allowed through a pointer(not implemented yet)", 
                        dependant.data
                    )
                ).problem(
                    dependant.pos,
                    format!("This value depends on itself")
                );
				add_dependencies(&mut error, deps);
				error
			}
			Poisoned => ErrorPrintingData::new(format!(
				"Error caused by poison. This is just debug text, not shipping error"
			)),
		}
	}
}

pub struct CompileManager {
	pub namespace_manager: NamespaceManager,

	// TODO: Change the type of this
	// to something better. Maybe a concurrent queue of some sort?
	ready_to_compile: Mutex<Vec<CompilationUnitId>>,

	pub functions: CIdMap<FunctionId, FunctionCompUnit>,

	pub named_types: CIdMap<NamedTypeId, NamedTypeCompUnit>,
	pub resolved_types: CIdMap<ResolvedTypeId, ResolvedTypeDef>,

	/// This is basically a reverse mapping from a type to a type id.
	/// This is because a ResolvedTypeId for a 2 types is the same,
	/// and only the same, if the types have the same signature.
	pub unnamed_types: Mutex<HashMap<ResolvedTypeDef, ResolvedTypeId>>,
}

impl CompileManager {
	pub fn new() -> CompileManager {
		CompileManager {
			namespace_manager: NamespaceManager::new(),
			ready_to_compile: Mutex::new(vec![]),

			functions: CIdMap::new(),

			named_types: CIdMap::new(),
			resolved_types: CIdMap::new(),

			unnamed_types: Mutex::new(HashMap::new()),
		}
	}

	pub fn get_named_type_mut_or<'a, E>(
		&'a self,
		id: CompilationUnitId,
		err: impl FnOnce(CompUnitWindow<'_>) -> E,
	) -> Result<(NamedTypeId, WriteGuard<'a, NamedTypeId, NamedTypeCompUnit>), E> {
		match id {
			CompilationUnitId::NamedType(named_id) => Ok((
				named_id,
				self.named_types
					.get_mut(named_id)
					.expect("Invalid NamedTypeId"),
			)),
			CompilationUnitId::Function(id) => {
				let lock = self.functions.get(id).expect("Invalid FunctionId");
				let window = lock.get_window();
				let err = err(window);
				Err(err)
			}
		}
	}

	pub fn get_named_type_or<'a, E>(
		&'a self,
		id: CompilationUnitId,
		err: impl FnOnce(CompUnitWindow<'_>) -> E,
	) -> Result<(NamedTypeId, ReadGuard<'a, NamedTypeId, NamedTypeCompUnit>), E> {
		match id {
			CompilationUnitId::NamedType(named_id) => Ok((
				named_id,
				self.named_types.get(named_id).expect("Invalid NamedTypeId"),
			)),
			CompilationUnitId::Function(id) => {
				let lock = self.functions.get(id).expect("Invalid FunctionId");
				let window = lock.get_window();
				let err = err(window);
				Err(err)
			}
		}
	}

	pub fn insert_function(
		&self,
		namespace_id: NamespaceId,
		pos: SourcePos,
		names: Vec<Identifier>,
		header: FunctionHeader<TypeDef>,
		body: ast::ExpressionDef,
	) -> FunctionId {
		body.pretty_print(0);
		let id = self.functions.insert(FunctionCompUnit {
			pos,
			namespace_id,
			dependencies: BTreeSet::new(),
			header_names: names,
			header,
			body,
			typed: CompUnitStage::unresolved(),
			stage: FunctionStage::Defined,
		});
		self.add_ready_compilation_unit(CompilationUnitId::Function(id));

		println!("Added function: {:?}", id);

		id
	}

	pub fn insert_named_type(
		&self,
		namespace_id: NamespaceId,
		identifier: Identifier,
		definition: TypeDef,
	) -> Result<NamedTypeId, NamespaceError> {
		let type_id = self.named_types.insert(NamedTypeCompUnit {
			location: namespace_id,
			definition,
			dependencies: BTreeSet::new(),
			fully_sized: CompUnitStage::unresolved(),
			stage: NamedTypeStage::Defined,
		});
		let id = CompilationUnitId::NamedType(type_id);

		self.namespace_manager
			.insert_member(namespace_id, identifier, id, AllowAmbiguity::Deny)?;
		self.add_ready_compilation_unit(id);
		Ok(type_id)
	}

	pub fn get_ready_compilation_unit(&self) -> Option<CompilationUnitId> {
		let mut lock = self.ready_to_compile.lock().unwrap();
		lock.pop()
	}

	pub fn add_ready_compilation_unit(&self, id: CompilationUnitId) {
		let mut lock = self.ready_to_compile.lock().unwrap();
		lock.push(id);
	}

	pub fn advance_compilation_unit(
		&self,
		id: CompilationUnitId,
	) -> Result<(), CompileManagerError> {
		match id {
			CompilationUnitId::NamedType(type_id) => self.advance_type(type_id)?,
			CompilationUnitId::Function(id) => self.advance_function(id)?,
		}

		Ok(())
	}

	fn advance_function(&self, id: FunctionId) -> Result<(), CompileManagerError> {
		let mut comp_unit = self.functions.get_mut(id).expect("Invalid FunctionId");
		let FunctionCompUnit {
			pos,
			namespace_id,
			dependencies,
			header_names,
			header,
			body,
			typed,
			stage,
		} = &mut comp_unit as &mut FunctionCompUnit;

		match stage {
			FunctionStage::Defined => {
				*stage = FunctionStage::Poisoned;

				header.get_dependencies(&mut |dep| {
					println!("{:?}", dep);

					let (_dependency_pos, dependency_id) = self
						.namespace_manager
						.get_member(*namespace_id, dep.data)
						.map_err(|err| {
							CompileManagerError::dependency_not_in_namespace(
								err,
								pos.clone(),
								dep.clone(),
							)
						})?;

					let (_, mut named_type) =
						self.get_named_type_mut_or(dependency_id, |general_comp_unit| {
							CompileManagerError::ExpectedType {
								pos: pos.clone(),
								reference_pos: general_comp_unit.pos,
							}
						})?;

					// Depend on the fully sized field of the
					// named_type. TODO: Make this the sized field
					if named_type
						.fully_sized
						.insert_dependency(CompilationUnitId::Function(id))
					{
						dependencies.insert(dependency_id);
					}

					Ok(())
				})?;

				*stage = FunctionStage::WaitingForTyping;

				println!("Sent off dependencies for function header types!");

				if dependencies.len() == 0 {
					// Make sure to not get a lock-block
					std::mem::drop(comp_unit);
					self.advance_function(id);
				}
			}
			FunctionStage::WaitingForTyping => {
				*stage = FunctionStage::Typed;

				// TODO: Implement body dependencies

				// Make sure to not get a lock-block
				std::mem::drop(comp_unit);
				self.advance_function(id);
			}
			FunctionStage::Typed => {
				*stage = FunctionStage::Poisoned;
			}
			_ => unimplemented!(),
		}

		Ok(())
	}

	/// Compiles a NamedType a bit further.
	/// This requires all the old dependencies to be fullfilled
	fn advance_type(&self, type_id: NamedTypeId) -> Result<(), CompileManagerError> {
		let mut comp_unit = self
			.named_types
			.get_mut(type_id)
			.expect("Invalid NamedTypeId");
		let NamedTypeCompUnit {
			location: namespace_id,
			stage,
			dependencies,
			definition,
			fully_sized,
		} = &mut comp_unit as &mut NamedTypeCompUnit;

		match stage {
			NamedTypeStage::Defined => {
				*stage = NamedTypeStage::Poisoned;

				definition.get_dependencies(&mut |dep| {
					let (_dependency_pos, dependency_id) = self
						.namespace_manager
						.get_member(*namespace_id, dep.data)
						.map_err(|err| {
							CompileManagerError::dependency_not_in_namespace(
								err,
								definition.pos.clone(),
								dep.clone(),
							)
						})?;

					if dependency_id == CompilationUnitId::NamedType(type_id) {
						// References self? This is garbage! Self has
						// to be references through a poitner
						unimplemented!("TODO: Error message for self referencing");
					}

					let (_, mut named_type) =
						self.get_named_type_mut_or(dependency_id, |general_comp_unit| {
							CompileManagerError::ExpectedType {
								pos: definition.pos.clone(),
								reference_pos: general_comp_unit.pos,
							}
						})?;

					// Depend on the fully sized field of the
					// named_type. TODO: Make this the sized field
					if named_type
						.fully_sized
						.insert_dependency(CompilationUnitId::NamedType(type_id))
					{
						dependencies.insert(dependency_id);
					}

					Ok(())
				})?;

				*stage = NamedTypeStage::LocateNames;

				// If there were no dependencies, just advance the
				// type directly
				if dependencies.len() == 0 {
					std::mem::drop(comp_unit);
					self.advance_type(type_id);
				}
			}
			NamedTypeStage::LocateNames => {
				*stage = NamedTypeStage::Poisoned;

				// Size the type!
				// This is only fine because we know that
				// all our dependencies on other named types
				// are resolved.
				let (size, align, resolved_id) = types::size_type_def(self, definition, type_id)?;

				let dependants =
					std::mem::replace(fully_sized, CompUnitStage::Done((size, align, resolved_id)));

				if let CompUnitStage::Unresolved { dependants } = dependants {
					for dependant in dependants {
						self.remove_dependency(dependant, CompilationUnitId::NamedType(type_id));
					}
				} else {
					unreachable!("The 'fully_sized' member has to be matched with the stage");
				}

				println!("Resolved type {}, {}", size, align);

				*stage = NamedTypeStage::FullySized;
			}
			NamedTypeStage::FullySized => {
				*stage = NamedTypeStage::Poisoned;
				unreachable!();
			}
			NamedTypeStage::Poisoned => {}
		}

		Ok(())
	}

	pub fn remove_dependency(&self, remove_from: CompilationUnitId, dependency: CompilationUnitId) {
		match remove_from {
			CompilationUnitId::NamedType(id) => {
				let mut comp_unit = self.named_types.get_mut(id).unwrap();
				assert!(
					comp_unit.dependencies.remove(&dependency),
					"Cannot remove a dependency that doesn't exist"
				);

				if comp_unit.dependencies.len() == 0 {
					self.add_ready_compilation_unit(remove_from);
				}
			}
			CompilationUnitId::Function(id) => {
				let mut comp_unit = self.functions.get_mut(id).unwrap();
				assert!(
					comp_unit.dependencies.remove(&dependency),
					"Cannot remove a dependency that doesn't exist"
				);

				if comp_unit.dependencies.len() == 0 {
					self.add_ready_compilation_unit(remove_from);
				}
			}
		}
	}
}

#[derive(Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub enum CompilationUnitId {
	NamedType(NamedTypeId),
	Function(FunctionId),
}

create_id!(FunctionId);

#[derive(Clone, Copy, Hash, PartialEq, Eq, Debug)]
pub enum FunctionStage {
	Poisoned,
	Defined,
	WaitingForTyping,
	Typed,
	DependantsDone,
}

pub struct FunctionCompUnit {
	pub pos: SourcePos,
	pub dependencies: BTreeSet<CompilationUnitId>,
	pub namespace_id: NamespaceId,

	pub header_names: Vec<Identifier>,
	pub header: FunctionHeader<TypeDef>,
	pub body: ast::ExpressionDef,

	pub typed: CompUnitStage<FunctionHeader<ResolvedTypeId>>,
	// pub bytecode: CompUnitStage<Vec<InterInstruction>>,
	pub stage: FunctionStage,
}

impl CompUnit for FunctionCompUnit {
	type Stage = FunctionStage;

	fn get_window_mut<'a>(&'a mut self) -> CompUnitWindowMut<'a> {
		CompUnitWindowMut {
			pos: self.pos.clone(),
			dependencies: &mut self.dependencies,
		}
	}

	fn get_window<'a>(&'a self) -> CompUnitWindow<'a> {
		CompUnitWindow {
			pos: self.pos.clone(),
			dependencies: &self.dependencies,
		}
	}

	fn get_stage_dependants_mut<'a>(
		&'a mut self,
		stage: Self::Stage,
	) -> Option<&'a mut BTreeSet<CompilationUnitId>> {
		match stage {
			FunctionStage::Poisoned => None,
			FunctionStage::Defined => None,
			FunctionStage::WaitingForTyping => self.typed.get_dependants_mut(),
			FunctionStage::Typed => None, // TODO: Fix this one
			FunctionStage::DependantsDone => None,
		}
	}
}

#[derive(Clone, Copy, Hash, PartialEq, Eq, Debug)]
pub enum NamedTypeStage {
	Poisoned,
	Defined,
	LocateNames,
	FullySized,
}

pub struct NamedTypeCompUnit {
	pub location: NamespaceId,
	pub definition: TypeDef,
	pub dependencies: BTreeSet<CompilationUnitId>,

	// TODO: Add another sizing step here,
	// to allow for circular pointer referencing
	pub fully_sized: CompUnitStage<(usize, usize, ResolvedTypeId)>,

	pub stage: NamedTypeStage,
}

impl CompUnit for NamedTypeCompUnit {
	type Stage = NamedTypeStage;

	fn get_window_mut<'a>(&'a mut self) -> CompUnitWindowMut<'a> {
		CompUnitWindowMut {
			pos: self.definition.pos.clone(),
			dependencies: &mut self.dependencies,
		}
	}

	fn get_window<'a>(&'a self) -> CompUnitWindow<'a> {
		CompUnitWindow {
			pos: self.definition.pos.clone(),
			dependencies: &self.dependencies,
		}
	}

	fn get_stage_dependants_mut<'a>(
		&'a mut self,
		stage: Self::Stage,
	) -> Option<&'a mut BTreeSet<CompilationUnitId>> {
		match stage {
			NamedTypeStage::Poisoned => None,
			NamedTypeStage::Defined => None,
			NamedTypeStage::LocateNames => None,
			NamedTypeStage::FullySized => self.fully_sized.get_dependants_mut(),
		}
	}
}

pub enum CompUnitStage<T> {
	Unresolved {
		dependants: BTreeSet<CompilationUnitId>,
	},
	Done(T),
}

impl<T> CompUnitStage<T> {
	pub fn unresolved() -> CompUnitStage<T> {
		CompUnitStage::Unresolved {
			dependants: BTreeSet::new(),
		}
	}

	pub fn insert_dependency(&mut self, dep: CompilationUnitId) -> bool {
		if let Some(dependants) = self.get_dependants_mut() {
			dependants.insert(dep);
			true
		} else {
			false
		}
	}

	pub fn get_dependants_mut<'a>(&'a mut self) -> Option<&'a mut BTreeSet<CompilationUnitId>> {
		match self {
			CompUnitStage::Unresolved { dependants } => Some(dependants),
			CompUnitStage::Done(_) => None,
		}
	}

	pub fn done(&self) -> Option<&T> {
		match self {
			CompUnitStage::Unresolved { .. } => None,
			CompUnitStage::Done(val) => Some(val),
		}
	}

	pub fn done_mut(&mut self) -> Option<&mut T> {
		match self {
			CompUnitStage::Unresolved { .. } => None,
			CompUnitStage::Done(val) => Some(val),
		}
	}
}

pub trait CompUnit {
	type Stage: Copy;

	fn get_window_mut<'a>(&'a mut self) -> CompUnitWindowMut<'a>;
	fn get_window<'a>(&'a self) -> CompUnitWindow<'a>;
	fn get_stage_dependants_mut<'a>(
		&'a mut self,
		stage: Self::Stage,
	) -> Option<&'a mut BTreeSet<CompilationUnitId>>;
}

pub struct CompUnitWindow<'a> {
	pub pos: SourcePos,
	pub dependencies: &'a BTreeSet<CompilationUnitId>,
}

pub struct CompUnitWindowMut<'a> {
	pub pos: SourcePos,
	pub dependencies: &'a mut BTreeSet<CompilationUnitId>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Identifier {
	pub pos: SourcePos,
	pub data: TinyString,
}

pub enum Dependency {
	CompUnit(SourcePos, CompilationUnitId),
	Name(Identifier),
}
