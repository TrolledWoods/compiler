use crate::error::{CompileError, ErrorPrintingData};
use crate::id::CIdMap;
use crate::lexer::SourcePos;
use crate::namespace::{
    AllowAmbiguity, NamespaceAccessError, NamespaceError, NamespaceId, NamespaceManager,
};
use crate::string_pile::TinyString;
use crate::types::{
    self, NamedTypeId, ResolvedTypeDef, ResolvedTypeId, ResolvedTypeKind, TypeDef, TypeDefKind,
};
use std::collections::HashMap;
use std::sync::Mutex;

pub type Dependencies = Vec<(TinyString, Vec<SourcePos>)>;

#[derive(Debug)]
pub enum CompileManagerError {
    UndefinedDependency {
        dependant: SourcePos,
        dependency: (TinyString, Vec<SourcePos>),
    },
    AmbiguousDependency {
        dependant: SourcePos,
        dependency: (TinyString, Vec<SourcePos>),
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
                dependency: (depending_on, deps),
            } => {
                let mut error = ErrorPrintingData::new(format!(
                    "Item cannot be compiled because it depends on '{}', which doesn't exist",
                    depending_on
                ))
                .problem(dependant, format!("invalid dependency in this value"));
                add_dependencies(&mut error, deps);
                error
            }
            AmbiguousDependency {
                dependant,
                dependency: (depending_on, deps),
                defined,
            } => {
                let mut error = ErrorPrintingData::new(format!(
                    "Item cannot be compiled because it depends on '{}', which is ambiguous",
                    depending_on
                ))
                .problem(dependant, format!("invalid dependency in this value"));
                add_dependencies(&mut error, deps);
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

    pub named_types: CIdMap<NamedTypeId, CompilationUnit<TypeDef, (TypeDef, ResolvedTypeId)>>,
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

            named_types: CIdMap::new(),
            resolved_types: CIdMap::new(),

            unnamed_types: Mutex::new(HashMap::new()),
        }
    }

    pub fn insert_named_type(
        &self,
        namespace_id: NamespaceId,
        identifier: Identifier,
        definition: TypeDef,
        dependencies: Dependencies,
    ) -> Result<NamedTypeId, NamespaceError> {
        let type_id = self.named_types.insert(CompilationUnit::Defined {
            namespace_id,
            definition,
            dependencies,
            dependants: Vec::new(),
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
        }

        Ok(())
    }

    fn advance_type(&self, type_id: NamedTypeId) -> Result<(), CompileManagerError> {
        let mut comp_unit = self
            .named_types
            .get_mut(type_id)
            .expect("Invalid NamedTypeId");

        let owned_comp_unit = std::mem::replace(
            &mut comp_unit as &mut CompilationUnit<TypeDef, _>,
            CompilationUnit::Poisoned,
        );
        match owned_comp_unit {
            CompilationUnit::Defined {
                namespace_id,
                definition,
                dependencies,
                dependants,
            } => {
                // Hook up all dependencies
                let mut n_dependencies = 0;
                for (dependency, locations) in dependencies {
                    match self.namespace_manager.get_member(namespace_id, dependency) {
                        Ok((pos, id)) => {
                            if id == CompilationUnitId::NamedType(type_id) {
                                return Err(CompileManagerError::DependingOnSelf {
                                    dependant: Identifier {
                                        data: dependency,
                                        pos,
                                    },
                                    dependency: (dependency, locations),
                                });
                            }

                            self.modify_general_compilation_unit(id, |unit| match unit {
                                GeneralCompilationUnit::Poisoned => {
                                    Err(CompileManagerError::Poisoned)
                                }
                                GeneralCompilationUnit::Defined { dependants, .. } => {
                                    dependants.push(CompilationUnitId::NamedType(type_id));
                                    n_dependencies += 1;
                                    Ok(())
                                }
                                GeneralCompilationUnit::DependencyWaiting {
                                    dependants, ..
                                } => {
                                    dependants.push(CompilationUnitId::NamedType(type_id));
                                    n_dependencies += 1;
                                    Ok(())
                                }
                                GeneralCompilationUnit::Resolved => Ok(()),
                            })?;
                        }
                        Err(NamespaceAccessError::DoesNotExist) => {
                            return Err(CompileManagerError::UndefinedDependency {
                                dependant: definition.pos.clone(),
                                dependency: (dependency, locations),
                            });
                        }
                        Err(NamespaceAccessError::Ambiguous(values)) => {
                            return Err(CompileManagerError::AmbiguousDependency {
                                dependant: definition.pos.clone(),
                                dependency: (dependency, locations),
                                defined: values.into_iter().map(|v| v.0).collect(),
                            });
                        }
                    }
                }

                *comp_unit = CompilationUnit::DependencyWaiting {
                    namespace_id,
                    definition,
                    n_dependencies,
                    dependants,
                };

                if n_dependencies == 0 {
                    self.add_ready_compilation_unit(CompilationUnitId::NamedType(type_id));
                }
            }
            CompilationUnit::DependencyWaiting {
                namespace_id,
                definition,
                n_dependencies,
                dependants,
            } => {
                assert_eq!(
                    n_dependencies, 0,
                    "DependencyWaiting wasn't ready to compile"
                );

                let (_size, _align, def_id) =
                    types::resolve_type_def(self, namespace_id, &definition)?;

                println!("Named type!");
                println!(
                    "\tsize: {} bytes, align: {} bytes, id: {:?}",
                    _size, _align, def_id
                );
                println!("\ttype: {}", &definition);

                *comp_unit = CompilationUnit::Resolved((definition, def_id));

                // Remove a dependency from the dependants
                for dependant_id in dependants {
                    self.modify_general_compilation_unit(dependant_id, |v| {
                        use GeneralCompilationUnit::*;
                        match v as GeneralCompilationUnit<'_> {
                            Poisoned => Err(CompileManagerError::Poisoned),
                            DependencyWaiting {
                                n_dependencies, ..
                            } => {
                                assert_ne!(*n_dependencies, 0);
                                *n_dependencies -= 1;

                                if *n_dependencies == 0 {
                                    self.add_ready_compilation_unit(dependant_id);
                                }

                                Ok(())
                            },
                            _ => unreachable!("Only 'DependencyWaiting' compilation units should have active dependencies"),
                        }
                    })?;
                }
            }
            _ => unimplemented!(),
        }

        Ok(())
    }

    fn modify_general_compilation_unit<V>(
        &self,
        id: CompilationUnitId,
        modifier: impl FnOnce(GeneralCompilationUnit) -> V,
    ) -> V {
        match id {
            CompilationUnitId::NamedType(type_id) => {
                let mut type_lock = self.named_types.get_mut(type_id).unwrap();
                let window = type_lock.create_window();
                modifier(window)
            }
        }
    }
}

#[derive(Clone, Copy, Hash, PartialEq, Eq, Debug)]
pub enum CompilationUnitId {
    NamedType(NamedTypeId),
}

#[derive(Debug)]
pub enum CompilationUnit<Defined, Resolved> {
    Poisoned,
    Defined {
        namespace_id: NamespaceId,
        definition: Defined,
        dependencies: Vec<(TinyString, Vec<SourcePos>)>,
        dependants: Vec<CompilationUnitId>,
    },
    DependencyWaiting {
        namespace_id: NamespaceId,
        definition: Defined,
        n_dependencies: u32,
        dependants: Vec<CompilationUnitId>,
    },
    Resolved(Resolved),
}

impl<D, R> CompilationUnit<D, R> {
    fn create_window<'a>(&'a mut self) -> GeneralCompilationUnit<'a> {
        match self {
            CompilationUnit::Poisoned => GeneralCompilationUnit::Poisoned,
            CompilationUnit::Defined {
                namespace_id,
                dependencies,
                dependants,
                ..
            } => GeneralCompilationUnit::Defined {
                namespace_id,
                dependencies,
                dependants,
            },
            CompilationUnit::DependencyWaiting {
                n_dependencies,
                dependants,
                ..
            } => GeneralCompilationUnit::DependencyWaiting {
                n_dependencies,
                dependants,
            },
            CompilationUnit::Resolved { .. } => GeneralCompilationUnit::Resolved,
        }
    }
}

#[derive(Debug)]
pub enum GeneralCompilationUnit<'a> {
    Poisoned,
    Defined {
        namespace_id: &'a mut NamespaceId,
        dependencies: &'a mut Vec<(TinyString, Vec<SourcePos>)>,
        dependants: &'a mut Vec<CompilationUnitId>,
    },
    DependencyWaiting {
        n_dependencies: &'a mut u32,
        dependants: &'a mut Vec<CompilationUnitId>,
    },
    Resolved,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Identifier {
    pub pos: SourcePos,
    pub data: TinyString,
}
