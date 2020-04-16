use crate::id::CIdMap;
use crate::lexer::SourcePos;
use crate::namespace::{AllowAmbiguity, NamespaceError, NamespaceId, NamespaceManager};
use crate::string_pile::TinyString;
use crate::types::{TypeDef, TypeKind};
use std::sync::atomic::AtomicU32;
use std::sync::Mutex;

pub type Dependencies = Vec<(TinyString, Vec<SourcePos>)>;

#[derive(Debug)]
pub enum CompileManagerError {
    UndefinedDependency {
        dependant: Identifier,
        dependency: (TinyString, Vec<SourcePos>),
    },
    AmbiguousDependency {
        dependant: Identifier,
        dependency: (TinyString, Vec<SourcePos>),
        defined: Vec<Identifier>,
    },
}

pub enum ResolvedTypeMode {
    UnnamedType,
    NamedType,
}

pub struct CompileManager {
    pub namespace_manager: NamespaceManager,

    // TODO: Change the type of this
    // to something better. Maybe a concurrent queue of some sort?
    ready_to_compile: Mutex<Vec<CompilationUnitId>>,

    pub named_types: CIdMap<NamedTypeId, CompilationUnit<TypeDef, (TypeDef, ResolvedTypeId)>>,
    pub resolved_types: CIdMap<ResolvedTypeId, (ResolvedTypeMode, ResolvedTypeDef)>,
}

impl CompileManager {
    pub fn new() -> CompileManager {
        CompileManager {
            namespace_manager: NamespaceManager::new(),
            ready_to_compile: Mutex::new(vec![]),

            named_types: CIdMap::new(),
            resolved_types: CIdMap::new(),
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
            &mut comp_unit as &mut CompilationUnit<_, _>,
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
                for (dependency, _locations) in dependencies.iter() {
                    match self.namespace_manager.get_member(namespace_id, *dependency) {
                        Ok((_pos, id)) => {
                            self.modify_general_compilation_unit(id, |unit| match unit {
                                GeneralCompilationUnit::Poisoned => unimplemented!(),
                                GeneralCompilationUnit::Defined { dependants, .. } => {
                                    dependants.push(CompilationUnitId::NamedType(type_id));
                                    n_dependencies += 1;
                                    ()
                                }
                                GeneralCompilationUnit::DependencyWaiting {
                                    dependants, ..
                                } => {
                                    dependants.push(CompilationUnitId::NamedType(type_id));
                                    n_dependencies += 1;
                                    ()
                                }
                                GeneralCompilationUnit::Resolved => (),
                            });
                        }
                        _ => unimplemented!(),
                    }
                }

                *comp_unit = CompilationUnit::DependencyWaiting {
                    namespace_id,
                    definition,
                    n_dependencies: AtomicU32::new(n_dependencies),
                    dependants,
                };

                println!("{:?}", comp_unit);

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
                let n_dependencies = n_dependencies.into_inner();
                assert_eq!(
                    n_dependencies, 0,
                    "DependencyWaiting wasn't ready to compile"
                );
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

create_id!(ResolvedTypeId);
create_id!(NamedTypeId);

pub struct ResolvedTypeDef {
    pub pos: SourcePos,
    pub size: usize,
    pub align: usize,
    pub kind: TypeKind<ResolvedTypeDef, ResolvedTypeId>,
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
        n_dependencies: AtomicU32,
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
        n_dependencies: &'a mut AtomicU32,
        dependants: &'a mut Vec<CompilationUnitId>,
    },
    Resolved,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Identifier {
    pub pos: SourcePos,
    pub data: TinyString,
}
