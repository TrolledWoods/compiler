use crate::string_pile::TinyString;
use crate::compilation_manager::Identifier;
use crate::lexer::SourcePos;

#[derive(Debug)]
pub struct TypeDef {
    pub pos: SourcePos,
    pub kind: TypeKind,
}

#[derive(Debug)]
pub enum TypeKind {
    Offload{
        name: Identifier, 
        generics: Vec<TypeDef>,
    },
    Tuple(Vec<TypeDef>),
    FunctionPtr(FunctionHeader),
}

#[derive(Debug)]
pub struct FunctionHeader {
    pub args: Vec<TypeDef>,
    pub returns: Vec<TypeDef>,
}
