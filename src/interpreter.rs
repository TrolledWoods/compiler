use crate::types::{ResolvedType, ResolvedTypeId};
use crate::id::CIdMap;
use crate::ast::{ExpressionDef, ExpressionDefKind};

pub struct Value {
    pub type_id: ResolvedTypeId,
    pub data: Box<[u8]>,
}

pub fn interpret(
    types: &CIdMap<ResolvedTypeId, ResolvedType>,
    stack: &mut Vec<(TinyString, ResolvedTypeId, Vec<u8>)>, 
    value: &Expression
) -> Result<Value, InterpreterError> {
    use ExpressionKind::*;
    match value {
    }
}
