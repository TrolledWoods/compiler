use crate::ast::{Expression, ExpressionKind};
use crate::compilation_manager::{CompileManager, CompileManagerError};
use crate::id::CIdMap;
use crate::lexer;
use crate::string_pile::TinyString;
use crate::types::{PrimitiveKind, ResolvedTypeDef, ResolvedTypeId};

#[derive(Debug)]
pub enum InterpreterError {
	CompileError(CompileManagerError),
}

impl From<CompileManagerError> for InterpreterError {
	fn from(other: CompileManagerError) -> InterpreterError {
		InterpreterError::CompileError(other)
	}
}

#[derive(Debug)]
pub enum ValueKind {
	Int(i128),
	Float(f64),
}

#[derive(Debug)]
pub struct Value {
	pub type_id: Option<ResolvedTypeId>,
	pub kind: ValueKind,
}

impl Value {
	fn kind(kind: ValueKind) -> Value {
		Value {
			type_id: None,
			kind,
		}
	}
}

pub fn interpret(
	manager: &CompileManager,
	value: &Expression,
	wanted_type: Option<ResolvedTypeId>,
) -> Result<Option<Value>, InterpreterError> {
	let mut stack = Vec::new();
	let value = interpret_req(manager, &mut stack, value)?;

	Ok(value)
}

pub fn interpret_req(
	manager: &CompileManager,
	stack: &mut Vec<(TinyString, ResolvedTypeId, Vec<u8>)>,
	value: &Expression,
) -> Result<Option<Value>, InterpreterError> {
	use ExpressionKind::*;
	match &value.kind {
		Literal(literal) => {
			use lexer::Literal;
			match literal {
				Literal::String(_) => unimplemented!(),
				Literal::Int(value) => Ok(Some(Value::kind(ValueKind::Int(*value)))),
				Literal::Float(value) => Ok(Some(Value::kind(ValueKind::Float(*value)))),
			}
		}
		Operator(op, args) => {
			assert_eq!(
				args.len(),
				2,
				"Cannot have other than 2 arguments to operator atm"
			);
			let mut args = args.iter();
			let arg1 = interpret_req(manager, stack, args.next().unwrap())?.unwrap();
			let arg2 = interpret_req(manager, stack, args.next().unwrap())?.unwrap();

			match (&arg1.kind, &arg2.kind) {
				(ValueKind::Int(a), ValueKind::Int(b)) => Ok(Some(match *op {
					"+" => Value::kind(ValueKind::Int(a + b)),
					"-" => Value::kind(ValueKind::Int(a - b)),
					"*" => Value::kind(ValueKind::Int(a * b)),
					"/" => Value::kind(ValueKind::Int(a / b)),
					_ => unimplemented!(),
				})),
				(ValueKind::Float(a), ValueKind::Float(b)) => Ok(Some(match *op {
					"+" => Value::kind(ValueKind::Float(a + b)),
					"-" => Value::kind(ValueKind::Float(a - b)),
					"*" => Value::kind(ValueKind::Float(a * b)),
					"/" => Value::kind(ValueKind::Float(a / b)),
					_ => unimplemented!(),
				})),
				_ => unimplemented!(),
			}
		}
		_ => unimplemented!(),
	}
}
