use crate::compilation_manager::{
	CompilationUnitId, CompileManager, CompileManagerError, Dependency, FunctionId, Identifier,
};
use crate::lexer::{Literal, SourcePos};
use crate::namespace::NamespaceId;
use crate::parser::ListKind;
use crate::string_pile::TinyString;
use crate::types::{ResolvedTypeId, TypeDef};

fn print_indent(indent: usize) {
	(0..indent).for_each(|_| print!("| "));
}

#[derive(Debug)]
pub struct Expression {
	pub pos: SourcePos,
	pub kind: ExpressionKind,
}

#[derive(Debug)]
pub enum ExpressionKind {
	Operator(&'static str, Vec<Expression>),
	UnaryOperator(&'static str, Box<Expression>),
	FunctionCall {
		function: Box<Expression>,
		args: Vec<Expression>,
	},
	Function(FunctionId),
	Offload(TinyString, NamespaceId),
	Array(Vec<Expression>),
	Collection(ListKind<Expression, Expression>),

	Literal(Literal),
	Block(Vec<Statement>, Option<Box<Expression>>),
}

#[derive(Debug)]
pub enum Statement {
	Declaration(Identifier, ResolvedTypeId, Expression),
	Assignment(Expression, &'static str, Expression),
	Expression(Expression),
	Block(Vec<Statement>),
}

#[derive(Debug)]
pub struct ExpressionDef {
	pub pos: SourcePos,
	pub kind: ExpressionDefKind,
}

impl ExpressionDef {
	pub fn pretty_print(&self, indent: usize) {
		self.kind.pretty_print(indent);
	}

	pub fn as_expression(
		&self,
		manager: &CompileManager,
	) -> Result<Expression, CompileManagerError> {
		use ExpressionDefKind::*;
		let new_kind = match &self.kind {
			Operator(op, args) => {
				let mut new_args = Vec::with_capacity(args.len());
				for arg in args {
					new_args.push(arg.as_expression(manager)?);
				}

				ExpressionKind::Operator(op, new_args)
			}
			UnaryOperator(op, arg) => {
				ExpressionKind::UnaryOperator(op, Box::new(arg.as_expression(manager)?))
			}
			FunctionCall { function, args } => {
				let mut new_args = Vec::with_capacity(args.len());
				for arg in args {
					new_args.push(arg.as_expression(manager)?);
				}

				ExpressionKind::FunctionCall {
					function: Box::new(function.as_expression(manager)?),
					args: new_args,
				}
			}
			Function(id) => ExpressionKind::Function(*id),
			Offload(name, namespace_id) => ExpressionKind::Offload(*name, *namespace_id),
			Array(members) => {
				let members = members
					.into_iter()
					.map(|arg| arg.as_expression(manager))
					.collect::<Result<Vec<_>, _>>()?;
				ExpressionKind::Array(members)
			}
			Collection(kind) => match kind {
				ListKind::Empty => ExpressionKind::Collection(ListKind::Empty),
				ListKind::Named(elements) => {
					let elements: Vec<_> = elements
						.into_iter()
						.map(|(name, arg)| Ok((name.clone(), arg.as_expression(manager)?)))
						.collect::<Result<Vec<_>, _>>()?;
					ExpressionKind::Collection(ListKind::Named(elements))
				}
				ListKind::Unnamed(elements) => {
					let elements: Vec<_> = elements
						.into_iter()
						.map(|arg| arg.as_expression(manager))
						.collect::<Result<Vec<_>, _>>()?;
					ExpressionKind::Collection(ListKind::Unnamed(elements))
				}
			},
			Literal(literal) => ExpressionKind::Literal(literal.clone()),
			Block(statements, returns) => {
				let statements = statements
					.into_iter()
					.map(|def| def.as_statement(manager))
					.collect::<Result<Vec<_>, _>>()?;

				let returns = match returns {
					Some(value) => Some(Box::new(value.as_expression(manager)?)),
					None => None,
				};

				ExpressionKind::Block(statements, returns)
			}
			_ => unimplemented!(),
		};

		Ok(Expression {
			pos: self.pos.clone(),
			kind: new_kind,
		})
	}

	pub fn get_dependencies<E>(
		&self,
		mut on_find_dep: &mut impl FnMut(Dependency) -> Result<(), E>,
	) -> Result<(), E> {
		use ExpressionDefKind::*;
		match &self.kind {
			Operator(_, args) => {
				for arg in args {
					arg.get_dependencies(on_find_dep)?;
				}
			}
			UnaryOperator(_, arg) => arg.get_dependencies(on_find_dep)?,
			FunctionCall { function, args } => {
				function.get_dependencies(on_find_dep)?;

				for arg in args {
					arg.get_dependencies(on_find_dep)?;
				}
			}
			Function(id) => {
				on_find_dep(Dependency::CompUnit(
					self.pos.clone(),
					CompilationUnitId::Function(*id),
				))?;
			}
			Array(members) => {
				for member in members {
					member.get_dependencies(on_find_dep)?;
				}
			}
			Collection(kind) => match kind {
				ListKind::Empty => (),
				ListKind::Named(members) => {
					for (_, member) in members {
						member.get_dependencies(on_find_dep)?;
					}
				}
				ListKind::Unnamed(members) => {
					for member in members {
						member.get_dependencies(on_find_dep)?;
					}
				}
			},
			Offload(name, namespace_id) => {
				on_find_dep(Dependency::Name(
					*namespace_id,
					Identifier {
						data: *name,
						pos: self.pos.clone(),
					},
				))?;
			}
			Literal(_) => (),
			Block(_, _) => unimplemented!(),
		}

		Ok(())
	}
}

#[derive(Debug)]
pub enum ExpressionDefKind {
	Operator(&'static str, Vec<ExpressionDef>),
	UnaryOperator(&'static str, Box<ExpressionDef>),
	FunctionCall {
		function: Box<ExpressionDef>,
		args: Vec<ExpressionDef>,
	},
	Function(FunctionId),
	Offload(TinyString, NamespaceId),
	Array(Vec<ExpressionDef>),
	Collection(ListKind<ExpressionDef, ExpressionDef>),

	Literal(Literal),
	Block(Vec<StatementDef>, Option<Box<ExpressionDef>>),
}

impl ExpressionDefKind {
	pub fn pretty_print(&self, indent: usize) {
		use ExpressionDefKind::*;
		match self {
			Operator(kind, args) => {
				assert_eq!(args.len(), 2);

				print!("[{} ", kind);
				args[0].pretty_print(indent);
				print!(", ");
				args[1].pretty_print(indent);
				print!("]");
			}
			UnaryOperator(kind, arg) => {
				print!("unary {} ", kind);
				arg.pretty_print(indent);
			}
			Offload(name, id) => print!("{:?}'{}'", id, name),
			Array(members) => {
				print!("[");
				for member in members {
					member.pretty_print(indent);
					print!(", ");
				}
				print!("]");
			}
			Collection(kind) => match kind {
				ListKind::Empty => {
					println!("{}", "{}");
				}
				ListKind::Named(members) => {
					println!("{}", '{');
					for (name, member) in members {
						print_indent(indent + 1);
						print!("{}: ", name.data);
						member.pretty_print(indent + 1);
						println!(",");
					}
					print_indent(indent);
					print!("{}", '}');
				}
				ListKind::Unnamed(members) => {
					println!("{}", '{');
					for member in members {
						print_indent(indent + 1);
						member.pretty_print(indent + 1);
						println!(",");
					}
					print_indent(indent);
					print!("{}", '}');
				}
			},
			Literal(content) => print!("{}", content),
			Block(statements, expression) => {
				if statements.len() > 0 {
					println!("(");
					for statement in statements {
						print_indent(indent + 1);
						statement.pretty_print(indent + 1);
						println!(";");
					}

					if let Some(expression) = &expression {
						print_indent(indent + 1);
						expression.pretty_print(indent + 1);
						println!("");
					}

					print_indent(indent);
					print!(")");
				} else {
					if let Some(expression) = expression {
						print!("(");
						expression.pretty_print(indent);
						print!(")");
					}
				}
			}
			Function(id) => {
				println!("{:?}", id);
			}
			FunctionCall { function, args } => {
				function.pretty_print(indent);

				print!(" [");
				for member in args {
					member.pretty_print(indent);
					print!(", ");
				}
				print!("]");
			}
		}
	}
}

#[derive(Debug)]
pub enum StatementDef {
	Declaration(Identifier, Option<TypeDef>, ExpressionDef),
	Assignment(ExpressionDef, &'static str, ExpressionDef),
	Expression(ExpressionDef),
	Block(Vec<StatementDef>),
}

impl StatementDef {
	fn as_statement(&self, manager: &CompileManager) -> Result<Statement, CompileManagerError> {
		use StatementDef::*;
		match self {
			Declaration(name, ty, expression) => {
				let ty = ty.as_ref().expect("We don't support type inference yet");

				unimplemented!();
			}
			_ => unimplemented!(),
		}
	}

	fn pretty_print(&self, indent: usize) {
		use StatementDef::*;
		match self {
			Declaration(ident, type_, expr) => {
				print!("let {}", ident.data);
				if let Some(type_) = type_ {
					print!(": {}", type_);
				}
				print!(" = ");
				expr.pretty_print(indent);
			}
			Assignment(left, expr, right) => {
				left.pretty_print(indent);
				print!(" {}= ", expr);
				right.pretty_print(indent);
			}
			Expression(expr) => expr.pretty_print(indent),
			Block(statements) => {
				println!("(");
				for statement in statements {
					print_indent(indent + 1);
					statement.pretty_print(indent + 1);
					println!(";");
				}

				print_indent(indent);
				print!(")");
			}
		}
	}
}
