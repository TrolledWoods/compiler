use crate::compilation_manager::{FunctionId, Identifier};
use crate::lexer::{Literal, SourcePos};
use crate::parser::ListKind;
use crate::string_pile::TinyString;
use crate::types::TypeDef;

fn print_indent(indent: usize) {
	(0..indent).for_each(|_| print!("| "));
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

	pub fn get_dependencies<E>(
		&self,
		mut on_find_dep: &mut impl FnMut(Identifier) -> Result<(), E>,
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
				println!("MAKE DEPENDENCIES SUPPORT COMPILATION UNIT ID:S TOO");
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
			Offload(name) => {
				on_find_dep(Identifier {
					data: *name,
					pos: self.pos.clone(),
				})?;
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
	Offload(TinyString),
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
			Offload(name) => print!("{}", name),
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
