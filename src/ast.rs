use crate::compilation_manager::Identifier;
use crate::lexer::SourcePos;
use crate::operator::OpKind;
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
            Operator(kind, args) => {
                for arg in args {
                    arg.get_dependencies(on_find_dep)?;
                }
            }
            UnaryOperator(kind, arg) => arg.get_dependencies(on_find_dep)?,
            FunctionCall { function, args } => {
                function.get_dependencies(on_find_dep)?;

                for arg in args {
                    arg.get_dependencies(on_find_dep)?;
                }
            }
            Function(args, returns, body) => {
                for (_, type_def) in args {
                    type_def.get_unsized_dependencies(on_find_dep)?;
                }
                for type_def in returns {
                    type_def.get_unsized_dependencies(on_find_dep)?;
                }
                body.get_dependencies(on_find_dep)?;
            }
            Array(members) => {
                for member in members {
                    member.get_dependencies(on_find_dep)?;
                }
            }
            Collection(kind) => match kind {
                CollectionDefKind::Named(members) => {
                    for (_, member) in members {
                        member.get_dependencies(on_find_dep)?;
                    }
                }
                CollectionDefKind::Unnamed(members) => {
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
            StringLiteral(_) | IntLiteral(_) | FloatLiteral(_) => (),
            Block(_, _) => unimplemented!(),
        }

        Ok(())
    }
}

#[derive(Debug)]
pub enum CollectionDefKind {
    Named(Vec<(Identifier, ExpressionDef)>),
    Unnamed(Vec<ExpressionDef>),
}

#[derive(Debug)]
pub enum ExpressionDefKind {
    Operator(OpKind, Vec<ExpressionDef>),
    UnaryOperator(OpKind, Box<ExpressionDef>),
    FunctionCall {
        function: Box<ExpressionDef>,
        args: Vec<ExpressionDef>,
    },
    Function(Vec<(Identifier, TypeDef)>, Vec<TypeDef>, Box<ExpressionDef>),
    Offload(TinyString),
    Array(Vec<ExpressionDef>),
    Collection(CollectionDefKind),

    StringLiteral(TinyString),
    IntLiteral(i128),
    FloatLiteral(f64),
    Block(Vec<StatementDef>, Box<ExpressionDef>),
}

impl ExpressionDefKind {
    pub fn pretty_print(&self, indent: usize) {
        use ExpressionDefKind::*;
        match self {
            Operator(kind, args) => {
                assert_eq!(args.len(), 2);

                print!("[{} ", kind.glyph());
                args[0].pretty_print(indent);
                print!(", ");
                args[1].pretty_print(indent);
                print!("]");
            }
            UnaryOperator(kind, arg) => {
                print!("unary {} ", kind.glyph());
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
                CollectionDefKind::Named(members) => {
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
                CollectionDefKind::Unnamed(members) => {
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
            StringLiteral(content) => print!("\"{}\"", content),
            IntLiteral(content) => print!("{}", content),
            FloatLiteral(content) => print!("{}", content),
            Block(statements, expression) => {
                if statements.len() > 0 {
                    println!("(");
                    for statement in statements {
                        print_indent(indent + 1);
                        statement.pretty_print(indent + 1);
                        println!(";");
                    }

                    print_indent(indent + 1);
                    expression.pretty_print(indent + 1);
                    println!("");

                    print_indent(indent);
                    print!(")");
                } else {
                    print!("(");
                    expression.pretty_print(indent);
                    print!(")");
                }
            }
            Function(args, returns, body) => {
                print!("[");
                for (i, (name, member)) in args.iter().enumerate() {
                    if i > 0 {
                        print!(", ");
                    }
                    print!("{}: {}", name.data, member);
                }
                print!("] -> [");
                for (i, member) in returns.iter().enumerate() {
                    if i > 0 {
                        print!(", ");
                    }
                    print!("{}", member);
                }
                print!("] ");
                body.pretty_print(indent);
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
    Declaration(Identifier, ExpressionDef),
    Assignment(ExpressionDef, OpKind, ExpressionDef),
    Expression(ExpressionDef),
    Block(Vec<StatementDef>),
}

impl StatementDef {
    fn pretty_print(&self, indent: usize) {
        use StatementDef::*;
        match self {
            Declaration(ident, expr) => {
                print!("let {} = ", ident.data);
                expr.pretty_print(indent);
            }
            Assignment(left, expr, right) => {
                left.pretty_print(indent);
                print!(" {}= ", expr.glyph());
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
