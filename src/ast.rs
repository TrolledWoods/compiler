use crate::compilation_manager::Identifier;
use crate::lexer::SourcePos;
use crate::operator::OpKind;
use crate::string_pile::TinyString;
use crate::types::TypeDef;

pub struct ExpressionDef {
    pub pos: SourcePos,
    pub kind: ExpressionDefKind,
}

impl ExpressionDef {
    fn get_dependencies<E>(
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
            Function(args, returns) => {
                for (_, type_def) in args {
                    type_def.get_unsized_dependencies(on_find_dep)?;
                }
                for type_def in returns {
                    type_def.get_unsized_dependencies(on_find_dep)?;
                }
            }
            Offload(name) => {
                on_find_dep(Identifier {
                    data: *name,
                    pos: self.pos.clone(),
                })?;
            }
            StringLiteral(_) | IntLiteral(_) | FloatLiteral(_) => (),
        }

        Ok(())
    }
}

pub enum ExpressionDefKind {
    Operator(OpKind, Vec<ExpressionDef>),
    UnaryOperator(OpKind, Box<ExpressionDef>),
    FunctionCall {
        function: Box<ExpressionDef>,
        args: Vec<ExpressionDef>,
    },
    Function(Vec<(Identifier, TypeDef)>, Vec<TypeDef>),
    Offload(TinyString),

    StringLiteral(TinyString),
    IntLiteral(i128),
    FloatLiteral(f64),
}
