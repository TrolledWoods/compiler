use std::num::NonZeroU8;

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum OpKind {
    NoOp,
    Assignment,
    Declaration,
    Constant,
    ReturnArrow,
    Dereference,
    Access,
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Pow,
    Equal,
    NotEqual,
    And,
    Or,
    Not,
    Less,
    Greater,
    LessEq,
    GreaterEq,
    BitAnd,
    BitOr,
    BitNot,
}

impl OpKind {
    /// Greater number means greater priority.
    pub fn priority(&self) -> NonZeroU8 {
        use OpKind::*;
        // TODO: Make sure that these
        // orders make sense
        let priority = match self {
            And | Or | Not => 1,
            Equal | NotEqual | Greater | Less | GreaterEq | LessEq => 2,
            Add | Sub => 3,
            Mul | Div | Mod => 4,
            Pow | BitAnd | BitOr | BitNot => 5,
            _ => unimplemented!(),
        };

        NonZeroU8::new(priority).unwrap()
    }
}
