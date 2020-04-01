#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum OpKind {
    NoOp,
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
