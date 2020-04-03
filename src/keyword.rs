#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub enum Keyword {
    // Flow control
    While,
    Loop,
    For,
    If,
    Else,
    Return,

    // Types
    Struct,
    Union,
    Type,
    Enum,
    Extern,

    // Namespaces
    Use,
    Module,
}
