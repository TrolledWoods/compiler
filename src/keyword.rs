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
	Union,
	Enum,
	Extern,

	// Namespaces
	Use,
	Let,
	Module,
	Const,
	TypeDef,
	Alias,
	Load,
}
