use std::ops::Deref;
use std::sync::Arc;
use std::sync::{RwLock, RwLockReadGuard};

lazy_static! {
	static ref PILE: Arc<RwLock<Vec<String>>> = Arc::new(RwLock::new(Vec::new()));
}

fn read_pile<'a>() -> RwLockReadGuard<'a, Vec<String>> {
	PILE.read().expect("The string pile is poisoned!")
}

/// Good for storing strings that are expected to repeat
/// (perfect for storing identifiers)
/// and good for equality checking on those strings,
/// which is also perfect for identifiers, because we have
/// to check equality on a lot of them
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct TinyString(usize);

impl TinyString {
	pub fn read<'a>(&'a self) -> TinyStringReadGuard<'a> {
		let guard = PILE.read().unwrap();

		TinyStringReadGuard {
			guard,
			index: self.0,
		}
	}
}

impl std::cmp::PartialEq<&str> for TinyString {
	fn eq(&self, other: &&str) -> bool {
		let other: TinyString = (*other).into();
		*self == other
	}
}

impl From<String> for TinyString {
	fn from(string: String) -> TinyString {
		(&string as &str).into()
	}
}

impl From<&str> for TinyString {
	fn from(string: &str) -> TinyString {
		// Search for an existing string
		let mut locked_pile = PILE.write().expect("The string pile is poisoned!");
		for (i, elem) in locked_pile.iter().enumerate() {
			if elem == string {
				return TinyString(i);
			}
		}

		let index = locked_pile.len();
		locked_pile.push(string.to_string());

		TinyString(index)
	}
}

impl std::fmt::Debug for TinyString {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "'{}'{}", &*read_pile()[self.0] as &str, self.0)
	}
}

impl std::fmt::Display for TinyString {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{}", &*read_pile()[self.0] as &str)
	}
}

pub struct TinyStringReadGuard<'a> {
	guard: RwLockReadGuard<'a, Vec<String>>,
	index: usize,
}

impl Deref for TinyStringReadGuard<'_> {
	type Target = str;

	fn deref<'a>(&'a self) -> &'a Self::Target {
		&self.guard[self.index]
	}
}

impl std::fmt::Display for TinyStringReadGuard<'_> {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{}", &*self)
	}
}

impl std::fmt::Debug for TinyStringReadGuard<'_> {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{}", &*self)
	}
}
