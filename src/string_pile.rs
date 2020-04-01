use std::sync::Arc;
use std::sync::{ RwLock, RwLockReadGuard };
use std::ops::Deref;

struct StringPile {
	data: Vec<String>,
}

impl StringPile {
	pub fn new() -> StringPile {
		StringPile {
			data: Vec::new()
		}
	}
}

#[derive(Debug)]
pub enum TinyStringError {
	PoisonedGuard
}

impl<T> From<std::sync::PoisonError<T>> for TinyStringError {
	fn from(_error: std::sync::PoisonError<T>) -> TinyStringError {
		TinyStringError::PoisonedGuard
	}
}

#[derive(Clone)]
pub struct TinyStringCreator {
	pile: Arc<RwLock<StringPile>>,
}

impl TinyStringCreator {
	pub fn new() -> TinyStringCreator {
		let arc = Arc::new(RwLock::new(StringPile::new()));
		TinyStringCreator {
			pile: arc
		}
	}

    pub fn debug_pile(&self) {
        for string in self.pile.read().unwrap().data.iter() {
            println!("{}", string);
        }
    }

	/// insert but it panics if the lock was dirty(it's only dirty if another thread panicked,
	/// so what this does is essentially propagate panics into other threads, which I think
	/// is completely fine, as I don't want any thread to panic in the first place)
	pub fn expect_insert(&self, string: &str) -> TinyString {
		self.insert(string).expect("The lock was poisoned")
	}

	pub fn insert(&self, string: &str) -> Result<TinyString, TinyStringError> {
		let mut lock = self.pile.write()?;
		for (i, element) in lock.data.iter().enumerate() {
			if element == string {
				return Ok(TinyString {
					pile: self.pile.clone(),
					id: i,
				});
			}
		}

		let id = lock.data.len();
		lock.data.push(string.to_string());

		Ok(TinyString {
			pile: self.pile.clone(),
			id: id,
		})
	}
}

impl std::cmp::PartialEq for TinyStringCreator {
	fn eq(&self, other: &Self) -> bool {
		Arc::ptr_eq(&self.pile, &other.pile)
	}
}

/// Good for storing strings that are expected to repeat
/// (perfect for storing identifiers)
/// and good for equality checking on those strings,
/// which is also perfect for identifiers, because we have
/// to check equality on a lot of them
#[derive(Clone)]
pub struct TinyString {
	pile: Arc<RwLock<StringPile>>,
	id: usize
}

impl TinyString {
	pub fn expect_read<'a>(&'a self) -> TinyStringReadGuard<'a>  {
		self.read().expect("Lock was poisoned")
	}

	pub fn read<'a>(&'a self) -> Result<TinyStringReadGuard<'a>, TinyStringError> {
		Ok(TinyStringReadGuard {
			guard: self.pile.read()?,
			id: self.id
		})
	}

	pub fn get_creator(&self) -> TinyStringCreator {
		TinyStringCreator {
			pile: self.pile.clone()
		}
	}
}

impl std::fmt::Debug for TinyString {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "'{}'{}", &*self.read().expect("Poisoned lock in fmt::Debug") as &str, self.id)
	}
}

impl std::cmp::PartialEq<str> for TinyString {
	fn eq(&self, other: &str) -> bool {
		self.pile.read().expect("Poisoned lock, cannot resolve here in PartialEq function")
			.data[self.id] == other
	}
}

impl std::cmp::PartialEq for TinyString {
	fn eq(&self, other: &Self) -> bool {
		self.id == other.id && Arc::ptr_eq(&self.pile, &other.pile)
	}
}

pub struct TinyStringReadGuard<'a> {
	guard: RwLockReadGuard<'a, StringPile>,
	id: usize,
}

impl Deref for TinyStringReadGuard<'_> {
	type Target = str;

	fn deref<'a>(&'a self) -> &'a Self::Target {
		&self.guard.data[self.id]
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

#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	fn string_pile_test() {
		let creator = TinyStringCreator::new();
		let tiny = creator.insert("hello").unwrap();
		assert_eq!(&*tiny.read().unwrap(), "hello");
		for i in 0..300 {
			let i = creator.insert(&i.to_string()).unwrap();
			assert_ne!(i, tiny);
		}

		assert_eq!(creator.insert("hello").unwrap(), tiny);
	}
}
