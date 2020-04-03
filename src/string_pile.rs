use std::sync::Arc;
use std::sync::{ RwLock, RwLockReadGuard };
use std::ops::Deref;

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
    pub fn new(s: impl AsRef<str>) -> TinyString {
        s.as_ref().into()
    }

    pub fn read<'a>(&self) -> TinyStringReadGuard<'a> {
        TinyStringReadGuard {
            guard: read_pile(),
            id: self.0
        }
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
	id: usize,
}

impl Deref for TinyStringReadGuard<'_> {
	type Target = str;

	fn deref<'a>(&'a self) -> &'a Self::Target {
		&self.guard[self.id]
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
		let tiny: TinyString = "hello".into();
		assert_eq!(&*tiny.read(), "hello");
		for i in 0..300 {
			let i: TinyString = i.to_string().into();
			assert_ne!(i, tiny);
		}

		assert_eq!(TinyString::new("hello"), tiny);
	}

    #[test]
    fn threading_test() {
        use std::thread;

        let other = thread::spawn(move || {
            let a: TinyString = "Hello world".into();
            let b: TinyString = "Yo".into();

            assert_ne!(a, b, "Thread stuff doesn't work");
            b
        });

        let c: TinyString = "Whut".into();
        let d: TinyString = "Yo".into();
        let b = other.join().unwrap();

        assert_eq!(d, b, "String from different threads aren't equal");
    }
}
