pub fn collect_to_vec_if_ok<V, E>(iter: impl Iterator<Item = Result<V, E>>) -> Result<Vec<V>, E> {
	let mut vec = if let (_, Some(s)) = iter.size_hint() {
		Vec::with_capacity(s)
	} else {
		Vec::new()
	};

	for item in iter {
		vec.push(item?);
	}

	Ok(vec)
}

pub fn fail_collect<V, E>(iter: impl Iterator<Item = Result<V, E>>) -> Result<Vec<V>, E> {
	let mut vec = if let (_, Some(s)) = iter.size_hint() {
		Vec::with_capacity(s)
	} else {
		Vec::new()
	};

	for item in iter {
		vec.push(item?);
	}

	Ok(vec)
}
