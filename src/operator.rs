use std::num::NonZeroU8;

pub fn priority(op: &str) -> NonZeroU8 {
	let num = match op {
		"*" | "/" | "%" => 1,
		"+" | "-" => 2,
		"<<" | ">>" => 3,
		"<=" | ">=" | "<" | ">" => 4,
		"==" | "!=" => 5,
		"&" => 6,
		"^" => 7,
		"|" => 8,
		"&&" => 9,
		"||" => 10,
		_ => unreachable!("Invalid operator {}!", op),
	};

	NonZeroU8::new(num).unwrap()
}
