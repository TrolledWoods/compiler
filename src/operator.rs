use std::num::NonZeroU8;

pub fn priority(op: &str) -> NonZeroU8 {
	let num = match op {
		"*" | "/" | "%" => 10,
		"+" | "-" => 9,
		"<<" | ">>" => 8,
		"<=" | ">=" | "<" | ">" => 7,
		"==" | "!=" => 6,
		"&" => 5,
		"^" => 4,
		"|" => 3,
		"&&" => 2,
		"||" => 1,
		_ => unreachable!("Invalid operator {}!", op),
	};

	NonZeroU8::new(num).unwrap()
}
