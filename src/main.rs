mod lexer;
mod string_pile;
mod operator;
mod keyword;

fn main() {
	let input = std::fs::read_to_string("syntax-test.txt").unwrap();

	let strings = string_pile::TinyStringCreator::new();
	let mut lexer = lexer::Lexer::new(&input, strings);

	while match lexer.eat_token() {
		Ok(Some(token)) => {
			println!("{}-{}: {:?}", token.start, token.end, token.kind);
			//println!(" :eyes: {:?}", lexer.peek_token(3));
			true
		},
		Ok(None) => {
			println!("End of file!");
			false
		},
		Err(error) => {
			println!("{:?}", error);
			false
		},
	} {}
}
