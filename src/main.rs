extern crate chashmap;
#[macro_use]
extern crate lazy_static;

mod compilation_manager;
mod keyword;
mod lexer;
mod namespace;
mod operator;
mod parser;
mod string_pile;
mod types;

fn main() {
    let input = std::fs::read_to_string("syntax-test.txt").unwrap();

    let mut lexer = lexer::Lexer::new(&input);
    let manager = compilation_manager::CompilationManager::new();
    let mut parser = parser::Parser {
        manager: &manager,
        namespace: "".into(),
        file: "syntax-test.txt".into(),
        tokens: lexer,
    };

    parser::parse_namespace(&mut parser).unwrap();
    //manager.log_all();
}
