#![allow(warnings)]

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
// mod types;

fn main() {
    let input = std::fs::read_to_string("syntax-test.txt").unwrap();

    let mut lexer = lexer::Lexer::new(&input);
    let manager = compilation_manager::CompileManager::new();
    let mut parser = parser::Parser {
        manager: &manager,
        file: "syntax-test.txt".into(),
        tokens: lexer,
    };

    let root = manager.namespace.create_root();
    parser::parse_namespace(&mut parser, false, root).unwrap();
    //manager.log_all();
}
