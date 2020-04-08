// #![allow(warnings)]
use std::sync::Arc;
use std::path::{ Path, PathBuf };

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

pub const SRC_EXTENSION: &str = "txt";

fn main() {
    let current_dir = std::env::current_dir().unwrap();

    let mut args = std::env::args();
    let file_arg = match args.next() {
        Some(value) => value,
        None => {
            println!("Please give the path to the file you want to compile");
            return;
        }
    };

    //let input = std::fs::read_to_string().unwrap();

    let input = std::fs::read_to_string("syntax-test.txt").unwrap();
    let mut lexer = lexer::Lexer::new(&input);
    let manager = Arc::new(compilation_manager::CompileManager::new());
    let path = std::path::Path::new("syntax-test.txt");

    let mut parser = parser::Parser {
        manager: manager.clone(),
        file: path.to_str().expect("you suck").into(),
        tokens: lexer,
    };

    let root = manager.namespace.create_root();
    parser::parse_namespace(&mut parser, false, root, Some(PathBuf::from("hi"))).unwrap();
    //manager.log_all();
}
