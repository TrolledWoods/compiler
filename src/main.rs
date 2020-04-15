// #![allow(warnings)]
use std::sync::Arc;
use std::path::{ PathBuf };

extern crate ansi_term;
extern crate chashmap;
#[macro_use]
extern crate lazy_static;

#[macro_use]
mod id;

mod compilation_manager;
mod keyword;
mod lexer;
mod namespace;
mod operator;
mod parser;
mod string_pile;
mod error;
mod debug_printing;
mod types;

pub const SRC_EXTENSION: &str = "txt";

fn main() {
    let mut args = std::env::args();

    args.next();

    let mut src_file_path = match args.next() {
        Some(value) => PathBuf::from(value),
        None => {
            println!("Please give the path to the file you want to compile");
            return;
        }
    };

    assert_eq!(args.next(), None, "Too many console arguments passed!");


    println!("{:?}", src_file_path);

    let input = std::fs::read_to_string(&src_file_path).unwrap();

    let mut lexer = lexer::Lexer::new(src_file_path.to_str().unwrap().into(), &input);
    let manager = Arc::new(compilation_manager::CompileManager::new());

    let mut parser = parser::Parser {
        manager: manager.clone(),
        file: src_file_path.to_str().expect("String conversion not possible :<").into(),
        tokens: lexer,
    };

    let stem = PathBuf::from(src_file_path.file_stem().unwrap());
    src_file_path.pop();
    src_file_path.push(stem);

    let root = manager.namespace_manager.insert_root();
    let mut errors = Vec::new();
    match parser::parse_namespace(&mut parser, false, root) {
        Ok(()) => (),
        Err(err) => errors.push(err),
    }

    if errors.len() > 0 {
        use crate::error::CompileError;

        println!("There were errors!");

        for error in errors {
            error.get_printing_data().print();
        }

        return;
    }

    std::mem::drop(parser);

    // Down here we should be the sole owner of the manager(because parse_namespace should join the
    // child thread it spawns
    let owned_manager = match Arc::try_unwrap(manager) {
        Ok(value) => value,
        Err(_) => panic!("Some thread is still alive and keeps an Arc to the compilation manager"),
    };
}
