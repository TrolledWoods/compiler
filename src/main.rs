use std::path::PathBuf;
use std::sync::Arc;

extern crate ansi_term;
extern crate chashmap;
#[macro_use]
extern crate lazy_static;

#[macro_use]
mod id;

mod compilation_manager;
mod debug_printing;
mod error;
mod keyword;
mod lexer;
mod namespace;
mod operator;
mod parser;
mod string_pile;
mod types;

pub const SRC_EXTENSION: &str = "gai";

fn main() {
    let mut args = std::env::args();

    args.next();

    let src_file_path = match args.next() {
        Some(value) => PathBuf::from(value),
        None => {
            println!("Please give the path to the file you want to compile");
            return;
        }
    };

    assert_eq!(args.next(), None, "Too many console arguments passed!");

    let manager = Arc::new(compilation_manager::CompileManager::new());

    // Parsing step
    let root = manager.namespace_manager.insert_root();
    let mut errors = Vec::new();
    if let Err(err) = parser::parse_file(&src_file_path, manager.clone(), root) {
        errors.push(err);
    }

    if errors.len() > 0 {
        use crate::error::CompileError;

        println!("There were errors while parsing!\n");

        for error in errors {
            error.get_printing_data().print();
        }

        return;
    }

    let _owned_manager = match Arc::try_unwrap(manager) {
        Ok(value) => value,
        Err(_) => panic!("Some thread is still alive and keeps an Arc to the compilation manager"),
    };

    // Now we have parsed everything.
}
