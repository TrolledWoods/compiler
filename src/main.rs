#![allow(dead_code)]
#![allow(unused_imports)]
// TODO: Remove this, but for
// development it doesn't matter
#![allow(unused)]

use std::path::PathBuf;
use std::sync::Arc;

extern crate ansi_term;
extern crate chashmap;
#[macro_use]
extern crate lazy_static;

#[macro_use]
mod id;
#[macro_use]
mod debugging;

mod ast;
mod compilation_manager;
mod debug_printing;
mod error;
mod keyword;
mod lexer;
mod misc;
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
		None => PathBuf::from("syntax-test.txt"),
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

	let manager = match Arc::try_unwrap(manager) {
		Ok(value) => value,
		Err(_) => panic!("Some thread is still alive and keeps an Arc to the compilation manager"),
	};

	let mut compilation_errors = Vec::new();
	while let Some(id) = manager.get_ready_compilation_unit() {
		match manager.advance_compilation_unit(id) {
			Ok(()) => (),
			Err(error) => compilation_errors.push(error),
		}
	}

	if compilation_errors.len() > 0 {
		use crate::error::CompileError;

		println!("There were errors while compiling!\n");

		for error in compilation_errors {
			error.get_printing_data().print();
		}

		return;
	}

	println!("Built succesfully! :D");
}
