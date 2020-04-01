# Compiler

A compiler for some kind of strange
language written in rust.

Is going to compile to a special byte code for a stack based
VM when it's done, but it's not even close to done yet.

# Todo list: (Not necessarily in order)
--- Lexer and parser run in parallel on different files, i.e. we run them on 1 thread / file
* Lexer (almost done)
* Parser (should be done by 4-03 if I work hard)
* Multithreaded compilation (compilation unit based compilation, should take a while and pretty complicated concept)
* Make multithreaded compilation sync threads properly (which isn't going to be easy, fyi)
* Think about syntax
* Make a virtual machine.
* Support externally defined functions, and export functions to the compiler
* Build an API to run the VM and write wrappers for programs?
