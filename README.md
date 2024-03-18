# Compiler project
This is a course project for University of Helsinki course CSM14204 Compilers.
The project is a simple end-to-end compiler for a simple language specified in the [course material](https://hy-compilers.github.io/spring-2024/).
The course has ended, so this repository most likely will not be updated much if at all.

## Usage
Make sure you have Rust installed and `cargo` available on your system.
```
cargo run -- <command> [args]
```

Usage can be printed by just running
```
cargo run
```

Tests (including end-to-end tests) can be run by
```
cargo test
```

## Features
I have implemented the base language and functions from the features shown on course page. This includes:

- basic calculation and comparison operations
- if ... then ... else ...
- while ... do ...
- functions


## Steps
The project consists of the following steps:
1. Tokenizer
2. Parser
3. Interpreter
4. Type checker
5. IR generator
6. Assembly generator
7. End-to-end compilation

My plan is to implement some functionality for each step first. After completing the whole pipeline for a small subset of the language, I will continue adding language features while refactoring and improving the code.

### 1. Tokenizer
Tokenizer preprocesses the code file so that it can be parsed. It uses regular expressions to separate different tokentypes. List of tokens can be be viewed by running `cargo run -- tokens code.txt`.

### 2. Parser
Parser creates the AST (abstract source tree) from tokens. `cargo run -- ast code.txt` can be used to see a debug print of the AST parsed from code.txt.

### 3. Interpreter
The interpreter is quite far behind compared to the compiler, and does not even have the entire base language implemented. Interpreter can be used on a code file by `cargo run -- interpret code.txt`, or if you want to use the interactive interpreter, run `cargo run -- interpret`.

### 4. Type checker
Type checker makes sure that everything is typed properly. It recusively goes through the AST, and assigns types to all nodes. The only types implemented are Int, Bool and None. Typechecked AST can be debug printed by `cargo run -- typecheck code.txt`.

### 5. IR generator
Generates intermediate representation from typechecked AST. `cargo run -- ir code.txt` generates and prints the intermediate representation generated from code found in file code.txt.

### 6. Assembly generator
Generates assembly from intermediate representation. To see the generated assembly, run `cargo run -- asm code.txt`.

### 7. End-to-end compilation
Running `cargo run -- compile code.txt` creates a binary file called `a.out`. If you want to create a binary with a specific name, run `cargo run -- compile code.txt output_filename`. Assembling of the binary is done using the `as` command.

## Known issues
- Compile errors give horribly small amount of information (no location in source code etc.)
- There are cases, where the compiler tries using IR variable, that does not exist (probably problem with function return statements)
