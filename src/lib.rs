/// Abstract source tree structs.
pub mod assembler;
pub mod assembly_generator;
pub mod ast;
pub mod interpreter;
pub mod ir;
pub mod ir_generator;
pub mod parser;
pub mod token;
pub mod tokenizer;
pub mod type_checker;
pub mod variable;

use std::path::Path;

use tokenizer::tokenize;

/// Incomplete function handling the complete compilation process.
pub fn compile(source_code: &'static str, _output: &Path) {
    let tokens = tokenize(source_code);
    if cfg!(debug_assertions) {
        for token in tokens {
            print!("{}, ", token);
        }
    }
}
