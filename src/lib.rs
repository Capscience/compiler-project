/// Abstract source tree structs.
pub mod ast;
pub mod interpreter;
pub mod parser;
pub mod token;
pub mod tokenizer;

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