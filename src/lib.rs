use std::path::Path;

use tokenizer::tokenize;

/// Tokenizer and support struct module.
pub mod tokenizer;

/// Incomplete function handling the complete compilation process.
pub fn compile(source_code: &'static str, _output: &Path) {
    let tokens = tokenize(source_code);
    if cfg!(debug_assertions) {
        for token in tokens {
            print!("{}, ", token);
        }
    }
}
