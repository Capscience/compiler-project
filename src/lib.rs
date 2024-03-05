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

use std::collections::HashMap;
use std::path::Path;

use assembler::assemble;
use assembly_generator::generate_assembly;
use ir_generator::generate_ir;
use parser::parse;
use tokenizer::tokenize;
use type_checker::TypeChecker;

pub fn compile(code: String, output: &Path) {
    let mut typechecker = TypeChecker::new();
    let ast_result = parse(&tokenize(&code));
    if let Err(error) = ast_result {
        println!("Parsing error: {error}");
        std::process::exit(1);
    }
    let mut ast = ast_result.expect("Handled above.");
    for mut node in &mut ast {
        if let Err(error) = typechecker.typecheck(&mut node) {
            println!("Typecheck error: {error}");
            std::process::exit(1);
        }
    }
    let instructions = generate_ir(HashMap::new(), ast);
    let assembly = generate_assembly(&instructions);
    assemble(assembly, output)
}
