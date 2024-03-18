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
use parser::parse_module;
use tokenizer::tokenize;
use type_checker::TypeChecker;

pub fn compile(code: String, output: &Path) -> Result<(), String> {
    let mut typechecker = TypeChecker::new();
    let mut ast = parse_module(&tokenize(&code))?;
    let _ = typechecker.typecheck_module(&mut ast)?;
    let instructions = generate_ir(HashMap::new(), ast);
    let assembly = generate_assembly(instructions);
    assemble(assembly, output);
    Ok(())
}
