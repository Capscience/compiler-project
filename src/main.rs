use std::{collections::HashMap, env, fs, io::Write, path::PathBuf};

use compiler_project::{
    assembly_generator::generate_assembly, interpreter::Interpreter, ir_generator::generate_ir,
    parser::parse, tokenizer::tokenize, type_checker::TypeChecker,
};

fn main() {
    let args: Vec<String> = env::args().collect();
    match args.len() {
        2 => match args[1].as_str() {
            "interpret" => interpreter_cli(),
            _ => {
                usage();
                std::process::exit(1);
            }
        },
        3 => match args[1].as_str() {
            "ir" => ir_gen(args[2].clone().into()),
            "asm" => asm_gen(args[2].clone().into()),
            _ => {
                usage();
                std::process::exit(1);
            }
        },
        _ => {
            usage();
            std::process::exit(1);
        }
    }
}

fn interpreter_cli() {
    println!("Limited functionality for now. Exit with Ctlr+C");
    let mut interpreter = Interpreter::new();
    let mut line = String::new();
    loop {
        print!(">>> ");
        let _ = std::io::stdout().flush();
        line.clear();

        if std::io::stdin().read_line(&mut line).is_ok() {
            let tokens = tokenize(&line);
            let ast = parse(&tokens);
            if let Err(error) = ast {
                println!("Error while parsing: {}", error);
            } else {
                for expression in ast.unwrap() {
                    let result = interpreter.interpret(expression);
                    if let Err(error) = result {
                        println!("Error while interpreting: {}", error);
                    } else if let Ok(value) = result {
                        println!("{}", value);
                    }
                }
            }
        };
    }
}

fn ir_gen(filename: PathBuf) {
    let code = fs::read_to_string(filename).expect("File not found!");
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
    let ir = generate_ir(HashMap::new(), ast);
    for line in ir {
        println!("{}", line.to_string());
    }
}

fn asm_gen(filename: PathBuf) {
    let code = fs::read_to_string(filename).expect("File not found!");
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
    for line in assembly.lines() {
        println!("{}", line.to_string());
    }
}

fn usage() {
    println!("Usage:");
    println!("\tcargo run -- <command> [args]\n");
    println!("Commands:");
    println!("\tinterpret");
    println!("\tir <filename>");
}
