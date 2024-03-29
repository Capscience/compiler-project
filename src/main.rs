use std::{env, fs, io::Write, path::PathBuf};

use compiler_project::{
    assembly_generator::generate_assembly,
    interpreter::Interpreter,
    ir::Instruction,
    ir_generator::generate_ir,
    parser::{parse, parse_module},
    tokenizer::tokenize,
    type_checker::TypeChecker,
};

fn main() {
    let args: Vec<String> = env::args().collect();
    match args.len() {
        2 => match args[1].as_str() {
            "interpret" => cli_interactive_interpret(),
            _ => {
                usage();
                std::process::exit(1);
            }
        },
        3 => match args[1].as_str() {
            "interpret" => cli_interpret(args[2].clone().into()),
            "tokens" => cli_tokenize(args[2].clone().into()),
            "ast" => cli_ast(args[2].clone().into()),
            "typecheck" => cli_typecheck(args[2].clone().into()),
            "ir" => ir_gen(args[2].clone().into()),
            "asm" => asm_gen(args[2].clone().into()),
            "compile" => cli_compile(args[2].clone().into(), "a.out".into()),
            _ => {
                usage();
                std::process::exit(1);
            }
        },
        4 => match args[1].as_str() {
            "compile" => cli_compile(args[2].clone().into(), args[3].clone().into()),
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

fn cli_interactive_interpret() {
    println!();
    println!("The interpreter does not support the complete base language!");
    println!();
    println!("Exit with Ctlr+C");
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
                let ast = ast.unwrap();
                let exprs = ast.content.expressions().expect("No expressions found!");
                for expression in exprs {
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

fn cli_interpret(codefile: PathBuf) {
    let mut interpreter = Interpreter::new();
    let code = fs::read_to_string(codefile).expect("Codefile not found!");
    let ast_result = parse_module(&tokenize(&code));
    if let Err(error) = &ast_result {
        eprint!("Parsing failed with error: {error}");
    }
    let module = ast_result.unwrap();
    if module.main.is_none() {
        std::process::exit(1);
    }
    println!(
        "{}",
        interpreter
            .interpret(&module.main.expect("Checked in previous if."))
            .unwrap()
    );
}

fn cli_tokenize(codefile: PathBuf) {
    let code = fs::read_to_string(codefile).expect("Codefile not found!");
    let tokens = tokenize(&code);
    for token in tokens {
        print!("{}, ", token);
    }
    println!();
}

fn cli_ast(codefile: PathBuf) {
    let code = fs::read_to_string(codefile).expect("Codefile not found!");
    let ast_result = parse_module(&tokenize(&code));
    if let Err(error) = &ast_result {
        eprint!("Parsing failed with error: {error}");
    }
    let module = ast_result.unwrap();
    print!("{:#?}", module);
    println!();
}

fn cli_typecheck(codefile: PathBuf) {
    let code = fs::read_to_string(codefile).expect("Codefile not found!");
    let ast_result = parse_module(&tokenize(&code));
    if let Err(error) = &ast_result {
        eprint!("Parsing failed with error: {error}");
    }
    let mut module = ast_result.unwrap();
    let mut typechecker = TypeChecker::new();
    if let Err(error) = typechecker.typecheck_module(&mut module) {
        println!("Typecheck error: {error}");
        std::process::exit(1);
    }
    print!("{:#?}", module);
    println!();
}

fn cli_compile(codefile: PathBuf, binfile: PathBuf) {
    let code = fs::read_to_string(codefile).expect("Codefile not found!");
    if let Err(error) = compiler_project::compile(code, &binfile) {
        eprint!("{}", error);
        std::process::exit(1);
    }
}

fn ir_gen(filename: PathBuf) {
    let code = fs::read_to_string(filename).expect("File not found!");
    let mut typechecker = TypeChecker::new();
    let ast_result = parse_module(&tokenize(&code));
    if let Err(error) = ast_result {
        println!("Parsing error: {error}");
        std::process::exit(1);
    }
    let mut ast = ast_result.expect("Handled above.");
    if let Err(error) = typechecker.typecheck_module(&mut ast) {
        println!("Typecheck error: {error}");
        std::process::exit(1);
    }
    let ir = generate_ir(ast);
    for function in ir.values() {
        for line in function {
            match &line {
                Instruction::Label { .. } => println!("\n{}", line.to_string()),
                _ => println!("{}", line.to_string()),
            }
        }
    }
}

fn asm_gen(filename: PathBuf) {
    let code = fs::read_to_string(filename).expect("File not found!");
    let mut typechecker = TypeChecker::new();
    let ast_result = parse_module(&tokenize(&code));
    if let Err(error) = ast_result {
        println!("Parsing error: {error}");
        std::process::exit(1);
    }
    let mut ast = ast_result.expect("Handled above.");
    if let Err(error) = typechecker.typecheck_module(&mut ast) {
        println!("Typecheck error: {error}");
        std::process::exit(1);
    }
    let instructions = generate_ir(ast);
    let assembly = generate_assembly(instructions);
    for line in assembly.lines() {
        println!("{}", line);
    }
}

fn usage() {
    println!("Usage:");
    println!("\tcargo run -- <command> [args]\n");
    println!("Commands:");
    println!("\ttokens <filename>");
    println!("\tast <filename>");
    println!("\ttypecheck <filename>");
    println!("\tinterpret");
    println!("\tinterpret <filename>");
    println!("\tir <filename>");
    println!("\tasm <filename>");
    println!("\tcompile <filename> [output_filename]");
}
