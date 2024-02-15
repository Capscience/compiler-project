use std::{env, io::Write};

use compiler_project::{interpreter::Interpreter, parser::parse, tokenizer::tokenize};

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

fn usage() {
    println!("Usage:");
    println!("\tcargo run -- <command> [args]\n");
    println!("Commands:");
    println!("\tinterpret");
}
