use std::env;

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
    println!("WIP");
}

fn usage() {
    println!("Usage:");
    println!("\tcargo run -- <command> [args]\n");
    println!("Commands:");
    println!("\tinterpret");
}
