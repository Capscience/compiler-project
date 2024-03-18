use std::collections::HashMap;

use crate::ir::Instruction;

const PARAM_REGISTERS: [&str; 6] = ["%rdi", "%rsi", "%rdx", "%rcx", "%r8", "%r9"];

struct AssemblyGenerator {
    lines: Vec<String>,
    functions: HashMap<String, Vec<Instruction>>,
}

impl AssemblyGenerator {
    pub fn new(functions: HashMap<String, Vec<Instruction>>) -> Self {
        let mut lines = Vec::new();
        for func in functions.keys() {
            lines.push(format!("    .global {func}"));
            lines.push(format!("    .type {func}, @function"));
        }

        lines.push("    .section .text".to_string());
        Self { lines, functions }
    }

    pub fn generate_all(&mut self) {
        for (fun, instructions) in self.functions.clone() {
            self.generate_function(fun.to_string(), &instructions);
        }
    }

    pub fn emit(&mut self, line: String) {
        self.lines.push(line);
    }

    pub fn output(&mut self) -> String {
        let mut asm = self.lines.join("\n");
        asm.push('\n');
        asm
    }

    pub fn generate_function(&mut self, name: String, instructions: &[Instruction]) {
        let locals = Locals::new(get_all_variables(instructions));
        self.emit(format!("{name}:"));
        self.emit("    pushq %rbp".to_string());
        self.emit("    movq %rsp, %rbp".to_string());
        self.emit(format!("    subq ${}, %rsp", locals.stack_used()));

        for instruction in instructions {
            self.emit(format!("\n# {}", instruction.to_string()));
            match instruction {
                Instruction::FunParams { params } => {
                    for (i, param) in params.iter().enumerate() {
                        self.emit(format!(
                            "movq {}, {}",
                            PARAM_REGISTERS[i],
                            locals
                                .get_ref(param.to_string())
                                .expect("IR variable does not exist!")
                        ));
                    }
                }
                Instruction::Return { variable } => {
                    self.emit(format!(
                        "movq {}, %rax",
                        locals
                            .get_ref(variable.to_string())
                            .expect("IR variable does not exist!")
                    ));
                    self.emit(format!("jmp .L{name}"));
                }
                Instruction::Label { name } => self.emit(format!(".{name}:")),
                Instruction::Jump { label } => self.emit(format!("jmp .{label}")),
                Instruction::LoadIntConst { value, dest } => {
                    if -(2_i64.pow(31)) <= *value && *value < 2_i64.pow(31) {
                        self.emit(format!(
                            "movq ${value}, {}",
                            locals
                                .get_ref(dest.to_string())
                                .expect("IR variable does not exist!")
                        ));
                    } else {
                        self.emit(format!("movabsq ${value}, %rax"));
                        self.emit(format!(
                            "movq %rax, {}",
                            locals
                                .get_ref(dest.to_string())
                                .expect("IR variable does not exist!")
                        ))
                    }
                }
                Instruction::LoadBoolConst { value, dest } => {
                    let value = if *value { 1 } else { 0 };
                    self.emit(format!(
                        "movq ${value}, {}",
                        locals
                            .get_ref(dest.to_string())
                            .expect("IR variable does not exist!")
                    ))
                }
                Instruction::CondJump {
                    cond,
                    then_label,
                    else_label,
                } => {
                    self.emit(format!(
                        "cmpq $0, {}",
                        locals
                            .get_ref(cond.to_string())
                            .expect("IR variable does not exist!")
                    ));
                    self.emit(format!("jne .{then_label}"));
                    self.emit(format!("jmp .{else_label}"));
                }
                Instruction::Copy { source, dest } => {
                    let src_ref = locals
                        .get_ref(source.to_string())
                        .expect("IR variable does not exist!");
                    let dest_ref = locals
                        .get_ref(dest.to_string())
                        .expect("IR variable does not exist!");
                    self.emit(format!("movq {src_ref}, %rax"));
                    self.emit(format!("movq %rax, {dest_ref}"));
                }
                Instruction::Call { fun, args, dest } => {
                    let arg_refs = args
                        .iter()
                        .map(|arg| {
                            locals
                                .get_ref(arg.to_string())
                                .expect("IR variable does not exist!")
                                .to_string()
                        })
                        .collect();
                    self.emit(generate_call(
                        fun.to_string(),
                        arg_refs,
                        locals
                            .get_ref(dest.to_string())
                            .expect("IR variable does not exist!")
                            .to_string(),
                    ));
                }
            }
        }
        self.emit(format!(".L{}:", name));
        self.emit("movq %rbp, %rsp".to_string());
        self.emit("popq %rbp".to_string());
        self.emit("ret".to_string());
    }
}

struct Locals {
    var_to_location: HashMap<String, String>,
    stack_used: i32,
}

impl Locals {
    pub fn new(variables: Vec<String>) -> Self {
        let mut var_to_location = HashMap::new();
        let mut stack_used = 0;
        for var in variables {
            stack_used += 8;
            var_to_location.insert(var, format!("-{stack_used}(%rbp)"));
        }
        Self {
            var_to_location,
            stack_used,
        }
    }

    pub fn get_ref(&self, var: String) -> Option<&String> {
        self.var_to_location.get(&var)
    }

    pub fn stack_used(&self) -> i32 {
        self.stack_used
    }
}

pub fn generate_assembly(functions: HashMap<String, Vec<Instruction>>) -> String {
    let mut generator = AssemblyGenerator::new(functions);
    generator.generate_all();
    generator.output()
}

fn generate_call(fun: String, arg_refs: Vec<String>, dest_ref: String) -> String {
    let mut lines = Vec::new();
    let mut emit = |line: String| {
        lines.push(line);
    };
    let mut emit_comparison = |setcc_instruction: &str| {
        emit("xor %rax, %rax".to_string());
        emit(format!("movq {}, %rdx", arg_refs[0]));
        emit(format!("cmpq {}, %rdx", arg_refs[1]));
        emit(format!("{} %al", setcc_instruction));
        emit(format!("movq %rax, {}", dest_ref));
    };

    match fun.as_str() {
        "unary_-" => {
            emit(format!("movq {}, %rax", arg_refs[0]));
            emit("negq %rax".to_string());
            emit(format!("movq %rax, {}", dest_ref));
        }
        "unary_not" => {
            emit(format!("movq {}, %rax", arg_refs[0]));
            emit("xorq $1, %rax".to_string());
            emit(format!("movq %rax, {}", dest_ref));
        }
        "+" => {
            emit(format!("movq {}, %rax", arg_refs[0]));
            emit(format!("addq {}, %rax", arg_refs[1]));
            emit(format!("movq %rax, {}", dest_ref));
        }
        "-" => {
            emit(format!("movq {}, %rax", arg_refs[0]));
            emit(format!("subq {}, %rax", arg_refs[1]));
            emit(format!("movq %rax, {}", dest_ref));
        }
        "*" => {
            emit(format!("movq {}, %rax", arg_refs[0]));
            emit(format!("imulq {}, %rax", arg_refs[1]));
            emit(format!("movq %rax, {}", dest_ref));
        }
        "/" => {
            emit(format!("movq {}, %rax", arg_refs[0]));
            emit("cqto".to_string());
            emit(format!("idivq {}", arg_refs[1]));
            emit(format!("movq %rax, {}", dest_ref));
        }
        "%" => {
            emit(format!("movq {}, %rax", arg_refs[0]));
            emit("cqto".to_string());
            emit(format!("idivq {}", arg_refs[1]));
            emit(format!("movq %rdx, {}", dest_ref));
        }
        "==" => emit_comparison("sete"),
        "!=" => emit_comparison("setne"),
        "<" => emit_comparison("setl"),
        ">" => emit_comparison("setg"),
        "<=" => emit_comparison("setle"),
        ">=" => emit_comparison("setge"),
        _ => {
            for (i, arg_ref) in arg_refs.iter().enumerate() {
                emit(format!("movq {}, {}", arg_ref, PARAM_REGISTERS[i]))
            }
            emit(format!("callq {fun}"));
            emit(format!("movq %rax, {dest_ref}"));
        }
    }
    lines.join("\n")
}

fn get_all_variables(instructions: &[Instruction]) -> Vec<String> {
    let mut vars = Vec::new();
    for instruction in instructions {
        vars.extend(instruction.get_vars());
    }
    vars.sort_unstable();
    vars.dedup();
    vars
}
