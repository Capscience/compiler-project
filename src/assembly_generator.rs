use std::collections::HashMap;

use crate::ir::Instruction;

const ASM_START: &str = "
    .extern printf
    .global main
    .type main, @function

    .section .text

main:
    pushq %rbp
    movq %rsp, %rbp";

const PARAM_REGISTERS: [&str; 6] = ["%rdi", "%rsi", "%rdx", "%rcx", "%r8", "%r9"];

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

pub fn generate_assembly(instructions: &[Instruction]) -> String {
    let mut lines = Vec::new();
    let mut emit = |line: &str| {
        let _ = lines.push(line.to_string());
    };

    let locals = Locals::new(get_all_variables(instructions));
    emit(ASM_START);
    emit(format!("    subq ${}, %rsp", locals.stack_used()).as_str());

    for instruction in instructions {
        emit(format!("\n# {}", instruction.to_string()).as_str());
        match instruction {
            Instruction::Return => {
                emit("movq $0, %rax");
                emit("movq %rbp, %rsp");
                emit("popq %rbp");
                emit("ret");
            }
            Instruction::Label { name } => emit(format!(".{name}:").as_str()),
            Instruction::Jump { label } => emit(format!("jmp .{label}").as_str()),
            Instruction::LoadIntConst { value, dest } => emit(
                format!(
                    "movq ${value}, {}",
                    locals
                        .get_ref(dest.to_string())
                        .expect("IR variable does not exist!")
                )
                .as_str(),
            ),
            Instruction::LoadBoolConst { value, dest } => {
                let value = if *value { 1 } else { 0 };
                emit(
                    format!(
                        "movq ${value}, {}",
                        locals
                            .get_ref(dest.to_string())
                            .expect("IR variable does not exist!")
                    )
                    .as_str(),
                )
            }
            Instruction::CondJump {
                cond,
                then_label,
                else_label,
            } => {
                emit(
                    format!(
                        "cmpq $0, {}",
                        locals
                            .get_ref(cond.to_string())
                            .expect("IR variable does not exist!")
                    )
                    .as_str(),
                );
                emit(format!("jne .{then_label}").as_str());
                emit(format!("jmp .{else_label}").as_str());
            }
            Instruction::Copy { source, dest } => {
                let src_ref = locals
                    .get_ref(source.to_string())
                    .expect("IR variable does not exist!");
                let dest_ref = locals
                    .get_ref(dest.to_string())
                    .expect("IR variable does not exist!");
                emit(format!("movq {src_ref}, %rax").as_str());
                emit(format!("movq %rax, {dest_ref}").as_str());
            }
            Instruction::Call { fun, args, dest } => {
                for (i, arg) in args.iter().enumerate() {
                    let arg_ref = locals
                        .get_ref(arg.to_string())
                        .expect("IR variable does not exist!");
                    emit(format!("movq {}, {}", arg_ref, PARAM_REGISTERS[i]).as_str())
                }
                let return_ref = locals
                    .get_ref(dest.to_string())
                    .expect("IR variable does not exist!");
                emit(format!("callq {fun}").as_str());
                emit(format!("movq %rax, {return_ref}").as_str());
            }
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
