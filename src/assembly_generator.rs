use std::collections::HashMap;

use crate::ir::Instruction;

const ASM_TEMPLATE: &str = "
    .extern printf
    .global main
    .type main, @function

    .section .text

main:
    pushq %rbp
    movq %rsp, %rbp";

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
    emit(ASM_TEMPLATE);
    emit(format!("    subq ${}, %rsp", locals.stack_used()).as_str());

    lines.join("\n")
}

fn get_all_variables(instructions: &[Instruction]) -> Vec<String> {
    let mut vars = Vec::new();
    for instruction in instructions {
        vars.extend(instruction.get_vars());
    }
    vars.dedup();
    vars
}
