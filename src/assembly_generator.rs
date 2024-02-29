use std::collections::HashMap;

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
