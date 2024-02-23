use std::collections::HashMap;

use crate::{
    ast::{Expr, Type},
    ir::{IRVar, Instruction},
    variable::SymbolTable,
};

pub fn generate_ir(root_types: HashMap<String, Type>, exprs: Vec<Expr>) {
    let mut var_types = root_types.clone();
    let mut var_number = 1;
    let new_var = |t: Type| {
        let name = format!("x{var_number}");
        var_number += 1;
        var_types.insert(name.clone(), t);
        name
    };

    let mut instructions: Vec<Instruction> = Vec::new();
}
