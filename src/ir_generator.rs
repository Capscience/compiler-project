use std::collections::HashMap;

use crate::{
    ast::{Expr, ExprKind, Type},
    ir::{IRVar, Instruction},
    variable::SymbolTable,
};

pub struct IRGenerator {
    var_number: i32,
    var_types: HashMap<String, Type>,
    var_none: String,
    instructions: Vec<Instruction>,
}

impl IRGenerator {
    pub fn new(mut var_types: HashMap<String, Type>) -> Self {
        let var_none = "none".to_string();
        var_types.insert(var_none.clone(), Type::None);
        Self {
            var_number: 1,
            var_types,
            var_none,
            instructions: Vec::new(),
        }
    }

    pub fn new_var(&mut self, t: Type) -> IRVar {
        let name = format!("x{}", self.var_number);
        self.var_number += 1;
        self.var_types.insert(name.clone(), t);
        name
    }

    pub fn visit(&mut self, symbol_table: &mut SymbolTable<IRVar>, expr: Expr) -> IRVar {
        let mut var = String::new();
        match expr.content {
            ExprKind::Literal { value } => match expr.type_ {
                Type::Int => {
                    var = self.new_var(Type::Int);
                    self.instructions.push(Instruction::LoadIntConst {
                        value: value.parse::<i64>().expect("Checked by typechecker."),
                        dest: var.clone(),
                    });
                }
                Type::Bool => {
                    var = self.new_var(Type::Bool);
                    self.instructions.push(Instruction::LoadBoolConst {
                        value: value.parse::<bool>().expect("Checked by typechecker."),
                        dest: var.clone(),
                    });
                }
                Type::None => {
                    var = self.var_none.clone();
                }
            },
            ExprKind::Identifier { value } => {
                var = symbol_table
                    .get(&value)
                    .expect("Type checker should have caught this missing value")
                    .to_string();
            }
            _ => todo!(),
        };
        var
    }
}

pub fn generate_ir(root_types: HashMap<String, Type>, exprs: Vec<Expr>) -> Vec<Instruction> {
    let mut generator = IRGenerator::new(root_types.clone());

    let mut root_symtab: SymbolTable<String> = SymbolTable::new(None);
    for key in root_types.keys() {
        root_symtab.declare(key.to_string()).unwrap();
        root_symtab.set(key.to_string(), key.to_string()).unwrap();
    }
    for expr in exprs {
        generator.visit(&mut root_symtab, expr);
    }
    generator.instructions
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_ir_literals() {
        let instructions = generate_ir(
            HashMap::new(),
            vec![
                Expr {
                    type_: Type::Int,
                    content: ExprKind::Literal { value: "1".into() },
                },
                Expr {
                    type_: Type::Bool,
                    content: ExprKind::Literal {
                        value: "true".into(),
                    },
                },
            ],
        );

        assert_eq!(
            instructions[0].to_string(),
            "LoadIntConst(1, x1)".to_string()
        );
        assert_eq!(
            instructions[1].to_string(),
            "LoadBoolConst(true, x2)".to_string()
        );
    }
}
