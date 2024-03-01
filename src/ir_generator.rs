use std::collections::HashMap;

use crate::{
    ast::{Expr, ExprKind, Type},
    ir::{IRVar, Instruction},
    variable::SymbolTable,
};

pub struct IRGenerator {
    var_number: i32,
    label_number: i32,
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
            label_number: 1,
            var_types,
            var_none,
            instructions: Vec::new(),
        }
    }

    fn new_var(&mut self, t: Type) -> IRVar {
        let name = format!("x{}", self.var_number);
        self.var_number += 1;
        self.var_types.insert(name.clone(), t);
        name
    }

    fn new_label(&mut self) -> IRVar {
        let name = format!("L{}", self.label_number);
        self.label_number += 1;
        name
    }

    fn emit(&mut self, instruction: Instruction) {
        self.instructions.push(instruction);
    }

    pub fn visit(&mut self, symbol_table: &mut SymbolTable<IRVar>, expr: Expr) -> IRVar {
        let mut var = String::new();
        match expr.content {
            ExprKind::Literal { value } => match expr.type_ {
                Type::Int => {
                    var = self.new_var(Type::Int);
                    self.emit(Instruction::LoadIntConst {
                        value: value.parse::<i64>().expect("Checked by typechecker."),
                        dest: var.clone(),
                    });
                }
                Type::Bool => {
                    var = self.new_var(Type::Bool);
                    self.emit(Instruction::LoadBoolConst {
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
            ExprKind::BinaryOperation {
                left,
                operation,
                right,
            } => {
                let var_left = self.visit(symbol_table, *left);
                let var_right = self.visit(symbol_table, *right);
                if &operation == "=" {
                    self.emit(Instruction::Copy {
                        source: var_right.clone(),
                        dest: var_left.clone(),
                    });
                    var = var_left;
                } else {
                    let var_result = self.new_var(expr.type_.clone());
                    self.emit(Instruction::Call {
                        fun: operation,
                        args: vec![var_left, var_right],
                        dest: var_result.clone(),
                    });
                    var = var_result;
                }
            }
            ExprKind::VarDeclaration { identifier } => {
                let _ = symbol_table.declare(identifier.clone());
                let _ = symbol_table.set(identifier.clone(), self.new_var(Type::None));
                var = symbol_table
                    .get(&identifier)
                    .expect("This should never happen.")
                    .to_string();
                // TODO: proper error handling
            }
            ExprKind::IfClause {
                condition,
                if_block,
                else_block,
            } => {
                if let Some(else_block) = else_block {
                    let l_then = self.new_label();
                    let l_else = self.new_label();
                    let l_end = self.new_label();

                    let var_cond = self.visit(symbol_table, *condition);

                    var = self.new_var(expr.type_.clone());
                    self.emit(Instruction::CondJump {
                        cond: var_cond,
                        then_label: l_then.clone(),
                        else_label: l_else.clone(),
                    });
                    self.emit(Instruction::Label { name: l_then });
                    let then_return_var = self.visit(symbol_table, *if_block);
                    self.emit(Instruction::Copy {
                        source: then_return_var,
                        dest: var.clone(),
                    });
                    self.emit(Instruction::Jump {
                        label: l_end.clone(),
                    });
                    self.emit(Instruction::Label { name: l_else });
                    let else_return_var = self.visit(symbol_table, *else_block);
                    self.emit(Instruction::Copy {
                        source: else_return_var,
                        dest: var.clone(),
                    });

                    self.emit(Instruction::Label { name: l_end });
                } else {
                    let l_then = self.new_label();
                    let l_end = self.new_label();

                    let var_cond = self.visit(symbol_table, *condition);

                    self.emit(Instruction::CondJump {
                        cond: var_cond,
                        then_label: l_then.clone(),
                        else_label: l_end.clone(),
                    });
                    self.emit(Instruction::Label { name: l_then });
                    self.visit(symbol_table, *if_block);
                    self.emit(Instruction::Label { name: l_end });
                    var = self.var_none.clone();
                }
            } // _ => todo!(),
            ExprKind::Block { expressions } => {
                for expr in expressions {
                    var = self.visit(symbol_table, expr);
                }
            }
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
    generator.emit(Instruction::Label {
        name: "Start".into(),
    });
    let mut final_result = String::new();
    for expr in exprs {
        final_result = generator.visit(&mut root_symtab, expr);
    }
    let final_type = generator
        .var_types
        .get(&final_result)
        .expect("The final return variable does not exist!")
        .clone();
    let return_var = generator.new_var(Type::None);
    match final_type {
        Type::Int => generator.emit(Instruction::Call {
            fun: "print_int".into(),
            args: vec![final_result],
            dest: return_var,
        }),
        Type::Bool => generator.emit(Instruction::Call {
            fun: "print_bool".into(),
            args: vec![final_result],
            dest: return_var,
        }),
        Type::None => {}
    }
    generator.emit(Instruction::Return);
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
