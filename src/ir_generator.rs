use core::panic;
use std::collections::HashMap;

use crate::{
    ast::{Expr, ExprKind, Module, Type},
    ir::Instruction,
    variable::SymbolTable,
};

pub struct IRGenerator {
    var_number: i32,
    label_number: i32,
    var_types: HashMap<String, Type>,
    var_none: String,
    functions: HashMap<String, Vec<Instruction>>,
    pub current_func: String,
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
            functions: HashMap::new(),
            current_func: String::new(),
        }
    }

    fn new_var(&mut self, t: Type) -> String {
        let name = format!("x{}", self.var_number);
        self.var_number += 1;
        self.var_types.insert(name.clone(), t);
        name
    }

    fn new_label(&mut self) -> String {
        let name = format!("L{}", self.label_number);
        self.label_number += 1;
        name
    }

    fn emit(&mut self, instruction: Instruction) {
        let instructions = self.functions.get_mut(&self.current_func);
        if let Some(instructions) = instructions {
            instructions.push(instruction);
        } else {
            panic!(
                "Tried pushing instructions to nonexistent function {}",
                self.current_func
            );
        }
    }

    pub fn visit(&mut self, symbol_table: &mut SymbolTable<String>, expr: &Expr) -> String {
        let mut var = String::new();
        match &expr.content {
            ExprKind::Return { .. } => todo!(),
            ExprKind::FunDef { .. } => todo!(),
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
                Type::Func { .. } => todo!(),
            },
            ExprKind::Identifier { value } => {
                var = symbol_table
                    .get(value)
                    .expect("Type checker should have caught this missing value")
                    .to_string();
            }
            ExprKind::BinaryOperation {
                left,
                operation,
                right,
            } => {
                let var_left = match &left.content {
                    ExprKind::VarDeclaration { identifier, .. } => {
                        let _ = symbol_table
                            .declare(identifier.clone(), self.new_var(expr.type_.clone()));
                        symbol_table
                            .get(identifier)
                            .expect("This should never happen.")
                            .to_string()
                    }
                    _ => self.visit(symbol_table, left),
                };
                match operation.as_str() {
                    "or" => {
                        let or_right_label = self.new_label();
                        let skip_or_label = self.new_label();
                        let end_or_label = self.new_label();
                        let var_result = self.new_var(Type::Bool);

                        // Skip if left is true instruction
                        self.emit(Instruction::CondJump {
                            cond: var_left,
                            then_label: skip_or_label.clone(),
                            else_label: or_right_label.clone(),
                        });

                        // Left false, return value of right
                        self.emit(Instruction::Label {
                            name: or_right_label,
                        });
                        let var_right = self.visit(symbol_table, right);
                        self.emit(Instruction::Copy {
                            source: var_right,
                            dest: var_result.clone(),
                        });
                        self.emit(Instruction::Jump {
                            label: end_or_label.clone(),
                        });

                        // Skip or, aka return true
                        self.emit(Instruction::Label {
                            name: skip_or_label,
                        });
                        self.emit(Instruction::LoadBoolConst {
                            value: true,
                            dest: var_result.clone(),
                        });
                        // Optional jump to end?
                        self.emit(Instruction::Jump {
                            label: end_or_label.clone(),
                        });

                        // End of or
                        self.emit(Instruction::Label { name: end_or_label });
                        var = var_result;
                    }
                    "and" => {
                        let and_right_label = self.new_label();
                        let skip_and_label = self.new_label();
                        let end_and_label = self.new_label();
                        let var_result = self.new_var(Type::Bool);

                        // Skip if left is false instruction
                        self.emit(Instruction::CondJump {
                            cond: var_left,
                            then_label: and_right_label.clone(),
                            else_label: skip_and_label.clone(),
                        });

                        // Left true, return value of right
                        self.emit(Instruction::Label {
                            name: and_right_label,
                        });
                        let var_right = self.visit(symbol_table, right);
                        self.emit(Instruction::Copy {
                            source: var_right,
                            dest: var_result.clone(),
                        });
                        self.emit(Instruction::Jump {
                            label: end_and_label.clone(),
                        });

                        // Skip and, aka return false
                        self.emit(Instruction::Label {
                            name: skip_and_label,
                        });
                        self.emit(Instruction::LoadBoolConst {
                            value: false,
                            dest: var_result.clone(),
                        });
                        // Optional jump to end?
                        self.emit(Instruction::Jump {
                            label: end_and_label.clone(),
                        });

                        // End of and
                        self.emit(Instruction::Label {
                            name: end_and_label,
                        });
                        var = var_result;
                    }
                    "=" => {
                        let var_right = self.visit(symbol_table, right);
                        self.emit(Instruction::Copy {
                            source: var_right.clone(),
                            dest: var_left.clone(),
                        });
                        var = var_left;
                    }
                    _ => {
                        let var_right = self.visit(symbol_table, right);
                        let var_result = self.new_var(expr.type_.clone());
                        self.emit(Instruction::Call {
                            fun: operation.to_string(),
                            args: vec![var_left, var_right],
                            dest: var_result.clone(),
                        });
                        var = var_result;
                    }
                }
            }
            ExprKind::VarDeclaration { .. } => {} // Handled in BinOp =
            ExprKind::IfClause {
                condition,
                if_block,
                else_block,
            } => {
                if let Some(else_block) = else_block {
                    let l_then = self.new_label();
                    let l_else = self.new_label();
                    let l_end = self.new_label();

                    let var_cond = self.visit(symbol_table, condition);

                    var = self.new_var(expr.type_.clone());
                    self.emit(Instruction::CondJump {
                        cond: var_cond,
                        then_label: l_then.clone(),
                        else_label: l_else.clone(),
                    });
                    self.emit(Instruction::Label { name: l_then });
                    let then_return_var = self.visit(symbol_table, if_block);
                    self.emit(Instruction::Copy {
                        source: then_return_var,
                        dest: var.clone(),
                    });
                    self.emit(Instruction::Jump {
                        label: l_end.clone(),
                    });
                    self.emit(Instruction::Label { name: l_else });
                    let else_return_var = self.visit(symbol_table, else_block);
                    self.emit(Instruction::Copy {
                        source: else_return_var,
                        dest: var.clone(),
                    });

                    self.emit(Instruction::Label { name: l_end });
                } else {
                    let l_then = self.new_label();
                    let l_end = self.new_label();

                    let var_cond = self.visit(symbol_table, condition);

                    self.emit(Instruction::CondJump {
                        cond: var_cond,
                        then_label: l_then.clone(),
                        else_label: l_end.clone(),
                    });
                    self.emit(Instruction::Label { name: l_then });
                    self.visit(symbol_table, if_block);
                    self.emit(Instruction::Label { name: l_end });
                    var = self.var_none.clone();
                }
            }
            ExprKind::Block { expressions } => {
                for expr in expressions {
                    var = self.visit(symbol_table, expr);
                }
            }
            ExprKind::Unary { operator, target } => {
                let var_target = self.visit(symbol_table, target);
                let var_result = self.new_var(expr.type_.clone());
                self.emit(Instruction::Call {
                    fun: format!("unary_{}", operator),
                    args: vec![var_target],
                    dest: var_result.clone(),
                });
                var = var_result;
            }
            ExprKind::WhileDo {
                condition,
                do_block,
            } => {
                let l_while = self.new_label();
                let l_do = self.new_label();
                let l_end = self.new_label();

                self.emit(Instruction::Label {
                    name: l_while.clone(),
                });

                let var_cond = self.visit(symbol_table, condition);

                self.emit(Instruction::CondJump {
                    cond: var_cond,
                    then_label: l_do.clone(),
                    else_label: l_end.clone(),
                });
                self.emit(Instruction::Label { name: l_do });
                // Do-block return value is never used, so ignore it
                let _ = self.visit(symbol_table, do_block);
                self.emit(Instruction::Jump { label: l_while });
                self.emit(Instruction::Label { name: l_end });

                var = self.var_none.clone();
            }
            ExprKind::Call { func, params } => {
                let ret_type = if let Type::Func {
                    params: _,
                    ret_type,
                } = expr.type_.clone()
                {
                    *ret_type
                } else {
                    expr.type_.clone()
                };
                var = self.new_var(ret_type);
                let mut args = Vec::new();
                for param in params {
                    args.push(self.visit(symbol_table, param));
                }
                self.emit(Instruction::Call {
                    fun: func.to_string(),
                    args,
                    dest: var.clone(),
                });
            }
            ExprKind::None => var = self.var_none.clone(),
        };
        var
    }
}

pub fn generate_ir(
    root_types: HashMap<String, Type>,
    module: Module,
) -> HashMap<String, Vec<Instruction>> {
    let mut generator = IRGenerator::new(root_types.clone());

    let mut root_symtab: SymbolTable<String> = SymbolTable::new(None);
    for key in root_types.keys() {
        root_symtab
            .declare(key.to_string(), key.to_string())
            .unwrap();
    }
    // generator.emit(Instruction::Label {
    //     name: "Start".into(),
    // });
    generator.current_func = "main".to_string();
    generator
        .functions
        .insert(generator.current_func.clone(), Vec::new());
    let final_result = generator.visit(
        &mut root_symtab,
        &module
            .main
            .expect("Typechecking fails if there is no main."),
    );
    let final_type = generator
        .var_types
        .get(&final_result)
        .expect("The final return variable does not exist!")
        .clone();
    let return_var = generator.new_var(Type::None);
    if let Some(final_ins) = generate_final(final_result, final_type, return_var) {
        generator.emit(final_ins);
    }
    generator.emit(Instruction::Return);
    generator.functions
}

fn generate_final(
    final_result: String,
    final_type: Type,
    return_var: String,
) -> Option<Instruction> {
    match final_type {
        Type::Int => Some(Instruction::Call {
            fun: "print_int".into(),
            args: vec![final_result],
            dest: return_var,
        }),
        Type::Bool => Some(Instruction::Call {
            fun: "print_bool".into(),
            args: vec![final_result],
            dest: return_var,
        }),
        Type::Func { ret_type, .. } => generate_final(final_result, *ret_type, return_var),
        Type::None => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_ir_literals() {
        let functions = generate_ir(
            HashMap::new(),
            Module {
                main: Some(
                    ExprKind::Block {
                        expressions: vec![
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
                    }
                    .into(),
                ),
                functions: Vec::new(),
            },
        );

        let instructions = functions.get("main").expect("No main function found!");

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
