use crate::ast::{Expr, ExprKind};
use crate::variable::{SymbolTable, Value};
use std::error::Error;

pub struct Interpreter {
    symbol_table: SymbolTable<Value>,
}

impl Default for Interpreter {
    fn default() -> Self {
        Self::new()
    }
}

impl Interpreter {
    pub fn new() -> Self {
        Interpreter {
            symbol_table: SymbolTable::new(None),
        }
    }

    pub fn interpret(&mut self, node: &Expr) -> Result<Value, Box<dyn Error>> {
        let value = match &node.content {
            ExprKind::Return { .. } => todo!(),
            ExprKind::FunDef { .. } => todo!(),
            ExprKind::Literal { value } => {
                if let Ok(val) = value.parse::<bool>() {
                    Value::Bool { value: val }
                } else if let Ok(val) = value.parse::<i64>() {
                    Value::Int { value: val }
                } else {
                    return Err(format!("Invalid literal '{value}'").into());
                }
            }
            ExprKind::Identifier { value } => {
                let identifier_value = self.symbol_table.get(value);
                if let Some(val) = identifier_value {
                    *val
                } else {
                    return Err(format!("Use of undeclared variable '{}'", value).into());
                }
            }
            ExprKind::IfClause {
                condition,
                if_block,
                else_block,
            } => {
                if self.interpret(condition)? == (Value::Bool { value: true }) {
                    self.interpret(if_block)?
                } else if let Some(else_block) = else_block {
                    self.interpret(else_block)?
                } else {
                    Value::None
                }
            }
            ExprKind::BinaryOperation {
                left,
                operation,
                right,
            } => {
                let left_expr = left;
                let left = self.interpret(left_expr)?;
                let right = self.interpret(right)?;
                let a = if let Value::Int { value } = left {
                    Some(value)
                } else {
                    None
                };
                let b = if let Value::Int { value } = right {
                    Some(value)
                } else {
                    None
                };
                match operation.as_str() {
                    "+" => Value::Int {
                        value: a.ok_or("Invalid type!")? + b.ok_or("Invalid type!")?,
                    },
                    "-" => Value::Int {
                        value: a.ok_or("Invalid type!")? - b.ok_or("Invalid type!")?,
                    },
                    "*" => Value::Int {
                        value: a.ok_or("Invalid type!")? * b.ok_or("Invalid type!")?,
                    },
                    "/" => Value::Int {
                        value: a.ok_or("Invalid type!")? / b.ok_or("Invalid type!")?,
                    },
                    "%" => Value::Int {
                        value: a.ok_or("Invalid type!")? % b.ok_or("Invalid type!")?,
                    },
                    "<" => Value::Bool {
                        value: a.ok_or("Invalid type!")? < b.ok_or("Invalid type!")?,
                    },
                    ">" => Value::Bool {
                        value: a.ok_or("Invalid type!")? > b.ok_or("Invalid type!")?,
                    },
                    ">=" => Value::Bool {
                        value: a.ok_or("Invalid type!")? >= b.ok_or("Invalid type!")?,
                    },
                    "<=" => Value::Bool {
                        value: a.ok_or("Invalid type!")? <= b.ok_or("Invalid type!")?,
                    },
                    "==" => Value::Bool {
                        value: a.ok_or("Invalid type!")? == b.ok_or("Invalid type!")?,
                    },
                    "!=" => Value::Bool {
                        value: a.ok_or("Invalid type!")? != b.ok_or("Invalid type!")?,
                    },
                    "=" => {
                        let var_name = match &left_expr.content {
                            ExprKind::VarDeclaration {
                                identifier,
                                annotated_type,
                            } => {
                                if let Some(var_type) = annotated_type {
                                    match var_type.as_str() {
                                        "Int" => self.symbol_table.declare(
                                            identifier.to_string(),
                                            Value::Int { value: 0 },
                                        )?,
                                        "Bool" => self.symbol_table.declare(
                                            identifier.to_string(),
                                            Value::Bool { value: false },
                                        )?,
                                        "None" => self
                                            .symbol_table
                                            .declare(identifier.to_string(), Value::None)?,
                                        _ => {
                                            return Err(format!(
                                                "Invalid type annotation `{}`",
                                                var_type
                                            )
                                            .into())
                                        }
                                    };
                                    identifier
                                } else {
                                    let _ = &self
                                        .symbol_table
                                        .declare(identifier.to_string(), right)?;
                                    identifier
                                }
                            }
                            ExprKind::Identifier { value } => value,
                            _ => return Err("Invalid assignment!".into()),
                        };

                        self.symbol_table.set(var_name.clone(), right)?;
                        right
                    }
                    _ => return Err("Invalid binary operator".into()),
                }
            }
            ExprKind::Block { expressions } => {
                self.symbol_table =
                    SymbolTable::new(Some(Box::new(std::mem::take(&mut self.symbol_table))));
                let mut val = Value::None;
                for expression in expressions {
                    val = self.interpret(expression)?;
                }
                if let Some(symbol_table) = &mut self.symbol_table.parent {
                    self.symbol_table = std::mem::take(symbol_table);
                } else {
                    return Err("Symbol table has no parent!".into());
                }
                val
            }
            ExprKind::VarDeclaration { .. } => Value::None, // Declaration handled in BinOp =
            ExprKind::Unary { .. } => todo!(),
            ExprKind::WhileDo { .. } => todo!(),
            ExprKind::Call { .. } => todo!(),
            ExprKind::None => Value::None,
        };

        Ok(value)
    }

    #[cfg(test)]
    pub fn get_variable(&self, symbol: &String) -> Option<&Value> {
        self.symbol_table.get(symbol)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_variable_declaration() {
        let mut interpreter = Interpreter::new();
        let a = interpreter.interpret(
            &ExprKind::BinaryOperation {
                left: ExprKind::VarDeclaration {
                    identifier: "a".to_string(),
                    annotated_type: None,
                }
                .into(),
                operation: "=".to_string(),
                right: ExprKind::Literal {
                    value: "10".to_string(),
                }
                .into(),
            }
            .into(),
        );
        dbg!(&a);
        assert!(a.is_ok());
        assert_eq!(a.unwrap(), Value::Int { value: 10 });
        assert_eq!(
            interpreter.get_variable(&String::from("a")),
            Some(&Value::Int { value: 10 })
        );
    }

    #[test]
    fn test_literal() {
        let mut interpreter = Interpreter::new();
        let a = interpreter.interpret(
            &ExprKind::Literal {
                value: "15".to_string(),
            }
            .into(),
        );
        assert!(a.is_ok());
        assert_eq!(a.unwrap(), Value::Int { value: 15 });
    }

    #[test]
    fn test_bin_op() {
        let mut interpreter = Interpreter::new();
        let a = interpreter.interpret(
            &ExprKind::BinaryOperation {
                left: ExprKind::Literal {
                    value: "1".to_string(),
                }
                .into(),
                operation: "+".to_string(),
                right: ExprKind::Literal {
                    value: "1".to_string(),
                }
                .into(),
            }
            .into(),
        );
        assert!(a.is_ok());
        assert_eq!(a.unwrap(), Value::Int { value: 2 });

        let b = interpreter.interpret(
            &ExprKind::BinaryOperation {
                left: ExprKind::Literal {
                    value: "1".to_string(),
                }
                .into(),
                operation: "-".to_string(),
                right: ExprKind::Literal {
                    value: "1".to_string(),
                }
                .into(),
            }
            .into(),
        );
        assert!(b.is_ok());
        assert_eq!(b.unwrap(), Value::Int { value: 0 });
    }

    #[test]
    fn test_if() {
        let mut interpreter = Interpreter::new();
        let a = interpreter.interpret(
            &ExprKind::IfClause {
                condition: ExprKind::BinaryOperation {
                    left: ExprKind::Literal {
                        value: "1".to_string(),
                    }
                    .into(),
                    operation: "<".to_string(),
                    right: ExprKind::Literal {
                        value: "2".to_string(),
                    }
                    .into(),
                }
                .into(),
                if_block: ExprKind::Literal {
                    value: "1".to_string(),
                }
                .into(),
                else_block: None,
            }
            .into(),
        );
        dbg!(&a);
        assert!(a.is_ok());
        assert_eq!(a.unwrap(), Value::Int { value: 1 });

        let b = interpreter.interpret(
            &ExprKind::IfClause {
                condition: ExprKind::BinaryOperation {
                    left: ExprKind::Literal {
                        value: "1".to_string(),
                    }
                    .into(),
                    operation: ">".to_string(),
                    right: ExprKind::Literal {
                        value: "2".to_string(),
                    }
                    .into(),
                }
                .into(),
                if_block: ExprKind::Literal {
                    value: "1".to_string(),
                }
                .into(),
                else_block: None,
            }
            .into(),
        );
        assert!(b.is_ok());
        assert_eq!(b.unwrap(), Value::None);

        let c = interpreter.interpret(
            &ExprKind::IfClause {
                condition: ExprKind::BinaryOperation {
                    left: ExprKind::Literal {
                        value: "1".to_string(),
                    }
                    .into(),
                    operation: ">".to_string(),
                    right: ExprKind::Literal {
                        value: "2".to_string(),
                    }
                    .into(),
                }
                .into(),
                if_block: ExprKind::Literal {
                    value: "1".to_string(),
                }
                .into(),
                else_block: Some(
                    ExprKind::Literal {
                        value: "2".to_string(),
                    }
                    .into(),
                ),
            }
            .into(),
        );
        assert!(c.is_ok());
        assert_eq!(c.unwrap(), Value::Int { value: 2 });
    }
}
