use crate::ast::Expression;
use std::fmt::{self, Display};
use std::{collections::HashMap, error::Error, mem::discriminant};

#[derive(Default)]
struct SymbolTable {
    symbols: HashMap<String, Value>,
    parent: Option<Box<SymbolTable>>,
}

impl SymbolTable {
    pub fn new(parent: Option<Box<SymbolTable>>) -> SymbolTable {
        let symbols = HashMap::new();
        Self { symbols, parent }
    }

    pub fn get(&self, symbol: &String) -> Option<&Value> {
        if let Some(value) = self.symbols.get(symbol) {
            Some(value)
        } else if let Some(parent) = &self.parent {
            parent.get(symbol)
        } else {
            None
        }
    }

    pub fn set(&mut self, symbol: String, new_value: Value) -> Result<(), &str> {
        if let Some(value) = self.symbols.get(&symbol) {
            if value != &Value::None && discriminant(value) != discriminant(&new_value) {
                return Err("Incompatible types!");
            }
            self.symbols.insert(symbol, new_value);
            return Ok(());
        }
        if let Some(parent) = &mut self.parent {
            return parent.set(symbol, new_value);
        }
        Ok(())
    }

    pub fn declare(&mut self, symbol: String) -> Result<(), &str> {
        if self.get(&symbol).is_some() {
            Err("Variable already exists!")
        } else {
            self.symbols.insert(symbol, Value::None);
            Ok(())
        }
    }
}

#[derive(PartialEq, Debug, Copy, Clone)]
pub enum Value {
    Int { value: i64 },
    Bool { value: bool },
    None,
}

impl Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::Int { value } => write!(f, "{}", value),
            Value::Bool { value } => write!(f, "{}", value),
            Value::None => write!(f, "None"),
        }
    }
}

pub struct Interpreter {
    symbol_table: SymbolTable,
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

    pub fn interpret(&mut self, node: Expression) -> Result<Value, Box<dyn Error>> {
        let value = match node {
            Expression::Literal { value } => {
                if let Ok(val) = value.parse::<bool>() {
                    Value::Bool { value: val }
                } else if let Ok(val) = value.parse::<i64>() {
                    Value::Int { value: val }
                } else {
                    return Err(format!("Invalid literal '{value}'").into());
                }
            }
            Expression::Identifier { value } => {
                let identifier_value = self.symbol_table.get(&value);
                if let Some(val) = identifier_value {
                    *val
                } else {
                    return Err(format!("Use of undeclared variable '{}'", value).into());
                }
            }
            Expression::IfClause {
                condition,
                if_block,
                else_block,
            } => {
                if self.interpret(*condition)? == (Value::Bool { value: true }) {
                    self.interpret(*if_block)?
                } else if let Some(else_block) = else_block {
                    self.interpret(*else_block)?
                } else {
                    Value::None
                }
            }
            Expression::BinaryOperation {
                left,
                operation,
                right,
            } => {
                let left_expr = *left;
                let variable_name = match left_expr {
                    Expression::Identifier { ref value } => value.to_string(),
                    Expression::VarDeclaration { ref identifier } => identifier.to_string(),
                    _ => String::new(),
                };
                let left = self.interpret(left_expr)?;
                let right = self.interpret(*right)?;
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
                    "=" => {
                        if variable_name.is_empty() {
                            return Err("Invalid assignment!".into());
                        }
                        self.symbol_table.set(variable_name.clone(), right)?;
                        Value::None
                    }
                    _ => return Err("Invalid binary operator".into()),
                }
            }
            Expression::Block { expressions } => {
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
            Expression::VarDeclaration { identifier } => {
                self.symbol_table.declare(identifier)?;
                Value::None
            }
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
        let a = interpreter.interpret(Expression::BinaryOperation {
            left: Box::new(Expression::VarDeclaration {
                identifier: "a".to_string(),
            }),
            operation: "=".to_string(),
            right: Box::new(Expression::Literal {
                value: "10".to_string(),
            }),
        });
        dbg!(&a);
        assert!(a.is_ok());
        assert_eq!(a.unwrap(), Value::None);
        assert_eq!(
            interpreter.get_variable(&String::from("a")),
            Some(&Value::Int { value: 10 })
        );
    }

    #[test]
    fn test_literal() {
        let mut interpreter = Interpreter::new();
        let a = interpreter.interpret(Expression::Literal {
            value: "15".to_string(),
        });
        assert!(a.is_ok());
        assert_eq!(a.unwrap(), Value::Int { value: 15 });
    }

    #[test]
    fn test_bin_op() {
        let mut interpreter = Interpreter::new();
        let a = interpreter.interpret(Expression::BinaryOperation {
            left: Box::new(Expression::Literal {
                value: "1".to_string(),
            }),
            operation: "+".to_string(),
            right: Box::new(Expression::Literal {
                value: "1".to_string(),
            }),
        });
        assert!(a.is_ok());
        assert_eq!(a.unwrap(), Value::Int { value: 2 });

        let b = interpreter.interpret(Expression::BinaryOperation {
            left: Box::new(Expression::Literal {
                value: "1".to_string(),
            }),
            operation: "-".to_string(),
            right: Box::new(Expression::Literal {
                value: "1".to_string(),
            }),
        });
        assert!(b.is_ok());
        assert_eq!(b.unwrap(), Value::Int { value: 0 });
    }

    #[test]
    fn test_if() {
        let mut interpreter = Interpreter::new();
        let a = interpreter.interpret(Expression::IfClause {
            condition: Box::new(Expression::BinaryOperation {
                left: Box::new(Expression::Literal {
                    value: "1".to_string(),
                }),
                operation: "<".to_string(),
                right: Box::new(Expression::Literal {
                    value: "2".to_string(),
                }),
            }),
            if_block: Box::new(Expression::Literal {
                value: "1".to_string(),
            }),
            else_block: None,
        });
        dbg!(&a);
        assert!(a.is_ok());
        assert_eq!(a.unwrap(), Value::Int { value: 1 });

        let b = interpreter.interpret(Expression::IfClause {
            condition: Box::new(Expression::BinaryOperation {
                left: Box::new(Expression::Literal {
                    value: "1".to_string(),
                }),
                operation: ">".to_string(),
                right: Box::new(Expression::Literal {
                    value: "2".to_string(),
                }),
            }),
            if_block: Box::new(Expression::Literal {
                value: "1".to_string(),
            }),
            else_block: None,
        });
        assert!(b.is_ok());
        assert_eq!(b.unwrap(), Value::None);

        let c = interpreter.interpret(Expression::IfClause {
            condition: Box::new(Expression::BinaryOperation {
                left: Box::new(Expression::Literal {
                    value: "1".to_string(),
                }),
                operation: ">".to_string(),
                right: Box::new(Expression::Literal {
                    value: "2".to_string(),
                }),
            }),
            if_block: Box::new(Expression::Literal {
                value: "1".to_string(),
            }),
            else_block: Some(Box::new(Expression::Literal {
                value: "2".to_string(),
            })),
        });
        assert!(c.is_ok());
        assert_eq!(c.unwrap(), Value::Int { value: 2 });
    }
}
