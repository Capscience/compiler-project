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
        } else {
            None
        }
    }

    pub fn set(&mut self, symbol: String, new_value: Value) -> Result<(), &str> {
        if let Some(value) = self.get(&symbol) {
            if discriminant(value) != discriminant(&new_value) {
                return Err("Incompatible types!");
            } else {
                self.symbols.insert(symbol, new_value);
                return Ok(());
            }
        }
        if let Some(parent) = &mut self.parent {
            return parent.set(symbol, new_value);
        }
        Ok(())
    }

    pub fn declare(&mut self, symbol: String) -> Result<(), &str> {
        if let Some(_) = self.get(&symbol) {
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
                if let Some(&ref val) = identifier_value {
                    val.clone()
                } else {
                    return Err("Symbol does not exist!".into());
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
                let Value::Int { value: a } = self.interpret(*left)? else {
                    return Err("No value!".into());
                };
                let Value::Int { value: b } = self.interpret(*right)? else {
                    return Err("No value!".into());
                };
                match operation.as_str() {
                    "+" => Value::Int { value: a + b },
                    "-" => Value::Int { value: a - b },
                    "*" => Value::Int { value: a * b },
                    "/" => Value::Int { value: a / b },
                    "%" => Value::Int { value: a % b },
                    "<" => Value::Bool { value: a < b },
                    ">" => Value::Bool { value: a > b },
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
}

#[cfg(test)]
mod tests {
    use super::*;

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
