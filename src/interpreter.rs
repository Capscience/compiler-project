use crate::ast::Expression;
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

enum Value {
    Int { value: i64 },
    Bool { value: bool },
    None,
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

    pub fn interpret(&mut self, node: Expression) -> Result<Option<i64>, Box<dyn Error>> {
        let value = match node {
            Expression::Literal { value } => Some(value.parse::<i64>()?),
            Expression::Identifier { value } => {
                if let Some(Value::Int { value }) = self.symbol_table.get(&value) {
                    Some(*value)
                } else {
                    None
                }
            }
            Expression::IfClause {
                condition,
                if_block,
                else_block,
            } => {
                if self.interpret(*condition)? != Some(0) {
                    self.interpret(*if_block)?
                } else if let Some(else_block) = else_block {
                    self.interpret(*else_block)?
                } else {
                    None
                }
            }
            Expression::BinaryOperation {
                left,
                operation,
                right,
            } => {
                let a = self.interpret(*left)?.expect("Invalid types");
                let b = self.interpret(*right)?.expect("Invalid types");
                match operation.as_str() {
                    "+" => Some(a + b),
                    "-" => Some(a - b),
                    "*" => Some(a * b),
                    "/" => Some(a / b),
                    "%" => Some(a % b),
                    "<" => {
                        if a < b {
                            Some(1)
                        } else {
                            Some(0)
                        }
                    }
                    ">" => {
                        if a > b {
                            Some(1)
                        } else {
                            Some(0)
                        }
                    }
                    _ => return Err("Invalid binary operator".into()),
                }
            }
            Expression::Block { expressions } => {
                self.symbol_table =
                    SymbolTable::new(Some(Box::new(std::mem::take(&mut self.symbol_table))));
                let mut val = None;
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
            Expression::VarDeclaration { identifier: _ } => todo!(),
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
        assert_eq!(a.unwrap(), Some(15));
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
        assert_eq!(a.unwrap(), Some(2));

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
        assert_eq!(b.unwrap(), Some(0));
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
        assert_eq!(a.unwrap(), Some(1));

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
        assert_eq!(b.unwrap(), None);

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
        assert_eq!(c.unwrap(), Some(2));
    }
}
