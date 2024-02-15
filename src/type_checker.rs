use crate::ast::Expression;
use crate::variable::{SymbolTable, Value};
use std::error::Error;
use std::mem::discriminant;

pub struct TypeChecker {
    symbol_table: SymbolTable,
}

impl Default for TypeChecker {
    fn default() -> Self {
        Self::new()
    }
}

impl TypeChecker {
    pub fn new() -> Self {
        TypeChecker {
            symbol_table: SymbolTable::new(None),
        }
    }

    pub fn typecheck(&mut self, node: Expression) -> Result<Value, Box<dyn Error>> {
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
                if !matches!(&self.typecheck(*condition)?, Value::Bool { value: _ }) {
                    return Err("If statement condition must be type `Bool`".into());
                }
                let if_type = self.typecheck(*if_block)?;
                if let Some(else_block) = else_block {
                    if discriminant(&self.typecheck(*else_block)?) != discriminant(&if_type) {
                        return Err("If- and Else-blocks must have same types".into());
                    }
                    if_type
                } else {
                    if if_type != Value::None {
                        return Err("If-block cannot return a value without an Else-block".into());
                    }
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
                let left = self.typecheck(left_expr)?;
                let right = self.typecheck(*right)?;
                match operation.as_str() {
                    "=" => {
                        if variable_name.is_empty() {
                            return Err("Invalid assignment!".into());
                        }
                        // Fails, if types are missmatched
                        self.symbol_table.set(variable_name.clone(), right)?;
                        Value::None
                    }
                    "<" | ">" | "==" | ">=" | "<=" | "!=" => {
                        if !(matches!(&left, Value::Int { value: _ })
                            && matches!(&right, Value::Int { value: _ }))
                        {
                            return Err(
                                "Both operands must be type `Int` when using comparison".into()
                            );
                        }
                        Value::Bool { value: false }
                    }
                    "+" | "-" | "*" | "/" | "%" => {
                        if !(matches!(&left, Value::Int { value: _ })
                            && matches!(&right, Value::Int { value: _ }))
                        {
                            return Err(format!(
                                "Both operands must be type `Int` when using operator {operation}"
                            )
                            .into());
                        }
                        Value::Int { value: 0 }
                    }
                    _ => {
                        if discriminant(&left) != discriminant(&right) {
                            return Err("Missmatched types!".into());
                        }
                        left
                    }
                }
            }
            Expression::Block { expressions } => {
                self.symbol_table =
                    SymbolTable::new(Some(Box::new(std::mem::take(&mut self.symbol_table))));
                let mut val = Value::None;
                for expression in expressions {
                    val = self.typecheck(expression)?;
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
    fn test_typecheck_literals() {
        let mut checker = TypeChecker::new();
        assert!(matches!(
            checker
                .typecheck(Expression::Literal {
                    value: "1".to_string()
                })
                .unwrap(),
            Value::Int { value: _ }
        ));
        assert!(matches!(
            checker
                .typecheck(Expression::Literal {
                    value: "true".to_string()
                })
                .unwrap(),
            Value::Bool { value: _ }
        ));
        assert!(checker
            .typecheck(Expression::Literal {
                value: "should_fail".to_string()
            })
            .is_err());
    }

    #[test]
    fn test_assignment() {
        let mut checker = TypeChecker::new();
        assert!(matches!(
            &checker
                .typecheck(Expression::BinaryOperation {
                    left: Box::new(Expression::VarDeclaration {
                        identifier: "a".to_string()
                    }),
                    operation: "=".to_string(),
                    right: Box::new(Expression::Literal {
                        value: "1".to_string()
                    })
                })
                .unwrap(),
            Value::None
        ));

        assert!(matches!(
            &checker
                .typecheck(Expression::BinaryOperation {
                    left: Box::new(Expression::Identifier {
                        value: "a".to_string()
                    }),
                    operation: "=".to_string(),
                    right: Box::new(Expression::Literal {
                        value: "3".to_string()
                    })
                })
                .unwrap(),
            Value::None
        ));

        assert!(&checker
            .typecheck(Expression::BinaryOperation {
                left: Box::new(Expression::Identifier {
                    value: "a".to_string()
                }),
                operation: "=".to_string(),
                right: Box::new(Expression::Literal {
                    value: "false".to_string()
                })
            })
            .is_err());
    }

    #[test]
    fn test_proper_comparison() {
        let mut checker = TypeChecker::new();
        assert!(matches!(
            &checker
                .typecheck(Expression::BinaryOperation {
                    left: Box::new(Expression::Literal {
                        value: "3".to_string()
                    }),
                    operation: ">".to_string(),
                    right: Box::new(Expression::Literal {
                        value: "1".to_string()
                    })
                })
                .unwrap(),
            Value::Bool { value: _ }
        ));
    }

    #[test]
    fn test_comparing_bools() {
        let mut checker = TypeChecker::new();

        assert!(&checker
            .typecheck(Expression::BinaryOperation {
                left: Box::new(Expression::Literal {
                    value: "true".to_string()
                }),
                operation: "<".to_string(),
                right: Box::new(Expression::Literal {
                    value: "false".to_string()
                })
            })
            .is_err());
    }

    #[test]
    fn test_comparing_bool_and_int() {
        let mut checker = TypeChecker::new();

        assert!(&checker
            .typecheck(Expression::BinaryOperation {
                left: Box::new(Expression::Literal {
                    value: "10".to_string()
                }),
                operation: "==".to_string(),
                right: Box::new(Expression::Literal {
                    value: "false".to_string()
                })
            })
            .is_err());
    }

    #[test]
    fn test_plus_minus_etc() {
        let mut checker = TypeChecker::new();
        assert!(matches!(
            &checker
                .typecheck(Expression::BinaryOperation {
                    left: Box::new(Expression::Literal {
                        value: "3".to_string()
                    }),
                    operation: "+".to_string(),
                    right: Box::new(Expression::Literal {
                        value: "1".to_string()
                    })
                })
                .unwrap(),
            Value::Int { value: _ }
        ));

        assert!(&checker
            .typecheck(Expression::BinaryOperation {
                left: Box::new(Expression::Literal {
                    value: "true".to_string()
                }),
                operation: "-".to_string(),
                right: Box::new(Expression::Literal {
                    value: "false".to_string()
                })
            })
            .is_err());

        assert!(&checker
            .typecheck(Expression::BinaryOperation {
                left: Box::new(Expression::Literal {
                    value: "10".to_string()
                }),
                operation: "*".to_string(),
                right: Box::new(Expression::Literal {
                    value: "false".to_string()
                })
            })
            .is_err());
    }

    #[test]
    fn test_proper_if_then_else() {
        let mut checker = TypeChecker::new();
        assert!(matches!(
            &checker
                .typecheck(Expression::IfClause {
                    condition: Box::new(Expression::Literal {
                        value: "true".to_string()
                    }),
                    if_block: Box::new(Expression::Literal {
                        value: "3".to_string()
                    }),
                    else_block: Some(Box::new(Expression::Literal {
                        value: "1".to_string()
                    }))
                })
                .unwrap(),
            Value::Int { value: _ }
        ));
    }

    #[test]
    fn test_if_with_int_condition() {
        let mut checker = TypeChecker::new();
        assert!(&checker
            .typecheck(Expression::IfClause {
                condition: Box::new(Expression::Literal {
                    value: "1".to_string()
                }),
                if_block: Box::new(Expression::Literal {
                    value: "3".to_string()
                }),
                else_block: Some(Box::new(Expression::Literal {
                    value: "1".to_string()
                }))
            })
            .is_err());
    }

    #[test]
    fn test_if_and_else_different_types() {
        let mut checker = TypeChecker::new();
        assert!(&checker
            .typecheck(Expression::IfClause {
                condition: Box::new(Expression::Literal {
                    value: "true".to_string()
                }),
                if_block: Box::new(Expression::Literal {
                    value: "true".to_string()
                }),
                else_block: Some(Box::new(Expression::Literal {
                    value: "1".to_string()
                }))
            })
            .is_err());
    }

    #[test]
    fn test_proper_if_without_else() {
        let mut checker = TypeChecker::new();
        assert!(matches!(
            &checker
                .typecheck(Expression::IfClause {
                    condition: Box::new(Expression::Literal {
                        value: "true".to_string()
                    }),
                    if_block: Box::new(Expression::BinaryOperation {
                        left: Box::new(Expression::VarDeclaration {
                            identifier: "a".to_string()
                        }),
                        operation: "=".to_string(),
                        right: Box::new(Expression::Literal {
                            value: "true".to_string()
                        })
                    }),
                    else_block: None,
                })
                .unwrap(),
            Value::None
        ));
    }

    #[test]
    fn test_if_with_return_without_else() {
        let mut checker = TypeChecker::new();
        assert!(&checker
            .typecheck(Expression::IfClause {
                condition: Box::new(Expression::Literal {
                    value: "true".to_string()
                }),
                if_block: Box::new(Expression::Literal {
                    value: "true".to_string()
                }),
                else_block: None,
            })
            .is_err());
    }
}
