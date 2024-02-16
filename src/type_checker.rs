use crate::ast::{Expr, ExprKind};
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

    pub fn typecheck(&mut self, node: Expr) -> Result<Value, Box<dyn Error>> {
        let value = match node.content {
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
                let identifier_value = self.symbol_table.get(&value);
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
            ExprKind::BinaryOperation {
                left,
                operation,
                right,
            } => {
                let left_expr = *left;
                let variable_name = match left_expr.content {
                    ExprKind::Identifier { ref value } => value.to_string(),
                    ExprKind::VarDeclaration { ref identifier } => identifier.to_string(),
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
            ExprKind::Block { expressions } => {
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
            ExprKind::VarDeclaration { identifier } => {
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
                .typecheck(
                    ExprKind::Literal {
                        value: "1".to_string()
                    }
                    .into()
                )
                .unwrap(),
            Value::Int { value: _ }
        ));
        assert!(matches!(
            checker
                .typecheck(
                    ExprKind::Literal {
                        value: "true".to_string()
                    }
                    .into()
                )
                .unwrap(),
            Value::Bool { value: _ }
        ));
        assert!(checker
            .typecheck(
                ExprKind::Literal {
                    value: "should_fail".to_string()
                }
                .into()
            )
            .is_err());
    }

    #[test]
    fn test_assignment() {
        let mut checker = TypeChecker::new();
        assert!(matches!(
            &checker
                .typecheck(
                    ExprKind::BinaryOperation {
                        left: ExprKind::VarDeclaration {
                            identifier: "a".to_string()
                        }
                        .into(),
                        operation: "=".to_string(),
                        right: ExprKind::Literal {
                            value: "1".to_string()
                        }
                        .into()
                    }
                    .into()
                )
                .unwrap(),
            Value::None
        ));

        assert!(matches!(
            &checker
                .typecheck(
                    ExprKind::BinaryOperation {
                        left: ExprKind::Identifier {
                            value: "a".to_string()
                        }
                        .into(),
                        operation: "=".to_string(),
                        right: ExprKind::Literal {
                            value: "3".to_string()
                        }
                        .into()
                    }
                    .into()
                )
                .unwrap(),
            Value::None
        ));

        assert!(&checker
            .typecheck(
                ExprKind::BinaryOperation {
                    left: ExprKind::Identifier {
                        value: "a".to_string()
                    }
                    .into(),
                    operation: "=".to_string(),
                    right: ExprKind::Literal {
                        value: "false".to_string()
                    }
                    .into()
                }
                .into()
            )
            .is_err());
    }

    #[test]
    fn test_proper_comparison() {
        let mut checker = TypeChecker::new();
        assert!(matches!(
            &checker
                .typecheck(
                    ExprKind::BinaryOperation {
                        left: ExprKind::Literal {
                            value: "3".to_string()
                        }
                        .into(),
                        operation: ">".to_string(),
                        right: ExprKind::Literal {
                            value: "1".to_string()
                        }
                        .into()
                    }
                    .into()
                )
                .unwrap(),
            Value::Bool { value: _ }
        ));
    }

    #[test]
    fn test_comparing_bools() {
        let mut checker = TypeChecker::new();

        assert!(&checker
            .typecheck(
                ExprKind::BinaryOperation {
                    left: ExprKind::Literal {
                        value: "true".to_string()
                    }
                    .into(),
                    operation: "<".to_string(),
                    right: ExprKind::Literal {
                        value: "false".to_string()
                    }
                    .into()
                }
                .into()
            )
            .is_err());
    }

    #[test]
    fn test_comparing_bool_and_int() {
        let mut checker = TypeChecker::new();

        assert!(&checker
            .typecheck(
                ExprKind::BinaryOperation {
                    left: ExprKind::Literal {
                        value: "10".to_string()
                    }
                    .into(),
                    operation: "==".to_string(),
                    right: ExprKind::Literal {
                        value: "false".to_string()
                    }
                    .into()
                }
                .into()
            )
            .is_err());
    }

    #[test]
    fn test_plus_minus_etc() {
        let mut checker = TypeChecker::new();
        assert!(matches!(
            &checker
                .typecheck(
                    ExprKind::BinaryOperation {
                        left: ExprKind::Literal {
                            value: "3".to_string()
                        }
                        .into(),
                        operation: "+".to_string(),
                        right: ExprKind::Literal {
                            value: "1".to_string()
                        }
                        .into()
                    }
                    .into()
                )
                .unwrap(),
            Value::Int { value: _ }
        ));

        assert!(&checker
            .typecheck(
                ExprKind::BinaryOperation {
                    left: ExprKind::Literal {
                        value: "true".to_string()
                    }
                    .into(),
                    operation: "-".to_string(),
                    right: ExprKind::Literal {
                        value: "false".to_string()
                    }
                    .into()
                }
                .into()
            )
            .is_err());

        assert!(&checker
            .typecheck(
                ExprKind::BinaryOperation {
                    left: ExprKind::Literal {
                        value: "10".to_string()
                    }
                    .into(),
                    operation: "*".to_string(),
                    right: ExprKind::Literal {
                        value: "false".to_string()
                    }
                    .into()
                }
                .into()
            )
            .is_err());
    }

    #[test]
    fn test_proper_if_then_else() {
        let mut checker = TypeChecker::new();
        assert!(matches!(
            &checker
                .typecheck(
                    ExprKind::IfClause {
                        condition: ExprKind::Literal {
                            value: "true".to_string()
                        }
                        .into(),
                        if_block: ExprKind::Literal {
                            value: "3".to_string()
                        }
                        .into(),
                        else_block: Some(
                            ExprKind::Literal {
                                value: "1".to_string()
                            }
                            .into()
                        )
                    }
                    .into()
                )
                .unwrap(),
            Value::Int { value: _ }
        ));
    }

    #[test]
    fn test_if_with_int_condition() {
        let mut checker = TypeChecker::new();
        assert!(&checker
            .typecheck(
                ExprKind::IfClause {
                    condition: ExprKind::Literal {
                        value: "1".to_string()
                    }
                    .into(),
                    if_block: ExprKind::Literal {
                        value: "3".to_string()
                    }
                    .into(),
                    else_block: Some(
                        ExprKind::Literal {
                            value: "1".to_string()
                        }
                        .into()
                    )
                }
                .into()
            )
            .is_err());
    }

    #[test]
    fn test_if_and_else_different_types() {
        let mut checker = TypeChecker::new();
        assert!(&checker
            .typecheck(
                ExprKind::IfClause {
                    condition: ExprKind::Literal {
                        value: "true".to_string()
                    }
                    .into(),
                    if_block: ExprKind::Literal {
                        value: "true".to_string()
                    }
                    .into(),
                    else_block: Some(
                        ExprKind::Literal {
                            value: "1".to_string()
                        }
                        .into()
                    )
                }
                .into()
            )
            .is_err());
    }

    #[test]
    fn test_proper_if_without_else() {
        let mut checker = TypeChecker::new();
        assert!(matches!(
            &checker
                .typecheck(
                    ExprKind::IfClause {
                        condition: ExprKind::Literal {
                            value: "true".to_string()
                        }
                        .into(),
                        if_block: ExprKind::BinaryOperation {
                            left: ExprKind::VarDeclaration {
                                identifier: "a".to_string()
                            }
                            .into(),
                            operation: "=".to_string(),
                            right: ExprKind::Literal {
                                value: "true".to_string()
                            }
                            .into()
                        }
                        .into(),
                        else_block: None,
                    }
                    .into()
                )
                .unwrap(),
            Value::None
        ));
    }

    #[test]
    fn test_if_with_return_without_else() {
        let mut checker = TypeChecker::new();
        assert!(&checker
            .typecheck(
                ExprKind::IfClause {
                    condition: ExprKind::Literal {
                        value: "true".to_string()
                    }
                    .into(),
                    if_block: ExprKind::Literal {
                        value: "true".to_string()
                    }
                    .into(),
                    else_block: None,
                }
                .into()
            )
            .is_err());
    }
}
