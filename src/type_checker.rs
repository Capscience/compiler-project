use crate::ast::{Expr, ExprKind, Type};
use crate::variable::SymbolTable;
use std::error::Error;
use std::mem::discriminant;

pub struct TypeChecker {
    symbol_table: SymbolTable<Type>,
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

    pub fn typecheck(&mut self, mut node: Expr) -> Result<Type, Box<dyn Error>> {
        let type_ = match node.content {
            ExprKind::Literal { value } => {
                if value.parse::<bool>().is_ok() {
                    Type::Bool
                } else if value.parse::<i64>().is_ok() {
                    Type::Int
                } else {
                    return Err(format!("Invalid literal '{value}'").into());
                }
            }
            ExprKind::Identifier { value } => {
                let identifier_value = self.symbol_table.get(&value);
                if let Some(val) = identifier_value {
                    val.clone()
                } else {
                    return Err(format!("Use of undeclared variable '{}'", value).into());
                }
            }
            ExprKind::IfClause {
                condition,
                if_block,
                else_block,
            } => {
                if !matches!(&self.typecheck(*condition)?, Type::Bool) {
                    return Err("If statement condition must be type `Bool`".into());
                }
                let if_type = self.typecheck(*if_block)?;
                if let Some(else_block) = else_block {
                    if discriminant(&self.typecheck(*else_block)?) != discriminant(&if_type) {
                        return Err("If- and Else-blocks must have same types".into());
                    }
                    if_type
                } else {
                    if if_type != Type::None {
                        return Err("If-block cannot return a value without an Else-block".into());
                    }
                    Type::None
                }
            }
            ExprKind::BinaryOperation {
                left,
                operation,
                right,
            } => {
                let left_expr = *left;
                let variable_name = match left_expr.content {
                    ExprKind::Identifier { ref value } => value.into(),
                    ExprKind::VarDeclaration { ref identifier } => identifier.into(),
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
                        Type::None
                    }
                    "<" | ">" | "==" | ">=" | "<=" | "!=" => {
                        if !(matches!(&left, Type::Int) && matches!(&right, Type::Int)) {
                            return Err(
                                "Both operands must be type `Int` when using comparison".into()
                            );
                        }
                        Type::Bool
                    }
                    "+" | "-" | "*" | "/" | "%" => {
                        if !(matches!(&left, Type::Int) && matches!(&right, Type::Int)) {
                            return Err(format!(
                                "Both operands must be type `Int` when using operator {operation}"
                            )
                            .into());
                        }
                        Type::Int
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
                let mut val = Type::None;
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
                Type::None
            }
        };

        node.type_ = type_.clone();
        Ok(type_)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_typed_ast_literal() {
        let mut checker = TypeChecker::new();
        assert!(matches!(
            checker
                .typecheck(
                    ExprKind::Literal {
                        value: "true".into()
                    }
                    .into()
                )
                .unwrap(),
            Type::Bool
        ))
    }

    #[test]
    fn test_typed_ast_expression() {
        let mut checker = TypeChecker::new();
        assert!(matches!(
            checker
                .typecheck(
                    ExprKind::BinaryOperation {
                        left: ExprKind::Literal { value: "1".into() }.into(),
                        operation: "+".into(),
                        right: ExprKind::Literal { value: "1".into() }.into()
                    }
                    .into()
                )
                .unwrap(),
            Type::Int
        ))
    }

    #[test]
    fn test_typecheck_literals() {
        let mut checker = TypeChecker::new();
        assert!(matches!(
            checker
                .typecheck(ExprKind::Literal { value: "1".into() }.into())
                .unwrap(),
            Type::Int
        ));
        assert!(matches!(
            checker
                .typecheck(
                    ExprKind::Literal {
                        value: "true".into()
                    }
                    .into()
                )
                .unwrap(),
            Type::Bool
        ));
        assert!(checker
            .typecheck(
                ExprKind::Literal {
                    value: "should_fail".into()
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
                            identifier: "a".into()
                        }
                        .into(),
                        operation: "=".into(),
                        right: ExprKind::Literal { value: "1".into() }.into()
                    }
                    .into()
                )
                .unwrap(),
            Type::None
        ));

        assert!(matches!(
            &checker
                .typecheck(
                    ExprKind::BinaryOperation {
                        left: ExprKind::Identifier { value: "a".into() }.into(),
                        operation: "=".into(),
                        right: ExprKind::Literal { value: "3".into() }.into()
                    }
                    .into()
                )
                .unwrap(),
            Type::None
        ));

        assert!(&checker
            .typecheck(
                ExprKind::BinaryOperation {
                    left: ExprKind::Identifier { value: "a".into() }.into(),
                    operation: "=".into(),
                    right: ExprKind::Literal {
                        value: "false".into()
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
                        left: ExprKind::Literal { value: "3".into() }.into(),
                        operation: ">".into(),
                        right: ExprKind::Literal { value: "1".into() }.into()
                    }
                    .into()
                )
                .unwrap(),
            Type::Bool
        ));
    }

    #[test]
    fn test_comparing_bools() {
        let mut checker = TypeChecker::new();

        assert!(&checker
            .typecheck(
                ExprKind::BinaryOperation {
                    left: ExprKind::Literal {
                        value: "true".into()
                    }
                    .into(),
                    operation: "<".into(),
                    right: ExprKind::Literal {
                        value: "false".into()
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
                    left: ExprKind::Literal { value: "10".into() }.into(),
                    operation: "==".into(),
                    right: ExprKind::Literal {
                        value: "false".into()
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
                        left: ExprKind::Literal { value: "3".into() }.into(),
                        operation: "+".into(),
                        right: ExprKind::Literal { value: "1".into() }.into()
                    }
                    .into()
                )
                .unwrap(),
            Type::Int
        ));

        assert!(&checker
            .typecheck(
                ExprKind::BinaryOperation {
                    left: ExprKind::Literal {
                        value: "true".into()
                    }
                    .into(),
                    operation: "-".into(),
                    right: ExprKind::Literal {
                        value: "false".into()
                    }
                    .into()
                }
                .into()
            )
            .is_err());

        assert!(&checker
            .typecheck(
                ExprKind::BinaryOperation {
                    left: ExprKind::Literal { value: "10".into() }.into(),
                    operation: "*".into(),
                    right: ExprKind::Literal {
                        value: "false".into()
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
                            value: "true".into()
                        }
                        .into(),
                        if_block: ExprKind::Literal { value: "3".into() }.into(),
                        else_block: Some(ExprKind::Literal { value: "1".into() }.into())
                    }
                    .into()
                )
                .unwrap(),
            Type::Int
        ));
    }

    #[test]
    fn test_if_with_int_condition() {
        let mut checker = TypeChecker::new();
        assert!(&checker
            .typecheck(
                ExprKind::IfClause {
                    condition: ExprKind::Literal { value: "1".into() }.into(),
                    if_block: ExprKind::Literal { value: "3".into() }.into(),
                    else_block: Some(ExprKind::Literal { value: "1".into() }.into())
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
                        value: "true".into()
                    }
                    .into(),
                    if_block: ExprKind::Literal {
                        value: "true".into()
                    }
                    .into(),
                    else_block: Some(ExprKind::Literal { value: "1".into() }.into())
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
                            value: "true".into()
                        }
                        .into(),
                        if_block: ExprKind::BinaryOperation {
                            left: ExprKind::VarDeclaration {
                                identifier: "a".into()
                            }
                            .into(),
                            operation: "=".into(),
                            right: ExprKind::Literal {
                                value: "true".into()
                            }
                            .into()
                        }
                        .into(),
                        else_block: None,
                    }
                    .into()
                )
                .unwrap(),
            Type::None
        ));
    }

    #[test]
    fn test_if_with_return_without_else() {
        let mut checker = TypeChecker::new();
        assert!(&checker
            .typecheck(
                ExprKind::IfClause {
                    condition: ExprKind::Literal {
                        value: "true".into()
                    }
                    .into(),
                    if_block: ExprKind::Literal {
                        value: "true".into()
                    }
                    .into(),
                    else_block: None,
                }
                .into()
            )
            .is_err());
    }
}
