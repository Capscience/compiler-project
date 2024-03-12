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

    pub fn typecheck(&mut self, node: &mut Expr) -> Result<Type, Box<dyn Error>> {
        let type_ = match &mut node.content {
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
                if !matches!(&self.typecheck(&mut *condition)?, Type::Bool) {
                    return Err("If statement condition must be type `Bool`".into());
                }
                let if_type = self.typecheck(&mut *if_block)?;
                if let Some(else_block) = else_block {
                    if discriminant(&self.typecheck(&mut *else_block)?) != discriminant(&if_type) {
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
                let mut left_expr = left;
                let left = self.typecheck(&mut left_expr)?;
                let right = self.typecheck(&mut *right)?;
                match operation.as_str() {
                    "=" => {
                        let var_name = match &left_expr.content {
                            ExprKind::VarDeclaration {
                                identifier,
                                annotated_type,
                            } => {
                                if let Some(var_type) = annotated_type {
                                    match var_type.as_str() {
                                        "Int" => self
                                            .symbol_table
                                            .declare(identifier.to_string(), Type::Int)?,
                                        "Bool" => self
                                            .symbol_table
                                            .declare(identifier.to_string(), Type::Bool)?,
                                        "None" => self
                                            .symbol_table
                                            .declare(identifier.to_string(), Type::None)?,
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
                                        .declare(identifier.to_string(), right.clone())?;
                                    identifier
                                }
                            }
                            ExprKind::Identifier { value } => value,
                            _ => return Err("Invalid assignment!".into()),
                        };

                        self.symbol_table.set(var_name.clone(), right.clone())?;
                        right
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
                for mut expression in expressions {
                    val = self.typecheck(&mut expression)?;
                }
                if let Some(symbol_table) = &mut self.symbol_table.parent {
                    self.symbol_table = std::mem::take(symbol_table);
                } else {
                    return Err("Symbol table has no parent!".into());
                }
                val
            }
            ExprKind::VarDeclaration { .. } => Type::None, // Handled in BinOp =
            ExprKind::Unary { operator, target } => {
                let target_type = self.typecheck(target)?;
                if operator.as_str() == "not" && target_type == Type::Bool {
                    Type::Bool
                } else if operator.as_str() == "-" && target_type == Type::Int {
                    Type::Int
                } else {
                    return Err(format!(
                        "Invalid unary operator `{}` for type `{:?}`",
                        operator, target_type
                    )
                    .into());
                }
            }
            ExprKind::WhileDo {
                condition,
                do_block,
            } => todo!(),
            ExprKind::None => Type::None,
        };

        node.type_ = type_.clone();
        Ok(type_)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_unary_int() {
        let mut checker = TypeChecker::new();
        assert!(matches!(
            checker
                .typecheck(
                    &mut ExprKind::Block {
                        expressions: vec![ExprKind::Unary {
                            operator: "-".to_string(),
                            target: ExprKind::Literal {
                                value: "1".to_string()
                            }
                            .into()
                        }
                        .into()]
                    }
                    .into()
                )
                .unwrap(),
            Type::Int
        ));
    }

    #[test]
    fn test_unary_bool() {
        let mut checker = TypeChecker::new();
        assert!(matches!(
            checker
                .typecheck(
                    &mut ExprKind::Unary {
                        operator: "not".to_string(),
                        target: ExprKind::Literal {
                            value: "true".to_string()
                        }
                        .into()
                    }
                    .into()
                )
                .unwrap(),
            Type::Bool
        ));
    }

    #[test]
    fn test_unary_not_int() {
        let mut checker = TypeChecker::new();
        assert!(checker
            .typecheck(
                &mut ExprKind::Unary {
                    operator: "not".to_string(),
                    target: ExprKind::Literal {
                        value: "1".to_string()
                    }
                    .into()
                }
                .into()
            )
            .is_err());
    }

    #[test]
    fn test_unary_minus_bool() {
        let mut checker = TypeChecker::new();
        assert!(checker
            .typecheck(
                &mut ExprKind::Unary {
                    operator: "-".to_string(),
                    target: ExprKind::Literal {
                        value: "true".to_string()
                    }
                    .into()
                }
                .into()
            )
            .is_err());
    }

    #[test]
    fn test_typed_ast_literal() {
        let mut checker = TypeChecker::new();
        assert!(matches!(
            checker
                .typecheck(
                    &mut ExprKind::Literal {
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
                    &mut ExprKind::BinaryOperation {
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
                .typecheck(&mut ExprKind::Literal { value: "1".into() }.into())
                .unwrap(),
            Type::Int
        ));
        assert!(matches!(
            checker
                .typecheck(
                    &mut ExprKind::Literal {
                        value: "true".into()
                    }
                    .into()
                )
                .unwrap(),
            Type::Bool
        ));
        assert!(checker
            .typecheck(
                &mut ExprKind::Literal {
                    value: "should_fail".into()
                }
                .into()
            )
            .is_err());
    }

    #[test]
    fn test_declare_and_assign_new_val() {
        let mut checker = TypeChecker::new();
        assert!(matches!(
            &checker
                .typecheck(
                    &mut ExprKind::BinaryOperation {
                        left: ExprKind::VarDeclaration {
                            identifier: "a".into(),
                            annotated_type: None,
                        }
                        .into(),
                        operation: "=".into(),
                        right: ExprKind::Literal { value: "1".into() }.into()
                    }
                    .into()
                )
                .unwrap(),
            Type::Int
        ));

        assert!(matches!(
            &checker
                .typecheck(
                    &mut ExprKind::BinaryOperation {
                        left: ExprKind::Identifier { value: "a".into() }.into(),
                        operation: "=".into(),
                        right: ExprKind::Literal { value: "3".into() }.into()
                    }
                    .into()
                )
                .unwrap(),
            Type::Int
        ));

        assert!(&checker
            .typecheck(
                &mut ExprKind::BinaryOperation {
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
                    &mut ExprKind::BinaryOperation {
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
                &mut ExprKind::BinaryOperation {
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
                &mut ExprKind::BinaryOperation {
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
                    &mut ExprKind::BinaryOperation {
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
                &mut ExprKind::BinaryOperation {
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
                &mut ExprKind::BinaryOperation {
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
                    &mut ExprKind::IfClause {
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
                &mut ExprKind::IfClause {
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
                &mut ExprKind::IfClause {
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
                    &mut ExprKind::IfClause {
                        condition: ExprKind::Literal {
                            value: "true".into()
                        }
                        .into(),
                        if_block: ExprKind::Block {
                            expressions: vec![
                                ExprKind::BinaryOperation {
                                    left: ExprKind::VarDeclaration {
                                        identifier: "a".into(),
                                        annotated_type: None,
                                    }
                                    .into(),
                                    operation: "=".into(),
                                    right: ExprKind::Literal {
                                        value: "true".into()
                                    }
                                    .into()
                                }
                                .into(),
                                ExprKind::None.into()
                            ]
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
                &mut ExprKind::IfClause {
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
