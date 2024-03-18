use crate::ast::{Expr, ExprKind, Module, Type};
use crate::variable::SymbolTable;
use std::mem::discriminant;

pub struct TypeChecker {
    symbol_table: SymbolTable<Type>,
    checking_function: bool,
}

impl Default for TypeChecker {
    fn default() -> Self {
        Self::new()
    }
}

impl TypeChecker {
    pub fn new() -> Self {
        let mut symbol_table: SymbolTable<Type> = SymbolTable::new(None);
        let _ = symbol_table.declare(
            "read_int".to_string(),
            Type::Func {
                params: Vec::new(),
                ret_type: Type::Int.into(),
            },
        );
        let _ = symbol_table.declare(
            "print_int".to_string(),
            Type::Func {
                params: vec![Type::Int],
                ret_type: Type::None.into(),
            },
        );
        let _ = symbol_table.declare(
            "print_bool".to_string(),
            Type::Func {
                params: vec![Type::Bool],
                ret_type: Type::None.into(),
            },
        );
        TypeChecker {
            symbol_table,
            checking_function: false,
        }
    }

    pub fn typecheck_module(&mut self, module: &mut Module) -> Result<(), String> {
        if let Some(ref mut main) = module.main {
            let _ = self.typecheck(main)?;
        } else {
            return Err("Top level does not exist!".to_string());
        }
        for node in &mut module.functions {
            let _ = self.typecheck(node)?;
        }
        Ok(())
    }

    pub fn typecheck(&mut self, node: &mut Expr) -> Result<Type, String> {
        let type_ = match &mut node.content {
            ExprKind::Return { expr } => self.check_return(expr)?,
            ExprKind::FunDef { .. } => self.check_fun_def()?,
            ExprKind::Literal { value } => self.check_literal(value)?,
            ExprKind::Identifier { value } => self.check_identifier(value)?,
            ExprKind::IfClause {
                condition,
                if_block,
                else_block,
            } => self.check_if_clause(condition, if_block, else_block)?,
            ExprKind::BinaryOperation {
                left,
                operation,
                right,
            } => self.check_bin_op(left, right, operation)?,
            ExprKind::Block { expressions } => self.check_block(expressions)?,
            ExprKind::VarDeclaration { .. } => Type::None, // Handled in BinOp =
            ExprKind::Unary { operator, target } => self.check_unary(operator, target)?,
            ExprKind::WhileDo {
                condition,
                do_block,
            } => self.check_while(condition, do_block)?,
            ExprKind::Call { func, params } => self.check_call(params, func)?,
            ExprKind::None => Type::None,
        };

        node.type_ = type_.clone();
        Ok(type_)
    }

    fn check_return(&mut self, expr: &mut Expr) -> Result<Type, String> {
        if !self.checking_function {
            return Err(
                "'return' expressions can only be used inside function definitions!".to_string(),
            );
        }
        self.typecheck(expr)
    }

    fn check_fun_def(&mut self) -> Result<Type, String> {
        todo!();
    }

    fn check_literal(&mut self, value: &String) -> Result<Type, String> {
        if value.parse::<bool>().is_ok() {
            Ok(Type::Bool)
        } else if value.parse::<i64>().is_ok() {
            Ok(Type::Int)
        } else {
            Err(format!("Invalid literal '{value}'"))
        }
    }

    fn check_identifier(&mut self, value: &String) -> Result<Type, String> {
        let identifier_value = self.symbol_table.get(value);
        if let Some(val) = identifier_value {
            Ok(val.clone())
        } else {
            Err(format!("Use of undeclared variable '{}'", value))
        }
    }

    fn check_if_clause(
        &mut self,
        condition: &mut Expr,
        if_block: &mut Expr,
        else_block: &mut Option<Box<Expr>>,
    ) -> Result<Type, String> {
        if !matches!(&self.typecheck(condition)?, Type::Bool) {
            return Err("If statement condition must be type `Bool`".into());
        }
        let if_type = self.typecheck(&mut *if_block)?;
        if let Some(else_block) = else_block {
            if discriminant(&self.typecheck(&mut *else_block)?) != discriminant(&if_type) {
                return Err("If- and Else-blocks must have same types".into());
            }
            Ok(if_type)
        } else {
            if if_type != Type::None {
                return Err("If-block cannot return a value without an Else-block".into());
            }
            Ok(Type::None)
        }
    }

    fn check_bin_op(
        &mut self,
        left: &mut Expr,
        right: &mut Expr,
        operation: &String,
    ) -> Result<Type, String> {
        let left_expr = left;
        let left = self.typecheck(left_expr)?;
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
                                _ => return Err(format!("Invalid type annotation `{}`", var_type)),
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
                Ok(right)
            }
            "==" | "!=" => {
                if !(matches!(&left, Type::Int) || matches!(&left, Type::Bool)) {
                    return Err(format!(
                        "Operator {operation} is only implemented for Int and Bool types!"
                    ));
                }
                if discriminant(&left) != discriminant(&right) {
                    return Err(format!(
                        "Both operands must be same type for operator {operation}!"
                    ));
                }
                Ok(Type::Bool)
            }
            "<" | ">" | ">=" | "<=" => {
                if !(matches!(&left, Type::Int) && matches!(&right, Type::Int)) {
                    return Err("Both operands must be type `Int` when using comparison".into());
                }
                Ok(Type::Bool)
            }
            "+" | "-" | "*" | "/" | "%" => {
                if !(matches!(&left, Type::Int) && matches!(&right, Type::Int)) {
                    return Err(format!(
                        "Both operands must be type `Int` when using operator {operation}"
                    ));
                }
                Ok(Type::Int)
            }
            "and" | "or" => {
                if !(matches!(&left, Type::Bool) && matches!(&right, Type::Bool)) {
                    return Err(format!(
                        "Both operands must be type `Bool` when using operator {operation}"
                    ));
                }
                Ok(Type::Bool)
            }
            _ => {
                if discriminant(&left) != discriminant(&right) {
                    return Err("Missmatched types!".into());
                }
                Ok(left)
            }
        }
    }

    fn check_block(&mut self, expressions: &mut [Expr]) -> Result<Type, String> {
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
        Ok(val)
    }

    fn check_unary(&mut self, operator: &String, target: &mut Expr) -> Result<Type, String> {
        let target_type = self.typecheck(target)?;
        if operator.as_str() == "not" && target_type == Type::Bool {
            Ok(Type::Bool)
        } else if operator.as_str() == "-" && target_type == Type::Int {
            Ok(Type::Int)
        } else {
            Err(format!(
                "Invalid unary operator `{}` for type `{:?}`",
                operator, target_type
            ))
        }
    }

    fn check_while(&mut self, condition: &mut Expr, do_block: &mut Expr) -> Result<Type, String> {
        if self.typecheck(condition)? != Type::Bool {
            return Err("While loop condition must be type Bool!".into());
        }
        let _ = self.typecheck(do_block);
        Ok(Type::None)
    }

    fn check_call(&mut self, params: &mut [Expr], func: &String) -> Result<Type, String> {
        let mut param_types = Vec::new();
        for param in params {
            let type_result = self.typecheck(param);
            type_result.as_ref()?;
            param_types.push(type_result.unwrap());
        }
        if let Some(Type::Func { params, ret_type }) = self.symbol_table.get(func) {
            for (param_type, should_be) in param_types.iter().zip(params) {
                if param_type != should_be {
                    return Err(format!(
                        "Function call expected type `{:?}`, got `{:?}`",
                        should_be, param_type
                    ));
                }
            }
            Ok(*ret_type.clone())
        } else {
            Err(format!("Undeclared function `{func}`"))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_module() {
        let mut checker = TypeChecker::new();
        let mut module = Module {
            main: Some(
                ExprKind::Literal {
                    value: "true".to_string(),
                }
                .into(),
            ),
            functions: Vec::new(),
        };
        assert!(checker.typecheck_module(&mut module).is_ok());
        assert_eq!(
            module.main.expect("Created as Some above.").type_,
            Type::Bool
        );
    }

    #[test]
    fn test_while_invalid_cond() {
        let mut checker = TypeChecker::new();
        assert!(checker
            .typecheck(
                &mut ExprKind::WhileDo {
                    condition: ExprKind::Literal {
                        value: "1".to_string()
                    }
                    .into(),
                    do_block: ExprKind::Block {
                        expressions: Vec::new()
                    }
                    .into()
                }
                .into()
            )
            .is_err());
    }

    #[test]
    fn test_valid_while() {
        let mut checker = TypeChecker::new();
        assert_eq!(
            checker
                .typecheck(
                    &mut ExprKind::WhileDo {
                        condition: ExprKind::Literal {
                            value: "true".to_string()
                        }
                        .into(),
                        do_block: ExprKind::Block {
                            expressions: Vec::new()
                        }
                        .into()
                    }
                    .into()
                )
                .unwrap(),
            Type::None
        );
    }

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
