use crate::ast::Expression;
use std::error::Error;

pub fn interpret(node: Expression) -> Result<Option<i32>, Box<dyn Error>> {
    let value = match node {
        Expression::Literal { value } => Some(value.parse::<i32>()?),
        Expression::Identifier { value: _ } => todo!(),
        Expression::IfClause {
            condition,
            if_block,
            else_block,
        } => {
            if interpret(*condition)? != Some(0) {
                interpret(*if_block)?
            } else {
                if let Some(else_block) = else_block {
                    interpret(*else_block)?
                } else {
                    None
                }
            }
        }
        Expression::BinaryOperation {
            left,
            operation,
            right,
        } => {
            let a = interpret(*left)?.expect("Invalid types");
            let b = interpret(*right)?.expect("Invalid types");
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
    };

    Ok(value)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_literal() {
        let a = interpret(Expression::Literal {
            value: "15".to_string(),
        });
        assert!(a.is_ok());
        assert_eq!(a.unwrap(), Some(15));
    }

    #[test]
    fn test_bin_op() {
        let a = interpret(Expression::BinaryOperation {
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

        let b = interpret(Expression::BinaryOperation {
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
        let a = interpret(Expression::IfClause {
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

        let b = interpret(Expression::IfClause {
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

        let c = interpret(Expression::IfClause {
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
