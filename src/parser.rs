use crate::ast::Expression;
use crate::tokenizer::{Token, TokenType};
use std::iter::Peekable;
use std::slice::Iter;

pub fn parse(tokens: &[Token]) -> Result<Box<Expression>, String> {
    let mut iter = tokens.iter().peekable();
    parse_expression(&mut iter)
}

fn parse_expression(iter: &mut Peekable<Iter<'_, Token>>) -> Result<Box<Expression>, String> {
    let mut left: Box<Expression> = parse_term(iter).unwrap();
    while iter.peek().is_some() {
        if !["+", "-"].contains(
            &iter
                .peek()
                .expect("Checked in while condition.")
                .text
                .as_str(),
        ) {
            break;
        };

        let operation = consume(iter, Some(vec!["+", "-"])).expect("Should fail if None.");
        let right = parse_term(iter).unwrap();
        left = Box::new(Expression::BinaryOperation {
            left,
            operation,
            right,
        });
    }
    Ok(left)
}

fn parse_term(iter: &mut Peekable<Iter<'_, Token>>) -> Result<Box<Expression>, String> {
    let mut left = parse_factor(iter).unwrap();
    while iter.peek().is_some() {
        if !["*", "/"].contains(
            &iter
                .peek()
                .expect("Checked in while condition.")
                .text
                .as_str(),
        ) {
            break;
        };

        let operation = consume(iter, Some(vec!["+", "-"])).expect("Should fail if None.");
        let right = parse_factor(iter).unwrap();
        left = Box::new(Expression::BinaryOperation {
            left,
            operation,
            right,
        });
    }
    Ok(left)
}

fn parse_factor(iter: &mut Peekable<Iter<'_, Token>>) -> Result<Box<Expression>, String> {
    let token = iter.peek().expect("Should never be None.");
    if token.text == "(" {
        return parse_parenthesized(iter);
    }
    match token.tokentype {
        TokenType::IntLiteral => parse_int_literal(iter),
        TokenType::Identifier => parse_identifier(iter),
        _ => Err("Expected IntLiteral".to_string()),
    }
}

fn parse_parenthesized(iter: &mut Peekable<Iter<'_, Token>>) -> Result<Box<Expression>, String> {
    consume(iter, Some(vec!["("]));
    let expression = parse_expression(iter)?;
    consume(iter, Some(vec![")"]));
    Ok(expression)
}

fn parse_int_literal(iter: &mut Peekable<Iter<'_, Token>>) -> Result<Box<Expression>, String> {
    let token = iter.peek().expect("Should never be None.");
    match token.tokentype {
        TokenType::IntLiteral => Ok(Box::new(Expression::Literal {
            value: consume(iter, None).expect("Should fail if None."),
        })),
        _ => Err("Expected IntLiteral".to_string()),
    }
}

fn parse_identifier(iter: &mut Peekable<Iter<'_, Token>>) -> Result<Box<Expression>, String> {
    let token = iter.peek().expect("Should never be None.");
    match token.tokentype {
        TokenType::Identifier => Ok(Box::new(Expression::Identifier {
            value: consume(iter, None).expect("Should fail if None."),
        })),
        _ => Err("Expected identifier".to_string()),
    }
}

fn consume(iter: &mut Peekable<Iter<'_, Token>>, expected: Option<Vec<&str>>) -> Option<String> {
    if let Some(token) = iter.peek() {
        if let Some(expected) = expected {
            for text in expected {
                if text == token.text {
                    let token = iter.next().unwrap();
                    return Some(token.text.clone());
                }
            }
        }
        let token = iter.next().expect("peek() is Some");
        return Some(token.text.clone());
    }
    None
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tokenizer::tokenize;

    #[test]
    fn test_identifier() {
        let expression = parse(&tokenize("a + 1"));

        assert_eq!(
            expression.unwrap(),
            Box::new(Expression::BinaryOperation {
                left: Box::new(Expression::Identifier {
                    value: "a".to_string()
                }),
                operation: "+".to_string(),
                right: Box::new(Expression::Literal {
                    value: "1".to_string()
                })
            })
        );

        let expression = parse(&tokenize("variable + x"));

        assert_eq!(
            expression.unwrap(),
            Box::new(Expression::BinaryOperation {
                left: Box::new(Expression::Identifier {
                    value: "variable".to_string()
                }),
                operation: "+".to_string(),
                right: Box::new(Expression::Identifier {
                    value: "x".to_string()
                })
            })
        );
    }
    #[test]
    fn test_add_and_sub() {
        let expression = parse(&tokenize("1 + 1"));
        assert_eq!(
            expression.unwrap(),
            Box::new(Expression::BinaryOperation {
                left: Box::new(Expression::Literal {
                    value: "1".to_string()
                }),
                operation: "+".to_string(),
                right: Box::new(Expression::Literal {
                    value: "1".to_string()
                })
            })
        );

        let expression = parse(&tokenize("1 + 3 - 5"));
        assert_eq!(
            expression.unwrap(),
            Box::new(Expression::BinaryOperation {
                left: Box::new(Expression::BinaryOperation {
                    left: Box::new(Expression::Literal {
                        value: "1".to_string()
                    }),
                    operation: "+".to_string(),
                    right: Box::new(Expression::Literal {
                        value: "3".to_string()
                    })
                }),
                operation: "-".to_string(),
                right: Box::new(Expression::Literal {
                    value: "5".to_string()
                })
            })
        );

        let expression = parse(&tokenize("5 + 3 - 1 + 3 - 5 + 10 + 1"));
        assert_eq!(
            expression.unwrap(),
            Box::new(Expression::BinaryOperation {
                left: Box::new(Expression::BinaryOperation {
                    left: Box::new(Expression::BinaryOperation {
                        left: Box::new(Expression::BinaryOperation {
                            left: Box::new(Expression::BinaryOperation {
                                left: Box::new(Expression::BinaryOperation {
                                    left: Box::new(Expression::Literal {
                                        value: "5".to_string()
                                    }),
                                    operation: "+".to_string(),
                                    right: Box::new(Expression::Literal {
                                        value: "3".to_string()
                                    })
                                }),
                                operation: "-".to_string(),
                                right: Box::new(Expression::Literal {
                                    value: "1".to_string()
                                })
                            }),
                            operation: "+".to_string(),
                            right: Box::new(Expression::Literal {
                                value: "3".to_string()
                            })
                        }),
                        operation: "-".to_string(),
                        right: Box::new(Expression::Literal {
                            value: "5".to_string()
                        })
                    }),
                    operation: "+".to_string(),
                    right: Box::new(Expression::Literal {
                        value: "10".to_string()
                    })
                }),
                operation: "+".to_string(),
                right: Box::new(Expression::Literal {
                    value: "1".to_string()
                })
            })
        )
    }

    #[test]
    fn test_mul_and_division() {
        let expression = parse(&tokenize("1 * 1"));
        assert_eq!(
            expression.unwrap(),
            Box::new(Expression::BinaryOperation {
                left: Box::new(Expression::Literal {
                    value: "1".to_string()
                }),
                operation: "*".to_string(),
                right: Box::new(Expression::Literal {
                    value: "1".to_string()
                })
            })
        );

        let expression = parse(&tokenize("1 * 3 - 5"));
        assert_eq!(
            expression.unwrap(),
            Box::new(Expression::BinaryOperation {
                left: Box::new(Expression::BinaryOperation {
                    left: Box::new(Expression::Literal {
                        value: "1".to_string()
                    }),
                    operation: "*".to_string(),
                    right: Box::new(Expression::Literal {
                        value: "3".to_string()
                    })
                }),
                operation: "-".to_string(),
                right: Box::new(Expression::Literal {
                    value: "5".to_string()
                })
            })
        );

        let expression = parse(&tokenize("5 + 3 * 1"));
        assert_eq!(
            expression.unwrap(),
            Box::new(Expression::BinaryOperation {
                left: Box::new(Expression::Literal {
                    value: "5".to_string()
                }),
                operation: "+".to_string(),
                right: Box::new(Expression::BinaryOperation {
                    left: Box::new(Expression::Literal {
                        value: "3".to_string()
                    }),
                    operation: "*".to_string(),
                    right: Box::new(Expression::Literal {
                        value: "1".to_string()
                    }),
                })
            })
        )
    }

    #[test]
    fn test_parentheses() {
        let expression = parse(&tokenize("(5 + 3) + 1"));
        assert_eq!(
            expression.unwrap(),
            Box::new(Expression::BinaryOperation {
                left: Box::new(Expression::BinaryOperation {
                    left: Box::new(Expression::Literal {
                        value: "5".to_string(),
                    }),
                    operation: "+".to_string(),
                    right: Box::new(Expression::Literal {
                        value: "3".to_string(),
                    }),
                }),
                operation: "+".to_string(),
                right: Box::new(Expression::Literal {
                    value: "1".to_string(),
                }),
            })
        );

        let expression = parse(&tokenize("5 + (3 + 1)"));
        assert_eq!(
            expression.unwrap(),
            Box::new(Expression::BinaryOperation {
                left: Box::new(Expression::Literal {
                    value: "5".to_string()
                }),
                operation: "+".to_string(),
                right: Box::new(Expression::BinaryOperation {
                    left: Box::new(Expression::Literal {
                        value: "3".to_string()
                    }),
                    operation: "+".to_string(),
                    right: Box::new(Expression::Literal {
                        value: "1".to_string()
                    }),
                })
            })
        )
    }
}
