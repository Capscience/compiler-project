use crate::ast::Expression;
use crate::tokenizer::{Token, TokenType};
use std::iter::Peekable;
use std::slice::Iter;

pub fn parse(tokens: &[Token]) -> Result<Box<Expression>, String> {
    let mut iter = tokens.iter().peekable();
    let expression = parse_expression(&mut iter)?;

    if iter.peek().is_some() {
        Err(format!("Expected EOF, got {:?}", consume(&mut iter, None)).to_string())
    } else {
        Ok(expression)
    }
}

fn parse_expression(iter: &mut Peekable<Iter<'_, Token>>) -> Result<Box<Expression>, String> {
    let mut left: Box<Expression> = parse_polynomial(iter)?;
    while iter.peek().is_some() {
        if !["<", ">"].contains(
            &iter
                .peek()
                .expect("Checked in while condition.")
                .text
                .as_str(),
        ) {
            break;
        };

        let operation = consume(iter, Some(vec!["+", "-"])).expect("Should fail if None.");
        let right = parse_polynomial(iter).unwrap();
        left = Box::new(Expression::BinaryOperation {
            left,
            operation,
            right,
        });
    }
    Ok(left)
}

fn parse_polynomial(iter: &mut Peekable<Iter<'_, Token>>) -> Result<Box<Expression>, String> {
    let mut left: Box<Expression> = parse_term(iter)?;
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
        let right = parse_term(iter)?;
        left = Box::new(Expression::BinaryOperation {
            left,
            operation,
            right,
        });
    }
    Ok(left)
}

fn parse_term(iter: &mut Peekable<Iter<'_, Token>>) -> Result<Box<Expression>, String> {
    let mut left = parse_factor(iter)?;
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
        let right = parse_factor(iter)?;
        left = Box::new(Expression::BinaryOperation {
            left,
            operation,
            right,
        });
    }
    Ok(left)
}

fn parse_factor(iter: &mut Peekable<Iter<'_, Token>>) -> Result<Box<Expression>, String> {
    if iter.peek().is_none() {
        return Err("No more tokens to read".to_string());
    }
    let token = iter.peek().expect("Should never be None.");
    if token.text == "(" {
        return parse_parenthesized(iter);
    } else if token.text == "if" {
        return parse_if_expression(iter);
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

fn parse_if_expression(iter: &mut Peekable<Iter<'_, Token>>) -> Result<Box<Expression>, String> {
    consume(iter, Some(vec!["if"]));
    let condition = parse_expression(iter)?;
    consume(iter, Some(vec!["then"]));
    let if_block = parse_expression(iter)?;

    // Optional else block
    let mut else_block = None;
    if let Some(next) = iter.peek() {
        if next.text == "else" {
            consume(iter, Some(vec!["else"]));
            else_block = Some(parse_expression(iter)?);
        }
    }

    Ok(Box::new(Expression::IfClause {
        condition,
        if_block,
        else_block,
    }))
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
                    let token = iter.next()?;
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
    fn test_if_expression() {
        let expression = parse(&tokenize("if 2 then 3"));

        assert_eq!(
            expression.unwrap(),
            Box::new(Expression::IfClause {
                condition: Box::new(Expression::Literal {
                    value: "2".to_string()
                }),
                if_block: Box::new(Expression::Literal {
                    value: "3".to_string()
                }),
                else_block: None
            })
        );
    }

    #[test]
    fn test_if_else() {
        let expression = parse(&tokenize("if true then 3 else 1"));

        assert_eq!(
            expression.unwrap(),
            Box::new(Expression::IfClause {
                condition: Box::new(Expression::Identifier {
                    value: "true".to_string()
                }),
                if_block: Box::new(Expression::Literal {
                    value: "3".to_string()
                }),
                else_block: Some(Box::new(Expression::Literal {
                    value: "1".to_string()
                }))
            })
        );
    }

    #[test]
    fn test_comparison() {
        let expression = parse(&tokenize("a < 1"));

        assert_eq!(
            expression.unwrap(),
            Box::new(Expression::BinaryOperation {
                left: Box::new(Expression::Identifier {
                    value: "a".to_string()
                }),
                operation: "<".to_string(),
                right: Box::new(Expression::Literal {
                    value: "1".to_string()
                })
            })
        );

        let expression = parse(&tokenize("x < 2 + 1"));

        assert_eq!(
            expression.unwrap(),
            Box::new(Expression::BinaryOperation {
                left: Box::new(Expression::Identifier {
                    value: "x".to_string()
                }),
                operation: "<".to_string(),
                right: Box::new(Expression::BinaryOperation {
                    left: Box::new(Expression::Literal {
                        value: "2".to_string()
                    }),
                    operation: "+".to_string(),
                    right: Box::new(Expression::Literal {
                        value: "1".to_string()
                    })
                })
            })
        );
    }

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
    fn test_parsing_nothing() {
        assert!(parse(&[]).is_err());
    }

    #[test]
    fn test_garbage_at_the_end() {
        assert!(parse(&tokenize("1+1 1")).is_err());
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
