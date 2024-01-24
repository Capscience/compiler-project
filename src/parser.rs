use crate::ast::Expression;
use crate::tokenizer::{Token, TokenType};
use std::iter::Peekable;
use std::slice::Iter;

pub fn parse<'a>(tokens: &Vec<Token>) -> Result<Box<Expression>, String> {
    let mut iter = tokens.iter().peekable();
    parse_expression(&mut iter)
}

fn parse_expression<'a>(iter: &mut Peekable<Iter<'_, Token>>) -> Result<Box<Expression>, String> {
    let mut left = Box::new(parse_term(iter).unwrap());
    while vec!["+", "-"].contains(
        &iter
            .peek()
            .unwrap_or(&&Token {
                text: "".to_string(),
                tokentype: TokenType::Comment,
                range: None,
            })
            .text
            .as_str(),
    ) {
        let operation = consume(iter, Some(vec!["+", "-"])).expect("Should fail if None.");
        let right = Box::new(parse_term(iter).unwrap());
        left = Box::new(Expression::BinaryOperation {
            left,
            operation,
            right,
        });
    }
    Ok(left)
}

fn parse_term<'a>(iter: &mut Peekable<Iter<'_, Token>>) -> Result<Expression, String> {
    let token = iter.peek().expect("Should never be None.");
    match token.tokentype {
        TokenType::IntLiteral => parse_int_literal(iter),
        // TokenType::Identifier => parse_identifier(iter),
        _ => Err("Expected IntLiteral".to_string()),
    }
}

fn parse_int_literal<'a>(iter: &mut Peekable<Iter<'_, Token>>) -> Result<Expression, String> {
    let token = iter.peek().expect("Should never be None.");
    match token.tokentype {
        TokenType::IntLiteral => Ok(Expression::Literal {
            value: consume(iter, None).expect("Should fail if None."),
        }),
        _ => Err("Expected IntLiteral".to_string()),
    }
}

fn consume<'a>(
    iter: &mut Peekable<Iter<'_, Token>>,
    expected: Option<Vec<&str>>,
) -> Option<String> {
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
    fn test_parser() {
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
}
