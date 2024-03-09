use crate::ast::{Expr, ExprKind};
use crate::tokenizer::{Token, TokenType};
use std::iter::Peekable;
use std::slice::Iter;

pub fn parse(tokens: &[Token]) -> Result<Vec<Expr>, String> {
    let mut iter = tokens.iter().peekable();
    let mut expressions = Vec::new();

    while iter.peek().is_some() {
        let next = iter.peek().expect("Checked on previous line").text.as_str();
        match next {
            "{" => {
                consume(&mut iter, Some(vec!["{"]));
                expressions.push(*parse_block(&mut iter)?);
            }
            _ => {
                expressions.push(*parse_assignment(&mut iter)?);
                if let Some(token) = iter.peek() {
                    if token.text.as_str() == ";" {
                        consume(&mut iter, Some(vec![";"]));
                    } else if token.text.as_str() != "}" {
                        return Err(format!("Expected end of block or EOF, got {}", token.text));
                    }
                }
            }
        }
    }

    if expressions.is_empty() {
        Err("Expected expression, found nothing".to_string())
    } else if iter.peek().is_some() {
        Err(format!("Expected EOF, got {:?}", consume(&mut iter, None)).to_string())
    } else {
        Ok(expressions)
    }
}

fn parse_block(iter: &mut Peekable<Iter<'_, Token>>) -> Result<Box<Expr>, String> {
    let mut block = ExprKind::Block {
        expressions: Vec::new(),
    };

    while iter.peek().is_some() {
        let next = iter.peek().expect("Checked on previous line").text.as_str();
        match next {
            "{" => {
                consume(iter, Some(vec!["{"]));
                block.push(*parse_block(iter)?);
            }
            "}" => {
                consume(iter, Some(vec!["}"]));
                break;
            }
            _ => {
                block.push(*parse_assignment(iter)?);
                if let Some(token) = iter.peek() {
                    if token.text.as_str() == ";" {
                        consume(iter, Some(vec![";"]));
                    } else if token.text.as_str() != "}" {
                        return Err(format!("Expected end of block or EOF, got {}", token.text));
                    }
                }
            }
        }
    }
    if block.get_first().is_none() {
        return Err("Expected expression, found nothing".to_string());
    }
    Ok(block.into())
}

fn parse_assignment(iter: &mut Peekable<Iter<'_, Token>>) -> Result<Box<Expr>, String> {
    let mut left = parse_binop(iter)?;
    while iter.peek().is_some() {
        if iter.peek().expect("Checked on previous line").text.as_str() != "=" {
            break;
        }
        let operation = consume(iter, Some(vec!["="])).expect("Checked on previous if statement");
        let right = parse_binop(iter)?;
        left = ExprKind::BinaryOperation {
            left,
            operation,
            right,
        }
        .into();
    }
    Ok(left)
}

fn parse_binop(iter: &mut Peekable<Iter<'_, Token>>) -> Result<Box<Expr>, String> {
    let mut left: Box<Expr> = parse_polynomial(iter)?;
    while iter.peek().is_some() {
        if !["<", ">", "<=", ">=", "==", "!="].contains(
            &iter
                .peek()
                .expect("Checked in while condition.")
                .text
                .as_str(),
        ) {
            break;
        };

        let operation = consume(iter, Some(vec!["<", ">", "<=", ">=", "==", "!="]))
            .expect("Should fail if None.");
        let right = parse_polynomial(iter)?;
        left = ExprKind::BinaryOperation {
            left,
            operation,
            right,
        }
        .into();
    }
    Ok(left)
}

fn parse_polynomial(iter: &mut Peekable<Iter<'_, Token>>) -> Result<Box<Expr>, String> {
    let mut left: Box<Expr> = parse_term(iter)?;
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
        left = ExprKind::BinaryOperation {
            left,
            operation,
            right,
        }
        .into();
    }
    Ok(left)
}

fn parse_term(iter: &mut Peekable<Iter<'_, Token>>) -> Result<Box<Expr>, String> {
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

        let operation = consume(iter, Some(vec!["*", "/"])).expect("Should fail if None.");
        let right = parse_factor(iter)?;
        left = ExprKind::BinaryOperation {
            left,
            operation,
            right,
        }
        .into();
    }
    Ok(left)
}

fn parse_factor(iter: &mut Peekable<Iter<'_, Token>>) -> Result<Box<Expr>, String> {
    if iter.peek().is_none() {
        return Err("No more tokens to read".to_string());
    }
    let token = iter.peek().expect("Should never be None.");
    match token.text.as_str() {
        "(" => return parse_parenthesized(iter),
        "if" => return parse_if_expression(iter),
        "var" => return parse_var_declaration(iter),
        "while" => return parse_while(iter),
        _ => {} // Continue according to token type
    }

    match token.tokentype {
        TokenType::IntLiteral => parse_int_literal(iter),
        TokenType::Identifier => parse_identifier(iter),
        _ => Err(format!("Expected IntLiteral, got {:?}", token.text)),
    }
}

fn parse_parenthesized(iter: &mut Peekable<Iter<'_, Token>>) -> Result<Box<Expr>, String> {
    consume(iter, Some(vec!["("]));
    let expression = parse_binop(iter)?;
    consume(iter, Some(vec![")"]));
    Ok(expression)
}

fn parse_if_expression(iter: &mut Peekable<Iter<'_, Token>>) -> Result<Box<Expr>, String> {
    consume(iter, Some(vec!["if"]));
    let condition = parse_binop(iter)?;
    consume(iter, Some(vec!["then"]));
    let if_block = parse_binop(iter)?;

    // Optional else block
    let mut else_block = None;
    if let Some(next) = iter.peek() {
        if next.text == "else" {
            consume(iter, Some(vec!["else"]));
            else_block = Some(parse_binop(iter)?);
        }
    }

    Ok(ExprKind::IfClause {
        condition,
        if_block,
        else_block,
    }
    .into())
}

fn parse_var_declaration(iter: &mut Peekable<Iter<'_, Token>>) -> Result<Box<Expr>, String> {
    consume(iter, Some(vec!["var"]));
    let identifier = parse_identifier(iter)?;
    if let ExprKind::Identifier { value } = identifier.content {
        Ok(ExprKind::VarDeclaration { identifier: value }.into())
    } else {
        Err(format!("Expected identifier, got {:?}", *identifier))
    }
}

fn parse_while(iter: &mut Peekable<Iter<'_, Token>>) -> Result<Box<Expr>, String> {
    consume(iter, Some(vec!["while"]));
    let condition = parse_binop(iter)?;
    consume(iter, Some(vec!["do"]));
    let peek = iter.peek();
    if peek.is_none() {
        return Err("Expected expression, found nothing.".into());
    }
    let do_block = match peek.unwrap().text.as_str() {
        "{" => {
            consume(iter, Some(vec!["{"]));
            parse_block(iter)?
        }
        _ => parse_binop(iter)?,
    };
    Ok(ExprKind::WhileDo {
        condition,
        do_block,
    }
    .into())
}

fn parse_int_literal(iter: &mut Peekable<Iter<'_, Token>>) -> Result<Box<Expr>, String> {
    let token = iter.peek().expect("Should never be None.");
    match token.tokentype {
        TokenType::IntLiteral => Ok(ExprKind::Literal {
            value: consume(iter, None).expect("Should fail if None."),
        }
        .into()),
        _ => Err("Expected IntLiteral".to_string()),
    }
}

fn parse_identifier(iter: &mut Peekable<Iter<'_, Token>>) -> Result<Box<Expr>, String> {
    let token = iter.peek().expect("Should never be None.");
    match token.tokentype {
        TokenType::Identifier => {
            if ["true", "false"].contains(&token.text.as_str()) {
                Ok(ExprKind::Literal {
                    value: consume(iter, None).expect("Should fail if None."),
                }
                .into())
            } else {
                Ok(ExprKind::Identifier {
                    value: consume(iter, None).expect("Should fail if None."),
                }
                .into())
            }
        }
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
        } else {
            let token = iter.next().expect("peek() is Some");
            return Some(token.text.clone());
        }
    }
    None
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tokenizer::tokenize;

    #[test]
    fn test_while_binop() {
        let expression = parse(&tokenize("while true do 1 + 1"));
        assert!(expression.is_ok());
        assert_eq!(
            expression
                .unwrap()
                .first()
                .expect("Parsing returned nothing!"),
            &ExprKind::WhileDo {
                condition: ExprKind::Literal {
                    value: "true".into()
                }
                .into(),
                do_block: ExprKind::BinaryOperation {
                    left: ExprKind::Literal { value: "1".into() }.into(),
                    operation: "+".into(),
                    right: ExprKind::Literal { value: "1".into() }.into()
                }
                .into()
            }
            .into()
        )
    }

    #[test]
    fn test_while_block() {
        let expression = parse(&tokenize("while true do {1 + 1}"));
        dbg!(&expression);
        assert!(expression.is_ok());
        assert_eq!(
            expression
                .unwrap()
                .first()
                .expect("Parsing returned nothing!"),
            &ExprKind::WhileDo {
                condition: ExprKind::Literal {
                    value: "true".into()
                }
                .into(),
                do_block: ExprKind::Block {
                    expressions: vec![ExprKind::BinaryOperation {
                        left: ExprKind::Literal { value: "1".into() }.into(),
                        operation: "+".into(),
                        right: ExprKind::Literal { value: "1".into() }.into()
                    }
                    .into()]
                }
                .into()
            }
            .into()
        )
    }

    #[test]
    fn test_boolean_literal() {
        let expression = parse(&tokenize("true"));
        assert!(expression.is_ok());
        assert_eq!(
            expression
                .unwrap()
                .first()
                .expect("Should be Some, else the test should fail"),
            &ExprKind::Literal {
                value: "true".to_string()
            }
            .into()
        );

        let expression = parse(&tokenize("false"));
        assert!(expression.is_ok());
        assert_eq!(
            expression
                .unwrap()
                .first()
                .expect("Should be Some, else the test should fail"),
            &ExprKind::Literal {
                value: "false".to_string()
            }
            .into()
        )
    }

    #[test]
    fn test_assignment() {
        let expression = parse(&tokenize("a = 10"));

        assert_eq!(
            expression
                .unwrap()
                .first()
                .expect("Should be Some, else test should fail"),
            &ExprKind::BinaryOperation {
                left: ExprKind::Identifier {
                    value: "a".to_string()
                }
                .into(),
                operation: "=".to_string(),
                right: ExprKind::Literal {
                    value: "10".to_string()
                }
                .into(),
            }
            .into()
        );
    }

    #[test]
    fn test_var_declaration() {
        let expression = parse(&tokenize("var a = 10"));

        assert_eq!(
            expression
                .unwrap()
                .first()
                .expect("Should be Some, else test should fail"),
            &ExprKind::BinaryOperation {
                left: ExprKind::VarDeclaration {
                    identifier: "a".to_string()
                }
                .into(),
                operation: "=".to_string(),
                right: ExprKind::Literal {
                    value: "10".to_string()
                }
                .into(),
            }
            .into()
        );
    }

    #[test]
    fn test_block() {
        let expression = parse(&tokenize("{ 1 + 1 }"));
        assert_eq!(
            expression
                .unwrap()
                .first()
                .expect("Should be Some, else test should fail")
                .content
                .get_first()
                .expect("Should be Some, else test should fail"),
            &ExprKind::BinaryOperation {
                left: ExprKind::Literal {
                    value: "1".to_string()
                }
                .into(),
                operation: "+".to_string(),
                right: ExprKind::Literal {
                    value: "1".to_string()
                }
                .into()
            }
            .into()
        );
    }

    #[test]
    fn test_2_expressions() {
        let expressions = parse(&tokenize("1; 2;")).unwrap();
        assert_eq!(expressions.len(), 2);
        assert_eq!(
            expressions[0],
            ExprKind::Literal {
                value: "1".to_string()
            }
            .into()
        );
        assert_eq!(
            expressions[1],
            ExprKind::Literal {
                value: "2".to_string()
            }
            .into()
        );
    }

    #[test]
    fn test_if_expression() {
        let expression = parse(&tokenize("if 2 then 3"));

        assert_eq!(
            expression
                .unwrap()
                .first()
                .expect("Should be Some, else test should fail"),
            &ExprKind::IfClause {
                condition: ExprKind::Literal {
                    value: "2".to_string()
                }
                .into(),
                if_block: ExprKind::Literal {
                    value: "3".to_string()
                }
                .into(),
                else_block: None
            }
            .into()
        );
    }

    #[test]
    fn test_if_else() {
        let expression = parse(&tokenize("if true then 3 else 1"));

        assert_eq!(
            expression
                .unwrap()
                .first()
                .expect("Should be Some, else test should fail"),
            &ExprKind::IfClause {
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
        );
    }

    #[test]
    fn test_comparison() {
        let expression = parse(&tokenize("a < 1"));

        assert_eq!(
            expression
                .unwrap()
                .first()
                .expect("Should be Some, else test should fail"),
            &ExprKind::BinaryOperation {
                left: ExprKind::Identifier {
                    value: "a".to_string()
                }
                .into(),
                operation: "<".to_string(),
                right: ExprKind::Literal {
                    value: "1".to_string()
                }
                .into()
            }
            .into()
        );

        let expression = parse(&tokenize("x < 2 + 1"));

        assert_eq!(
            expression
                .unwrap()
                .first()
                .expect("Should be Some, else test should fail"),
            &ExprKind::BinaryOperation {
                left: ExprKind::Identifier {
                    value: "x".to_string()
                }
                .into(),
                operation: "<".to_string(),
                right: ExprKind::BinaryOperation {
                    left: ExprKind::Literal {
                        value: "2".to_string()
                    }
                    .into(),
                    operation: "+".to_string(),
                    right: ExprKind::Literal {
                        value: "1".to_string()
                    }
                    .into()
                }
                .into()
            }
            .into()
        );
    }

    #[test]
    fn test_identifier() {
        let expression = parse(&tokenize("a + 1"));

        assert_eq!(
            expression
                .unwrap()
                .first()
                .expect("Should be Some, else test should fail"),
            &ExprKind::BinaryOperation {
                left: ExprKind::Identifier {
                    value: "a".to_string()
                }
                .into(),
                operation: "+".to_string(),
                right: ExprKind::Literal {
                    value: "1".to_string()
                }
                .into()
            }
            .into()
        );

        let expression = parse(&tokenize("variable + x"));

        assert_eq!(
            expression
                .unwrap()
                .first()
                .expect("Should be Some, else test should fail"),
            &ExprKind::BinaryOperation {
                left: ExprKind::Identifier {
                    value: "variable".to_string()
                }
                .into(),
                operation: "+".to_string(),
                right: ExprKind::Identifier {
                    value: "x".to_string()
                }
                .into()
            }
            .into()
        );
    }

    #[test]
    fn test_parsing_nothing() {
        let result = parse(&[]);
        dbg!(&result);
        assert!(result.is_err());
    }

    #[test]
    fn test_garbage_at_the_end() {
        let result = parse(&tokenize("1+1 1"));
        dbg!(&result);
        assert!(result.is_err());
    }

    #[test]
    fn test_add_and_sub() {
        let expression = parse(&tokenize("1 + 1"));
        assert_eq!(
            expression
                .unwrap()
                .first()
                .expect("Should be Some, else test should fail"),
            &ExprKind::BinaryOperation {
                left: ExprKind::Literal {
                    value: "1".to_string()
                }
                .into(),
                operation: "+".to_string(),
                right: ExprKind::Literal {
                    value: "1".to_string()
                }
                .into()
            }
            .into()
        );

        let expression = parse(&tokenize("1 + 3 - 5"));
        assert_eq!(
            expression
                .unwrap()
                .first()
                .expect("Should be Some, else test should fail"),
            &ExprKind::BinaryOperation {
                left: ExprKind::BinaryOperation {
                    left: ExprKind::Literal {
                        value: "1".to_string()
                    }
                    .into(),
                    operation: "+".to_string(),
                    right: ExprKind::Literal {
                        value: "3".to_string()
                    }
                    .into()
                }
                .into(),
                operation: "-".to_string(),
                right: ExprKind::Literal {
                    value: "5".to_string()
                }
                .into()
            }
            .into()
        );

        let expression = parse(&tokenize("5 + 3 - 1 + 3 - 5 + 10 + 1"));
        assert_eq!(
            expression
                .unwrap()
                .first()
                .expect("Should be Some, else test should fail"),
            &ExprKind::BinaryOperation {
                left: ExprKind::BinaryOperation {
                    left: ExprKind::BinaryOperation {
                        left: ExprKind::BinaryOperation {
                            left: ExprKind::BinaryOperation {
                                left: ExprKind::BinaryOperation {
                                    left: ExprKind::Literal {
                                        value: "5".to_string()
                                    }
                                    .into(),
                                    operation: "+".to_string(),
                                    right: ExprKind::Literal {
                                        value: "3".to_string()
                                    }
                                    .into()
                                }
                                .into(),
                                operation: "-".to_string(),
                                right: ExprKind::Literal {
                                    value: "1".to_string()
                                }
                                .into()
                            }
                            .into(),
                            operation: "+".to_string(),
                            right: ExprKind::Literal {
                                value: "3".to_string()
                            }
                            .into()
                        }
                        .into(),
                        operation: "-".to_string(),
                        right: ExprKind::Literal {
                            value: "5".to_string()
                        }
                        .into()
                    }
                    .into(),
                    operation: "+".to_string(),
                    right: ExprKind::Literal {
                        value: "10".to_string()
                    }
                    .into()
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
    }

    #[test]
    fn test_mul_and_division() {
        let expression = parse(&tokenize("1 * 1"));
        assert_eq!(
            expression
                .unwrap()
                .first()
                .expect("Should be Some, else test should fail"),
            &ExprKind::BinaryOperation {
                left: ExprKind::Literal {
                    value: "1".to_string()
                }
                .into(),
                operation: "*".to_string(),
                right: ExprKind::Literal {
                    value: "1".to_string()
                }
                .into()
            }
            .into()
        );

        let expression = parse(&tokenize("1 * 3 - 5"));
        assert_eq!(
            expression
                .unwrap()
                .first()
                .expect("Should be Some, else test should fail"),
            &ExprKind::BinaryOperation {
                left: ExprKind::BinaryOperation {
                    left: ExprKind::Literal {
                        value: "1".to_string()
                    }
                    .into(),
                    operation: "*".to_string(),
                    right: ExprKind::Literal {
                        value: "3".to_string()
                    }
                    .into()
                }
                .into(),
                operation: "-".to_string(),
                right: ExprKind::Literal {
                    value: "5".to_string()
                }
                .into()
            }
            .into()
        );

        let expression = parse(&tokenize("5 + 3 * 1"));
        assert_eq!(
            expression
                .unwrap()
                .first()
                .expect("Should be Some, else test should fail"),
            &ExprKind::BinaryOperation {
                left: ExprKind::Literal {
                    value: "5".to_string()
                }
                .into(),
                operation: "+".to_string(),
                right: ExprKind::BinaryOperation {
                    left: ExprKind::Literal {
                        value: "3".to_string()
                    }
                    .into(),
                    operation: "*".to_string(),
                    right: ExprKind::Literal {
                        value: "1".to_string()
                    }
                    .into(),
                }
                .into()
            }
            .into()
        )
    }

    #[test]
    fn test_parentheses() {
        let expression = parse(&tokenize("(5 + 3) + 1"));
        assert_eq!(
            expression
                .unwrap()
                .first()
                .expect("Should be Some, else test should fail"),
            &ExprKind::BinaryOperation {
                left: ExprKind::BinaryOperation {
                    left: ExprKind::Literal {
                        value: "5".to_string(),
                    }
                    .into(),
                    operation: "+".to_string(),
                    right: ExprKind::Literal {
                        value: "3".to_string(),
                    }
                    .into(),
                }
                .into(),
                operation: "+".to_string(),
                right: ExprKind::Literal {
                    value: "1".to_string(),
                }
                .into(),
            }
            .into()
        );

        let expression = parse(&tokenize("5 + (3 + 1)"));
        assert_eq!(
            expression
                .unwrap()
                .first()
                .expect("Should be Some, else test should fail"),
            &ExprKind::BinaryOperation {
                left: ExprKind::Literal {
                    value: "5".to_string()
                }
                .into(),
                operation: "+".to_string(),
                right: ExprKind::BinaryOperation {
                    left: ExprKind::Literal {
                        value: "3".to_string()
                    }
                    .into(),
                    operation: "+".to_string(),
                    right: ExprKind::Literal {
                        value: "1".to_string()
                    }
                    .into(),
                }
                .into()
            }
            .into()
        )
    }
}
