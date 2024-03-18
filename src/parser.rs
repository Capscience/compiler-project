use crate::ast::{Expr, ExprKind, Module, Type};
use crate::tokenizer::{Token, TokenType};
use std::iter::Peekable;
use std::vec::IntoIter;

struct Parser {
    iter: Box<Peekable<IntoIter<Token>>>,
    module: Module,
}

pub fn parse_module(tokens: &[Token]) -> Result<Module, String> {
    let mut parser = Parser::new(tokens);
    let top_level = parser.parse()?;
    parser.module.main = Some(top_level);
    Ok(parser.module)
}

pub fn parse(tokens: &[Token]) -> Result<Expr, String> {
    let mut parser = Parser::new(tokens);
    parser.parse()
}

/// Add optional ; after a block where needed
fn preprocess_tokens(tokens: &[Token]) -> Vec<Token> {
    let mut new_tokens = Vec::new();
    let mut previous = Token {
        text: "".to_string(),
        tokentype: TokenType::Comment,
        range: None,
    };
    for token in tokens {
        if previous.text.as_str() == "}"
            && token.text.as_str() != "}"
            && token.text.as_str() != "else"
        {
            new_tokens.push(Token {
                text: ";".to_string(),
                tokentype: TokenType::Delimiter,
                range: None,
            });
        }
        new_tokens.push(token.clone());
        previous = token.clone();
    }
    new_tokens
}

impl Parser {
    pub fn new(tokens: &[Token]) -> Self {
        let tokens: Vec<Token> = preprocess_tokens(tokens);
        let iter = tokens.into_iter().peekable();
        Self {
            iter: Box::new(iter),
            module: Module::new(),
        }
    }
    pub fn parse(&mut self) -> Result<Expr, String> {
        let block = *self.parse_block()?;

        if self.iter.peek().is_some() {
            Err(format!("Expected EOF, got {:?}", self.consume(None)).to_string())
        } else {
            Ok(block)
        }
    }

    fn parse_block(&mut self) -> Result<Box<Expr>, String> {
        let mut block = ExprKind::Block {
            expressions: Vec::new(),
        };

        while self.iter.peek().is_some() {
            let next = self.iter.peek().expect("Checked on previous line.");
            // Handle block end and `;` between expressions in the block.
            match next.text.as_str() {
                "}" => {
                    self.consume(Some(vec!["}"]));
                    break;
                }
                ";" => {
                    self.consume(Some(vec![";"]));
                    block.push(ExprKind::None.into());
                }
                "fun" => {
                    let func = *self.parse_fun_definition()?;
                    self.module.functions.push(func);
                }
                _ => {
                    let expressions = block.expressions().expect("Should be block");
                    if expressions.is_empty()
                        || matches!(
                            expressions.last(),
                            Some(Expr {
                                content: ExprKind::None,
                                type_: Type::None
                            })
                        )
                        || matches!(
                            expressions.last(),
                            Some(Expr {
                                content: ExprKind::Block { .. },
                                type_: Type::None
                            })
                        )
                    {
                        if next.text.as_str() == "{" {
                            self.consume(Some(vec!["{"]));
                            block.push(*self.parse_block()?);
                            if let Some(next) = self.iter.peek() {
                                if next.text.as_str() == "}" {
                                    self.consume(Some(vec!["}"]));
                                    break;
                                }
                            }
                            block.push(ExprKind::None.into());
                        } else {
                            block.push(*self.parse_expr()?);
                        }
                    } else {
                        dbg!(&block);
                        return Err(format!("Expected semicolon, got {:?}", next));
                    }
                }
            }
        }
        if block.get_first().is_none() {
            return Err("Expected expression, found nothing".to_string());
        }
        Ok(block.into())
    }

    fn parse_fun_definition(&mut self) -> Result<Box<Expr>, String> {
        self.consume(Some(vec!["fun"]));
        let func: String = self
            .consume(None)
            .ok_or("Expected function name, got EOF".to_string())?;
        self.consume(Some(vec!["("]))
            .ok_or("Expected `(`, got something else!".to_string())?;
        let mut params = Vec::new();
        let mut param_types = Vec::new();
        while let Some(token) = self.iter.peek() {
            if token.text == ")" {
                self.consume(Some(vec![")"]));
                break;
            };
            params.push(
                self.consume(None)
                    .ok_or("Expected function parameters or ), got EOF!".to_string())?,
            );
            self.consume(Some(vec![":"]))
                .ok_or("Function parameters must have types specified!".to_string())?;
            param_types.push(
                self.consume(None)
                    .ok_or("Expected parameter type, got EOF!".to_string())?,
            );
        }
        let return_type = if self.consume(Some(vec![":"])).is_some() {
            self.consume(None)
        } else {
            None
        };

        if self.consume(Some(vec!["{"])).is_none() {
            return Err("Expected function block, got something else!".to_string());
        }
        let block = self.parse_block()?;
        self.consume(Some(vec![";"]));
        Ok(ExprKind::FunDef {
            name: func,
            params,
            param_types,
            return_type,
            block,
        }
        .into())
    }

    fn parse_expr(&mut self) -> Result<Box<Expr>, String> {
        let mut left = self.parse_binop()?;
        while self.iter.peek().is_some() {
            if self
                .iter
                .peek()
                .expect("Checked on previous line")
                .text
                .as_str()
                != "="
            {
                break;
            }
            let operation = self
                .consume(Some(vec!["="]))
                .expect("Checked on previous if statement");
            let right = self.parse_binop()?;
            left = ExprKind::BinaryOperation {
                left,
                operation,
                right,
            }
            .into();
        }
        Ok(left)
    }

    fn parse_binop(&mut self) -> Result<Box<Expr>, String> {
        let mut left: Box<Expr> = self.parse_binop_and()?;
        while self.iter.peek().is_some() {
            if !["or"].contains(
                &self
                    .iter
                    .peek()
                    .expect("Checked in while condition.")
                    .text
                    .as_str(),
            ) {
                break;
            };

            let operation = self
                .consume(Some(vec!["or"]))
                .expect("Should fail if None.");
            let right = self.parse_polynomial()?;
            left = ExprKind::BinaryOperation {
                left,
                operation,
                right,
            }
            .into();
        }
        Ok(left)
    }

    fn parse_binop_and(&mut self) -> Result<Box<Expr>, String> {
        let mut left: Box<Expr> = self.parse_binop_comparison()?;
        while self.iter.peek().is_some() {
            if !["and"].contains(
                &self
                    .iter
                    .peek()
                    .expect("Checked in while condition.")
                    .text
                    .as_str(),
            ) {
                break;
            };

            let operation = self
                .consume(Some(vec!["and"]))
                .expect("Should fail if None.");
            let right = self.parse_polynomial()?;
            left = ExprKind::BinaryOperation {
                left,
                operation,
                right,
            }
            .into();
        }
        Ok(left)
    }

    fn parse_binop_comparison(&mut self) -> Result<Box<Expr>, String> {
        let mut left: Box<Expr> = self.parse_polynomial()?;
        while self.iter.peek().is_some() {
            if !["<", ">", "<=", ">=", "==", "!="].contains(
                &self
                    .iter
                    .peek()
                    .expect("Checked in while condition.")
                    .text
                    .as_str(),
            ) {
                break;
            };

            let operation = self
                .consume(Some(vec!["<", ">", "<=", ">=", "==", "!="]))
                .expect("Should fail if None.");
            let right = self.parse_polynomial()?;
            left = ExprKind::BinaryOperation {
                left,
                operation,
                right,
            }
            .into();
        }
        Ok(left)
    }

    fn parse_polynomial(&mut self) -> Result<Box<Expr>, String> {
        let mut left: Box<Expr> = self.parse_term()?;
        while self.iter.peek().is_some() {
            if !["+", "-"].contains(
                &self
                    .iter
                    .peek()
                    .expect("Checked in while condition.")
                    .text
                    .as_str(),
            ) {
                break;
            };

            let operation = self
                .consume(Some(vec!["+", "-"]))
                .expect("Should fail if None.");
            let right = self.parse_term()?;
            left = ExprKind::BinaryOperation {
                left,
                operation,
                right,
            }
            .into();
        }
        Ok(left)
    }

    fn parse_term(&mut self) -> Result<Box<Expr>, String> {
        let mut left = self.parse_factor()?;
        while self.iter.peek().is_some() {
            if !["*", "/", "%"].contains(
                &self
                    .iter
                    .peek()
                    .expect("Checked in while condition.")
                    .text
                    .as_str(),
            ) {
                break;
            };

            let operation = self
                .consume(Some(vec!["*", "/", "%"]))
                .expect("Should fail if None.");
            let right = self.parse_factor()?;
            left = ExprKind::BinaryOperation {
                left,
                operation,
                right,
            }
            .into();
        }
        Ok(left)
    }

    fn parse_factor(&mut self) -> Result<Box<Expr>, String> {
        if self.iter.peek().is_none() {
            return Err("No more tokens to read".to_string());
        }
        let token = self.iter.peek().expect("Should never be None.");
        match token.text.as_str() {
            "(" => return self.parse_parenthesized(),
            "{" => {
                self.consume(Some(vec!["{"]));
                return self.parse_block();
            }
            "if" => return self.parse_if_expression(),
            "var" => return self.parse_var_declaration(),
            "while" => return self.parse_while(),
            "return" => return self.parse_return(),
            "-" | "not" => return self.parse_unary(),
            _ => {} // Continue according to token type
        }

        match token.tokentype {
            TokenType::IntLiteral => self.parse_int_literal(),
            TokenType::Identifier => self.parse_identifier(),
            _ => Err(format!("Expected IntLiteral, got {:?}", token.text)),
        }
    }

    fn parse_return(&mut self) -> Result<Box<Expr>, String> {
        self.consume(Some(vec!["return"]))
            .expect("Parse return should never be called if next token is not return!");
        let expr = self.parse_expr()?;
        self.consume(Some(vec![";"]))
            .ok_or("Return statement must end with `;`!".to_string())?;
        Ok(ExprKind::Return { expr }.into())
    }

    fn parse_unary(&mut self) -> Result<Box<Expr>, String> {
        let operator = self
            .consume(Some(vec!["-", "not"]))
            .expect("Tried to parse unary without having an unary operator next!");
        let target = self.parse_factor()?;
        Ok(ExprKind::Unary { operator, target }.into())
    }

    fn parse_parenthesized(&mut self) -> Result<Box<Expr>, String> {
        self.consume(Some(vec!["("]));
        let expression = self.parse_binop()?;
        self.consume(Some(vec![")"]));
        Ok(expression)
    }

    fn parse_if_expression(&mut self) -> Result<Box<Expr>, String> {
        self.consume(Some(vec!["if"]));
        let condition = self.parse_binop()?;
        self.consume(Some(vec!["then"]));
        let if_block = self.parse_expr()?;

        // Optional else block
        let mut else_block = None;
        if let Some(next) = self.iter.peek() {
            if next.text == "else" {
                self.consume(Some(vec!["else"]));
                else_block = Some(self.parse_expr()?);
            }
        }

        Ok(ExprKind::IfClause {
            condition,
            if_block,
            else_block,
        }
        .into())
    }

    fn parse_var_declaration(&mut self) -> Result<Box<Expr>, String> {
        self.consume(Some(vec!["var"]));
        let identifier = self.parse_identifier()?;

        let annotated_type = if self.consume(Some(vec![":"])).is_some() {
            Some(self.consume(None).ok_or("Type annotation empty!")?)
        } else {
            None
        };

        if self.consume(Some(vec!["="])).is_none() {
            return Err("Expected type annotation or assignment!".to_string());
        }

        let right = self.parse_binop()?;

        if let ExprKind::Identifier { value } = identifier.content {
            Ok(ExprKind::BinaryOperation {
                left: ExprKind::VarDeclaration {
                    identifier: value,
                    annotated_type,
                }
                .into(),
                operation: "=".to_string(),
                right,
            }
            .into())
        } else {
            Err(format!("Expected identifier, got {:?}", *identifier))
        }
    }

    fn parse_while(&mut self) -> Result<Box<Expr>, String> {
        self.consume(Some(vec!["while"]));
        let condition = self.parse_binop()?;
        self.consume(Some(vec!["do"]));
        let peek = self.iter.peek();
        if peek.is_none() {
            return Err("Expected expression, found nothing.".into());
        }
        let do_block = self.parse_expr()?;
        Ok(ExprKind::WhileDo {
            condition,
            do_block,
        }
        .into())
    }

    fn parse_call(&mut self, func: String) -> Result<Box<Expr>, String> {
        let mut params = Vec::new();
        self.consume(Some(vec!["("]));
        while self.consume(Some(vec![")"])).is_none() {
            if self.iter.peek().is_some() {
                params.push(*self.parse_binop()?);
            } else {
                return Err("Expected function parameters, found EOF!".to_string());
            }
        }
        Ok(ExprKind::Call { func, params }.into())
    }

    fn parse_int_literal(&mut self) -> Result<Box<Expr>, String> {
        let token = self.iter.peek().expect("Should never be None.");
        match token.tokentype {
            TokenType::IntLiteral => Ok(ExprKind::Literal {
                value: self.consume(None).expect("Should fail if None."),
            }
            .into()),
            _ => Err("Expected IntLiteral".to_string()),
        }
    }

    fn parse_identifier(&mut self) -> Result<Box<Expr>, String> {
        let token = self.iter.peek().expect("Should never be None.");
        match token.tokentype {
            TokenType::Identifier => {
                if ["true", "false"].contains(&token.text.as_str()) {
                    Ok(ExprKind::Literal {
                        value: self.consume(None).expect("Should fail if None."),
                    }
                    .into())
                } else {
                    let text = self.consume(None).expect("Should fail if None.");
                    if let Some(next) = self.iter.peek() {
                        if next.text.as_str() == "(" {
                            return self.parse_call(text);
                        }
                    }
                    Ok(ExprKind::Identifier { value: text }.into())
                }
            }
            _ => Err("Expected identifier".to_string()),
        }
    }

    fn consume(&mut self, expected: Option<Vec<&str>>) -> Option<String> {
        if let Some(token) = self.iter.peek() {
            if let Some(expected) = expected {
                for text in expected {
                    if text == token.text {
                        let token = self.iter.next()?;
                        return Some(token.text.clone());
                    }
                }
            } else {
                let token = self.iter.next().expect("peek() is Some");
                return Some(token.text.clone());
            }
        }
        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tokenizer::tokenize;

    #[test]
    fn test_function_definition() {
        let module = parse_module(&tokenize("fun square(x: Int): Int { return x * x; } 0"));
        assert_eq!(
            module.unwrap(),
            Module {
                main: Some(
                    ExprKind::Block {
                        expressions: vec![ExprKind::Literal {
                            value: "0".to_string()
                        }
                        .into()]
                    }
                    .into()
                ),
                functions: vec![ExprKind::FunDef {
                    name: "square".to_string(),
                    params: vec!["x".to_string()],
                    param_types: vec!["Int".to_string()],
                    return_type: Some("Int".to_string()),
                    block: ExprKind::Block {
                        expressions: vec![ExprKind::Return {
                            expr: ExprKind::BinaryOperation {
                                left: ExprKind::Identifier {
                                    value: "x".to_string()
                                }
                                .into(),
                                operation: "*".to_string(),
                                right: ExprKind::Identifier {
                                    value: "x".to_string()
                                }
                                .into()
                            }
                            .into()
                        }
                        .into()]
                    }
                    .into()
                }
                .into()]
            }
            .into()
        )
    }

    #[test]
    fn test_module() {
        let module = parse_module(&tokenize("true"));
        assert_eq!(
            module.unwrap(),
            Module {
                main: Some(
                    ExprKind::Block {
                        expressions: vec![ExprKind::Literal {
                            value: "true".to_string()
                        }
                        .into()]
                    }
                    .into()
                ),
                functions: Vec::new(),
            }
        )
    }

    #[test]
    fn test_and() {
        let expr = parse(&tokenize("true and true"));
        assert_eq!(
            expr.unwrap().first().expect("Parsing returned nothing!"),
            &ExprKind::BinaryOperation {
                left: ExprKind::Literal {
                    value: "true".to_string()
                }
                .into(),
                operation: "and".to_string(),
                right: ExprKind::Literal {
                    value: "true".to_string()
                }
                .into()
            }
            .into()
        );
    }

    #[test]
    fn test_or() {
        let expr = parse(&tokenize("true or true"));
        assert_eq!(
            expr.unwrap().first().expect("Parsing returned nothing!"),
            &ExprKind::BinaryOperation {
                left: ExprKind::Literal {
                    value: "true".to_string()
                }
                .into(),
                operation: "or".to_string(),
                right: ExprKind::Literal {
                    value: "true".to_string()
                }
                .into()
            }
            .into()
        );
    }

    #[test]
    fn test_and_or() {
        let expr = parse(&tokenize("true and true or true"));
        assert_eq!(
            expr.unwrap().first().expect("Parsing returned nothing!"),
            &ExprKind::BinaryOperation {
                left: ExprKind::BinaryOperation {
                    left: ExprKind::Literal {
                        value: "true".to_string()
                    }
                    .into(),
                    operation: "and".to_string(),
                    right: ExprKind::Literal {
                        value: "true".to_string()
                    }
                    .into(),
                }
                .into(),
                operation: "or".to_string(),
                right: ExprKind::Literal {
                    value: "true".to_string()
                }
                .into()
            }
            .into()
        );
    }

    #[test]
    fn test_function_call() {
        let expr = parse(&tokenize("print_int(1)"));
        assert_eq!(
            expr.unwrap().first().expect("Parsing returned nothing!"),
            &ExprKind::Call {
                func: "print_int".to_string(),
                params: vec![ExprKind::Literal {
                    value: "1".to_string()
                }
                .into()]
            }
            .into()
        );
    }

    #[test]
    fn test_two_blocks_inside_block() {
        let expr = parse(&tokenize("{ { a } { b } }"));
        dbg!(&expr);
        assert!(expr.is_ok());
    }

    #[test]
    fn test_two_exprs_no_semicolon_in_block() {
        let expr = parse(&tokenize("{ a b }"));
        dbg!(&expr);
        assert!(expr.is_err());
    }

    #[test]
    fn test_if_without_semicolon_in_block() {
        let expr = parse(&tokenize("{ if true then { a } b }"));
        dbg!(&expr);
        assert!(expr.is_ok());
    }

    #[test]
    fn test_if_with_semicolon_in_block() {
        let expr = parse(&tokenize("{ if true then { a }; b }"));
        dbg!(&expr);
        assert!(expr.is_ok());
    }

    #[test]
    fn test_three_exprs_in_block_invalid_semicolon() {
        let expr = parse(&tokenize("{ if true then { a }; b c }"));
        dbg!(&expr);
        assert!(expr.is_err());
    }

    #[test]
    fn test_three_exprs_in_block_valid_semicolon() {
        let expr = parse(&tokenize("{ if true then { a } b; c }"));
        dbg!(&expr);
        assert!(expr.is_ok());
    }

    #[test]
    fn test_if_else_w_blocks_valid_semicolon() {
        let expr = parse(&tokenize("{ if true then { a } else { b } 3 }"));
        dbg!(&expr);
        assert!(expr.is_ok());
    }

    #[test]
    fn test_block_to_var_w_call_valid() {
        let expr = parse(&tokenize("x = { { f(a) } { b } }"));
        dbg!(&expr);
        assert!(expr.is_ok());
    }

    #[test]
    fn test_int_unary() {
        let expression = parse(&tokenize("-1"));
        assert_eq!(
            expression
                .unwrap()
                .first()
                .expect("Parsing returned nothing!"),
            &ExprKind::Unary {
                operator: "-".to_string(),
                target: ExprKind::Literal {
                    value: "1".to_string()
                }
                .into()
            }
            .into()
        );
    }

    #[test]
    fn test_bool_unary() {
        let expression = parse(&tokenize("not true"));
        assert_eq!(
            expression
                .unwrap()
                .first()
                .expect("Parsing returned nothing!"),
            &ExprKind::Unary {
                operator: "not".to_string(),
                target: ExprKind::Literal {
                    value: "true".to_string()
                }
                .into()
            }
            .into()
        );
    }

    #[test]
    fn test_unary_bool_binop() {
        let expression = parse(&tokenize("true != not true"));
        assert_eq!(
            expression
                .unwrap()
                .first()
                .expect("Parsing returned nothing!"),
            &ExprKind::BinaryOperation {
                left: ExprKind::Literal {
                    value: "true".to_string()
                }
                .into(),
                operation: "!=".to_string(),
                right: ExprKind::Unary {
                    operator: "not".to_string(),
                    target: ExprKind::Literal {
                        value: "true".to_string()
                    }
                    .into()
                }
                .into()
            }
            .into()
        );

        let expression2 = parse(&tokenize("not true != true"));
        assert_eq!(
            expression2
                .unwrap()
                .first()
                .expect("Parsing returned nothing!"),
            &ExprKind::BinaryOperation {
                left: ExprKind::Unary {
                    operator: "not".to_string(),
                    target: ExprKind::Literal {
                        value: "true".to_string()
                    }
                    .into()
                }
                .into(),
                operation: "!=".to_string(),
                right: ExprKind::Literal {
                    value: "true".to_string()
                }
                .into()
            }
            .into()
        );
    }

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
                    .into(),]
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
                    identifier: "a".to_string(),
                    annotated_type: None,
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
    fn test_typed_var_declaration() {
        let expression = parse(&tokenize("var a: Int = 10"));

        assert_eq!(
            expression
                .unwrap()
                .first()
                .expect("Should be Some, else test should fail"),
            &ExprKind::BinaryOperation {
                left: ExprKind::VarDeclaration {
                    identifier: "a".to_string(),
                    annotated_type: Some("Int".to_string()),
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
        dbg!(&expression);
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
        let block = parse(&tokenize("1; 2;")).unwrap();
        let exprs = block.content.expressions().expect("Block has no content!");
        assert_eq!(exprs.len(), 4); // 3rd and 4th are None caused by the `;`
        assert_eq!(
            exprs[0],
            ExprKind::Literal {
                value: "1".to_string()
            }
            .into()
        );
        assert_eq!(
            exprs[2],
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
