#[derive(Debug, PartialEq)]
pub enum Expression {
    Literal {
        value: String,
    },
    Identifier {
        value: String,
    },
    BinaryOperation {
        left: Box<Expression>,
        operation: String,
        right: Box<Expression>,
    },
    IfClause {
        condition: Box<Expression>,
        if_block: Box<Expression>,
        else_block: Option<Box<Expression>>,
    },
}
