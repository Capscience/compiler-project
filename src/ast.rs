#[derive(Debug, PartialEq)]
pub enum Expression {
    Literal {
        value: String,
    },
    BinaryOperation {
        left: Box<Expression>,
        operation: String,
        right: Box<Expression>,
    },
}
