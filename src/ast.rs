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
}
