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
    Block {
        expressions: Vec<Expression>,
    },
}

impl Expression {
    pub fn push(&mut self, expression: Expression) {
        if let Expression::Block { expressions } = self {
            expressions.push(expression)
        }
    }

    pub fn get_first(&self) -> Option<&Expression> {
        match self {
            Expression::Block { expressions } => expressions.first(),
            _ => None,
        }
    }

    pub fn expressions(&self) -> Option<&Vec<Expression>> {
        match self {
            Expression::Block { expressions } => Some(expressions),
            _ => None,
        }
    }
}
