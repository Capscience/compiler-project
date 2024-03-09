#[derive(Debug, PartialEq)]
pub struct Expr {
    pub content: ExprKind,
    pub type_: Type,
}

impl Expr {
    pub fn new(content: ExprKind, type_: Option<Type>) -> Self {
        if let Some(expr_type) = type_ {
            Self {
                content,
                type_: expr_type,
            }
        } else {
            Self {
                content,
                type_: Type::None,
            }
        }
    }

    pub fn first(&self) -> Result<&Expr, String> {
        if matches!(self.content, ExprKind::Block { .. }) {
            self.content.get_first().ok_or("Empty block!".to_string())
        } else {
            Err("Tried calling .first() on an Expr that is not ExprKind::Block!".to_string())
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Type {
    Int,
    Bool,
    None,
}

impl Default for Type {
    fn default() -> Self {
        Self::None
    }
}

#[derive(Debug, PartialEq)]
pub enum ExprKind {
    Literal {
        value: String,
    },
    Identifier {
        value: String,
    },
    BinaryOperation {
        left: Box<Expr>,
        operation: String,
        right: Box<Expr>,
    },
    IfClause {
        condition: Box<Expr>,
        if_block: Box<Expr>,
        else_block: Option<Box<Expr>>,
    },
    Block {
        expressions: Vec<Expr>,
    },
    VarDeclaration {
        identifier: String,
    },
    Unary {
        operator: String,
        target: Box<Expr>,
    },
    WhileDo {
        condition: Box<Expr>,
        do_block: Box<Expr>,
    },
    None,
}

impl std::convert::From<ExprKind> for Box<Expr> {
    fn from(value: ExprKind) -> Self {
        Box::new(Expr::new(value, None))
    }
}

impl std::convert::From<ExprKind> for Expr {
    fn from(value: ExprKind) -> Self {
        Expr::new(value, None)
    }
}

impl ExprKind {
    pub fn push(&mut self, expression: Expr) {
        if let ExprKind::Block { expressions } = self {
            expressions.push(expression)
        }
    }

    pub fn get_first(&self) -> Option<&Expr> {
        match self {
            ExprKind::Block { expressions } => expressions.first(),
            _ => None,
        }
    }

    pub fn expressions(&self) -> Option<&Vec<Expr>> {
        match self {
            ExprKind::Block { expressions } => Some(expressions),
            _ => None,
        }
    }
}
