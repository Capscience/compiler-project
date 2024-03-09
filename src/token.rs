use regex::Regex;
use std::fmt;
use std::ops::Range;
use strum_macros::EnumIter;

#[derive(Debug, Clone)]
pub struct Token {
    pub text: String,
    pub tokentype: TokenType,
    pub range: Option<Range<usize>>,
}

/// Simpler printing format for tokens.
impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.text)
    }
}

/// Custom PartialEq for easier testing.
impl PartialEq for Token {
    /// Token ranges are not compared, if either of them is none.
    fn eq(&self, other: &Self) -> bool {
        if self.range.is_none() || other.range.is_none() {
            self.text == other.text && self.tokentype == other.tokentype
        } else {
            self.text == other.text
                && self.tokentype == other.tokentype
                && self.range == other.range
        }
    }
}

/// The order of these variants matters!
/// For example, comment must be before operators, othervise // will be interpreted
/// as 2 divisions instead of a comment.
#[derive(Debug, PartialEq, EnumIter, Clone)]
pub enum TokenType {
    Comment,
    WhiteSpace,
    Operator,
    Identifier,
    IntLiteral,
    Delimiter,
}

impl TokenType {
    /// Get regex pattern for the specific TokenType.
    pub fn pattern(&self) -> Regex {
        match self {
            Self::Identifier => Regex::new(r"^[a-zA-Z_]+[\w_-]*").unwrap(),
            Self::IntLiteral => Regex::new(r"^[0-9]+").unwrap(),
            Self::Operator => Regex::new(r"^(and\b|or\b|not\b|<=|==|>=|!=|[><+\-=*/%])").unwrap(),
            Self::Delimiter => Regex::new(r"^[,;}{)(]").unwrap(),
            Self::WhiteSpace => Regex::new(r"^\s+").unwrap(),
            Self::Comment => Regex::new(r"^(//.*)|^/\*(.|\n)*?\*/").unwrap(),
        }
    }

    /// Returns whether a specific TokenType should be ignored in tokenization.
    pub fn ignore(&self) -> bool {
        matches!(self, Self::Comment | Self::WhiteSpace)
    }
}
