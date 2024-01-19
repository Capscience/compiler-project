use std::fmt;
use std::ops::Range;

use regex::Regex;
use strum::IntoEnumIterator;
use strum_macros::EnumIter;

#[derive(Debug)]
pub struct Token {
    text: &'static str,
    tokentype: TokenType,
    range: Option<Range<usize>>,
}

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
#[derive(Debug, PartialEq, EnumIter)]
pub enum TokenType {
    Comment,
    WhiteSpace,
    Identifier,
    IntLiteral,
    Operator,
    Delimiter,
}

impl TokenType {
    pub fn pattern(&self) -> Regex {
        match self {
            Self::Identifier => Regex::new(r"^[a-zA-Z_]+[\w_-]*").unwrap(),
            Self::IntLiteral => Regex::new(r"^[0-9]+").unwrap(),
            Self::Operator => Regex::new(r"^(<=|==|>=|[+-=*/])").unwrap(),
            Self::Delimiter => Regex::new(r"^(,|;|\(|\)|\{|\})").unwrap(),
            Self::WhiteSpace => Regex::new(r"^\s+").unwrap(),
            Self::Comment => Regex::new(r"^(//.*)|^/\*(.|\n)*?\*/").unwrap(),
        }
    }

    pub fn ignore(&self) -> bool {
        match self {
            Self::Comment => true,
            Self::WhiteSpace => true,
            _ => false,
        }
    }
}

pub fn tokenize(source_code: &'static str) -> Vec<Token> {
    let mut tokens: Vec<Token> = Vec::new();
    let mut position: usize = 0;

    while position < source_code.len() {
        for variant in TokenType::iter() {
            if let Some(re_match) = variant.pattern().find(&source_code[position..]) {
                if !variant.ignore() {
                    tokens.push(Token {
                        text: &source_code
                            [(position + re_match.start())..(position + re_match.end())],
                        tokentype: variant,
                        range: Some((position + re_match.start())..(position + re_match.end())),
                    });
                }
                position += re_match.end();
                break;
            }
        }
    }
    return tokens;
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Helper function for creating indentifiers
    fn identifier(text: &'static str) -> Token {
        Token {
            text,
            tokentype: TokenType::Identifier,
            range: None,
        }
    }

    /// Helper function for creating delimiters
    fn delimiter(text: &'static str) -> Token {
        Token {
            text,
            tokentype: TokenType::Delimiter,
            range: None,
        }
    }

    /// Helper function for creating operators
    fn operator(text: &'static str) -> Token {
        Token {
            text,
            tokentype: TokenType::Operator,
            range: None,
        }
    }

    /// Helper function for creating intliterals
    fn intliteral(text: &'static str) -> Token {
        Token {
            text,
            tokentype: TokenType::IntLiteral,
            range: None,
        }
    }

    #[test]
    fn test_tokenize() {
        assert_eq!(
            tokenize("if a <= bee then print_int(123)"),
            vec![
                identifier("if"),
                identifier("a"),
                operator("<="),
                identifier("bee"),
                identifier("then"),
                identifier("print_int"),
                delimiter("("),
                intliteral("123"),
                delimiter(")"),
            ]
        );
    }

    #[test]
    fn test_comment() {
        assert_eq!(tokenize("// this is a comment"), vec![]);
    }

    #[test]
    fn test_multiline_comment() {
        assert_eq!(
            tokenize(
                "/* this
        is 
        a 
        multiline
        comment */"
            ),
            vec![]
        );
    }
}
