use strum::IntoEnumIterator;

pub use crate::token::{Token, TokenType};

pub fn tokenize(source_code: &'static str) -> Vec<Token> {
    let mut tokens: Vec<Token> = Vec::new();
    let mut position: usize = 0;

    while position < source_code.len() {
        let mut match_found = false;

        for variant in TokenType::iter() {
            if let Some(re_match) = variant.pattern().find(&source_code[position..]) {
                match_found = true;
                if !variant.ignore() {
                    tokens.push(Token {
                        text: source_code
                            [(position + re_match.start())..(position + re_match.end())]
                            .to_string(),
                        tokentype: variant,
                        range: Some((position + re_match.start())..(position + re_match.end())),
                    });
                }
                position += re_match.end();
                break;
            }
        }
        if !match_found {
            panic!(
                "Error at {}: {}",
                position,
                &source_code.to_string()[position..(position + 1)]
            )
        }
    }
    tokens
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Helper function for creating indentifiers
    fn identifier(text: &str) -> Token {
        Token {
            text: text.to_string(),
            tokentype: TokenType::Identifier,
            range: None,
        }
    }

    /// Helper function for creating delimiters
    fn delimiter(text: &'static str) -> Token {
        Token {
            text: text.to_string(),
            tokentype: TokenType::Delimiter,
            range: None,
        }
    }

    /// Helper function for creating operators
    fn operator(text: &'static str) -> Token {
        Token {
            text: text.to_string(),
            tokentype: TokenType::Operator,
            range: None,
        }
    }

    /// Helper function for creating intliterals
    fn intliteral(text: &'static str) -> Token {
        Token {
            text: text.to_string(),
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
        assert_eq!(
            tokenize(
                "// this is a comment
        test"
            ),
            vec![identifier("test")]
        );
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

    #[test]
    fn test_and_operator() {
        assert_eq!(
            tokenize("foo and bar"),
            vec![identifier("foo"), operator("and"), identifier("bar")]
        );
        assert_eq!(
            tokenize("anderson = 0"),
            vec![identifier("anderson"), operator("="), intliteral("0")]
        );
    }

    #[test]
    fn test_or_operator() {
        assert_eq!(
            tokenize("foo or bar"),
            vec![identifier("foo"), operator("or"), identifier("bar")]
        );
        assert_eq!(
            tokenize("ore = 0"),
            vec![identifier("ore"), operator("="), intliteral("0")]
        );
        assert_eq!(
            tokenize("ore=0"),
            vec![identifier("ore"), operator("="), intliteral("0")]
        );
    }
}
