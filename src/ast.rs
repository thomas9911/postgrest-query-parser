use std::ops::Range;

use crate::lexer::{Lexer, Span, SpanType};

pub mod select;
use select::Select;

// keywords
pub const SELECT: &str = "select";

#[derive(thiserror::Error, Debug)]
pub enum Error {
    #[error("Expected {expected:?}, found {found:?}")]
    InvalidToken {
        expected: SpanType,
        found: SpanType,
        range: Range<usize>,
    },
    #[error("Reached unexpected end of input")]
    UnexpectedEnd,
}

impl Error {
    pub fn invalid_token(expected: SpanType, found: SpanType, range: Range<usize>) -> Error {
        Error::InvalidToken {
            expected,
            found,
            range,
        }
    }
}

#[derive(Debug, PartialEq, Clone, Default)]
pub struct Ast {
    pub select: Option<Select>,
}

impl Ast {
    pub fn from_lexer<T>(input: &str, tokens: Lexer<T>) -> Result<Ast, Error>
    where
        T: Iterator<Item = char>,
    {
        let mut ast = Ast::default();

        let mut peekable_tokens = tokens.peekable();
        while let Some(token) = peekable_tokens.next() {
            if token.span_type == SpanType::String {
                if SELECT == &input[token.range] {
                    match peekable_tokens.next() {
                        Some(Span {
                            span_type: SpanType::Equal,
                            ..
                        }) => (),
                        Some(Span {
                            span_type: found,
                            range,
                        }) => return Err(Error::invalid_token(SpanType::Equal, found, range)),
                        None => return Err(Error::UnexpectedEnd),
                    }
                    ast.select = Some(Self::parse_select(input, &mut peekable_tokens)?);
                }
            }
        }

        Ok(ast)
    }
}