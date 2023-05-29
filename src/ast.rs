use std::fmt::Debug;
use std::iter::Peekable;
use std::ops::Range;

use crate::lexer::{Lexer, Span, SpanType};

pub mod order;
pub mod select;
use order::Order;
use select::Select;

// keywords
pub const SELECT: &str = "select";
pub const ORDER: &str = "order";
pub const LIMIT: &str = "limit";
pub const OFFSET: &str = "offset";

#[derive(thiserror::Error, Debug, PartialEq)]
pub enum Error {
    #[error("Expected {expected:?}, found {found:?}")]
    InvalidToken {
        expected: SpanType,
        found: SpanType,
        range: Range<usize>,
    },
    #[error("Reached unexpected end of input")]
    UnexpectedEnd,
    #[error("Encountered unclosed bracket")]
    UnclosedBracket { range: Range<usize> },
    #[error("Trailing comma found")]
    TrailingComma { range: Range<usize> },
    #[error("No fields specified")]
    MissingFields { range: Range<usize> },
    #[error("Invalid nested fields")]
    InvalidNesting { range: Range<usize> },
    #[error("Invalid order direction found: {found:?}")]
    InvalidOrderDirection { found: String, range: Range<usize> },
    #[error("Invalid integer found: {found:?}")]
    InvalidInteger { found: String, range: Range<usize> },
}

impl Error {
    #[must_use]
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
    pub order: Option<Order>,
    pub offset: Option<usize>,
    pub limit: Option<usize>,
}

impl Ast {
    /// # Errors
    /// Returns an error if input input + tokens are invalid
    pub fn from_lexer<T>(input: &str, tokens: Lexer<T>) -> Result<Ast, Error>
    where
        T: Iterator<Item = char>,
    {
        let mut ast = Ast::default();

        let mut peekable_tokens = tokens.peekable();
        while let Some(token) = peekable_tokens.next() {
            if token.span_type == SpanType::String && SELECT == &input[token.range.clone()] {
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
                ast.select = Some(Self::parse_select(input, &mut peekable_tokens, 0)?);
            }
            if token.span_type == SpanType::String && ORDER == &input[token.range.clone()] {
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
                ast.order = Some(Self::parse_order(input, &mut peekable_tokens)?);
            }
            if token.span_type == SpanType::String && OFFSET == &input[token.range.clone()] {
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
                ast.offset = Some(Self::parse_integer(input, &mut peekable_tokens)?);
            }
            if token.span_type == SpanType::String && LIMIT == &input[token.range] {
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
                ast.limit = Some(Self::parse_integer(input, &mut peekable_tokens)?);
            }
        }

        Ok(ast)
    }

    pub(crate) fn parse_integer<T>(
        input: &str,
        tokens: &mut Peekable<Lexer<T>>,
    ) -> Result<usize, Error>
    where
        T: Iterator<Item = char>,
    {
        match tokens.next() {
            Some(token) if token.span_type == SpanType::String => {
                let data = &input[token.range.clone()];
                if let Ok(integer) = data.parse::<usize>() {
                    return Ok(integer);
                } else {
                    return Err(Error::InvalidInteger {
                        found: data.to_string(),
                        range: token.range,
                    });
                }
            }
            Some(token) => {
                return Err(Error::invalid_token(
                    SpanType::String,
                    token.span_type,
                    token.range,
                ));
            }
            None => return Err(Error::UnexpectedEnd),
        }
    }
}

#[test]
fn simple_limit() {
    let input = "limit=150";
    let lexer = Lexer::new(input.chars());
    let expected = Ast {
        limit: Some(150),
        ..Default::default()
    };
    let out = Ast::from_lexer(input, lexer).unwrap();

    assert_eq!(expected, out);
}

#[test]
fn simple_offset() {
    let input = "offset=1000";
    let lexer = Lexer::new(input.chars());
    let expected = Ast {
        offset: Some(1000),
        ..Default::default()
    };
    let out = Ast::from_lexer(input, lexer).unwrap();

    assert_eq!(expected, out);
}

#[test]
fn offset_and_limit() {
    let input = "limit=512&offset=9321";
    let lexer = Lexer::new(input.chars());
    let expected = Ast {
        limit: Some(512),
        offset: Some(9321),
        ..Default::default()
    };
    let out = Ast::from_lexer(input, lexer).unwrap();

    assert_eq!(expected, out);
}
