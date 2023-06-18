use crate::lexer::SpanType;
use std::ops::Range;

#[derive(thiserror::Error, Debug, PartialEq)]
#[non_exhaustive]
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
    #[error("Operator not implemented (yet), operator: {found:?}")]
    OperatorNotImplemented { found: String, range: Range<usize> },
    #[error("Invalid not order, not should come before the operator")]
    InvalidNotOrdering { range: Range<usize> },
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
