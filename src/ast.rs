use std::fmt::Debug;
use std::iter::Peekable;

use crate::lexer::{Lexer, Span, SpanType};
use crate::Error;

pub mod filter;
pub mod order;
pub mod select;

pub use filter::Filter;
pub use filter::InnerFilter;
pub use filter::Path as FilterPath;
pub use order::Order;
pub use order::OrderItem;
pub use select::Select;
pub use select::{Field, FieldKey};

// keywords
pub const SELECT: &str = "select";
pub const ORDER: &str = "order";
pub const LIMIT: &str = "limit";
pub const OFFSET: &str = "offset";
/// list of the constants in this module
pub const RESERVED_KEYWORDS: [&str; 4] = [SELECT, ORDER, LIMIT, OFFSET];

#[derive(Debug, PartialEq, Clone, Default)]
pub struct Ast {
    pub select: Option<Select>,
    pub order: Option<Order>,
    pub offset: Option<usize>,
    pub limit: Option<usize>,
    pub filter: Vec<Filter>,
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
            if token.span_type == SpanType::String && LIMIT == &input[token.range.clone()] {
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
            if token.span_type == SpanType::String
                && !RESERVED_KEYWORDS.contains(&&input[token.range.clone()])
            {
                match peekable_tokens.peek() {
                    Some(Span {
                        span_type: SpanType::Equal,
                        ..
                    }) => (),
                    Some(Span {
                        span_type: SpanType::PathSeparator,
                        ..
                    }) => (),
                    Some(Span {
                        span_type: found,
                        range,
                    }) => return Err(Error::invalid_token(SpanType::Equal, *found, range.clone())),
                    None => return Err(Error::UnexpectedEnd),
                }
                let field = &input[token.range];
                ast.filter
                    .push(Self::parse_filter(field, input, &mut peekable_tokens)?);
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

#[test]
fn simple_combined_query() {
    let input = "id=gte.14&order=id.asc&select=id&id=lt.54";
    let lexer = Lexer::new(input.chars());
    let expected = Ast {
        select: Some(Select {
            fields: vec![Field::Key(FieldKey {
                column: "id".to_string(),
                alias: None,
            })],
        }),
        order: Some(Order {
            fields: vec![OrderItem {
                field: "id".to_string(),
                operator: order::Operator::Asc,
                nulls_position: None,
            }],
        }),
        filter: vec![
            Filter::One(InnerFilter {
                path: FilterPath::Leaf("id".to_string()),
                operator: filter::Operator::GreaterThanEqual,
                value: "14".to_string(),
            }),
            Filter::One(InnerFilter {
                path: FilterPath::Leaf("id".to_string()),
                operator: filter::Operator::LessThan,
                value: "54".to_string(),
            }),
        ],
        ..Default::default()
    };
    let out = Ast::from_lexer(input, lexer).unwrap();

    assert_eq!(expected, out);
}
