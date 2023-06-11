use std::iter::Peekable;

use crate::ast::{Ast, Error};
use crate::lexer::{Lexer, SpanType};

const EQ: &str = "eq";
const GT: &str = "gt";
const GTE: &str = "gte";
const LT: &str = "lt";
const LTE: &str = "lte";
const NEQ: &str = "neq";
const LIKE: &str = "like";
const ILIKE: &str = "ilike";
const MATCH: &str = "match";
const IMATCH: &str = "imatch";
const IN: &str = "in";
const IS: &str = "is";
const ISDISTINCT: &str = "isdistinct";
const FTS: &str = "fts";
const PLFTS: &str = "plfts";
const PHFTS: &str = "phfts";
const WFTS: &str = "wfts";
const CS: &str = "cs";
const CD: &str = "cd";
const OV: &str = "ov";
const SL: &str = "sl";
const SR: &str = "sr";
const NXR: &str = "nxr";
const NXL: &str = "nxl";
const ADJ: &str = "adj";
const NOT: &str = "not";
const OR: &str = "or";
const AND: &str = "and";
const ALL: &str = "all";
const ANY: &str = "any";

pub type Value = String;

#[derive(Debug, PartialEq, Clone)]
pub enum Filter {
    And(Vec<InnerFilter>),
    Or(Vec<InnerFilter>),
    One(InnerFilter),
    Not(Box<Filter>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct InnerFilter {
    path: Path,
    operator: Operator,
    value: Value,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Path {
    Leaf(String),
    Nested(String, Box<Path>),
}

impl Path {
    fn append(&mut self, value: &str) {
        match self {
            Path::Leaf(field) => {
                *self = Path::Nested(field.to_string(), Box::new(Path::Leaf(value.to_string())));
            }
            Path::Nested(_field, inner) => inner.append(value),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Operator {
    Equal,
    GreaterThan,
    GreaterThanEqual,
    LessThan,
    LessThanEqual,
    NotEqual,
    Like,
    ILike,
    Match,
    IMatch,
    In,
    Is,
    IsDistinct,
    FullTextSearch,
    PLFT,
    PHFT,
    WebFullTextSearch,
    Contains,
    Contained,
    Overlap,
    StrictlyLeft,
    StrictlyRight,
    NotExtentToTheRight,
    NotExtentToTheLeft,
    Adjacent,
    Not,
    Or,
    And,
    All,
    Any,
}

impl Ast {
    pub(crate) fn parse_filter<T>(
        field: &str,
        input: &str,
        tokens: &mut Peekable<Lexer<T>>,
    ) -> Result<Filter, Error>
    where
        T: Iterator<Item = char>,
    {
        match field {
            OR => {
                return Err(Error::OperatorNotImplemented {
                    found: OR.to_string(),
                    range: 0..0,
                })
            }
            AND => {
                return Err(Error::OperatorNotImplemented {
                    found: AND.to_string(),
                    range: 0..0,
                })
            }
            _ => {
                // Filter::One(InnerFilter { path: (), operator: (), value: () })
                let mut not = false;
                let mut operator = None;
                let mut value = None;
                let mut path = Path::Leaf(field.to_string());
                let mut path_closed = false;

                while let Some(token) = tokens.next() {
                    match token.span_type {
                        SpanType::PathSeparator => {}
                        SpanType::String if operator.is_some() => {
                            value = Some(input[token.range.clone()].to_string());
                        }
                        SpanType::String if path_closed => match &input[token.range.clone()] {
                            GTE => {
                                operator = Some(Operator::GreaterThan);
                            }
                            LT => {
                                operator = Some(Operator::LessThan);
                            }
                            EQ => {
                                operator = Some(Operator::Equal);
                            }

                            operator => {
                                return Err(Error::OperatorNotImplemented {
                                    found: operator.to_string(),
                                    range: token.range.clone(),
                                })
                            }
                        },
                        SpanType::String if !path_closed => {
                            path.append(&input[token.range.clone()]);
                        }
                        SpanType::Equal => path_closed = true,
                        SpanType::And => break,
                        _ => {
                            return Err(Error::InvalidToken {
                                expected: SpanType::PathSeparator,
                                found: token.span_type,
                                range: token.range,
                            })
                        }
                    }
                }

                return Ok(Filter::One(InnerFilter {
                    path,
                    operator: operator.unwrap(),
                    value: value.unwrap(),
                }));
            }
        };
    }
}

#[test]
fn simple_filter() {
    let input = "age=gte.18";
    let lexer = Lexer::new(input.chars());
    let expected = Ast {
        filter: vec![Filter::One(InnerFilter {
            path: Path::Leaf("age".to_string()),
            operator: Operator::GreaterThan,
            value: "18".to_string(),
        })],
        ..Default::default()
    };
    let out = Ast::from_lexer(input, lexer).unwrap();

    assert_eq!(expected, out);
}

#[test]
fn multiple_simple_filter() {
    let input = "age=gte.18&age=lt.100";
    let lexer = Lexer::new(input.chars());
    let expected = Ast {
        filter: vec![
            Filter::One(InnerFilter {
                path: Path::Leaf("age".to_string()),
                operator: Operator::GreaterThan,
                value: "18".to_string(),
            }),
            Filter::One(InnerFilter {
                path: Path::Leaf("age".to_string()),
                operator: Operator::LessThan,
                value: "100".to_string(),
            }),
        ],
        ..Default::default()
    };
    let out = Ast::from_lexer(input, lexer).unwrap();

    assert_eq!(expected, out);
}

#[test]
fn nested_path_filter() {
    let input = "organization.projects.tasks.name=eq.Test";
    let lexer = Lexer::new(input.chars());
    let expected = Ast {
        filter: vec![Filter::One(InnerFilter {
            path: Path::Nested(
                "organization".to_string(),
                Box::new(Path::Nested(
                    "projects".to_string(),
                    Box::new(Path::Nested(
                        "tasks".to_string(),
                        Box::new(Path::Leaf("name".to_string())),
                    )),
                )),
            ),
            operator: Operator::Equal,
            value: "Test".to_string(),
        })],
        ..Default::default()
    };
    let out = Ast::from_lexer(input, lexer).unwrap();

    assert_eq!(expected, out);
}
