use std::iter::Peekable;

use crate::ast::{Ast, Error};
use crate::lexer::{Lexer, SpanType};

// wauw this is a long line with a lot of text ect. This mean that this will break to the next line hopefully because of rustfmt config

pub mod consts {
    pub const EQ: &str = "eq";
    pub const GT: &str = "gt";
    pub const GTE: &str = "gte";
    pub const LT: &str = "lt";
    pub const LTE: &str = "lte";
    pub const NEQ: &str = "neq";
    pub const LIKE: &str = "like";
    pub const ILIKE: &str = "ilike";
    pub const MATCH: &str = "match";
    pub const IMATCH: &str = "imatch";
    pub const IN: &str = "in";
    pub const IS: &str = "is";
    pub const ISDISTINCT: &str = "isdistinct";
    pub const FTS: &str = "fts";
    pub const PLFTS: &str = "plfts";
    pub const PHFTS: &str = "phfts";
    pub const WFTS: &str = "wfts";
    pub const CS: &str = "cs";
    pub const CD: &str = "cd";
    pub const OV: &str = "ov";
    pub const SL: &str = "sl";
    pub const SR: &str = "sr";
    pub const NXR: &str = "nxr";
    pub const NXL: &str = "nxl";
    pub const ADJ: &str = "adj";
    pub const NOT: &str = "not";
    pub const OR: &str = "or";
    pub const AND: &str = "and";
    pub const ALL: &str = "all";
    pub const ANY: &str = "any";
}

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
    pub path: Path,
    pub operator: Operator,
    pub value: Value,
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
            consts::OR => {
                return Err(Error::OperatorNotImplemented {
                    found: consts::OR.to_string(),
                    range: 0..0,
                })
            }
            consts::AND => {
                return Err(Error::OperatorNotImplemented {
                    found: consts::AND.to_string(),
                    range: 0..0,
                })
            }
            _ => {
                let mut not = false;
                let mut operator = None;
                let mut value = None;
                let mut path = Path::Leaf(field.to_string());
                let mut path_closed = false;

                while let Some(token) = tokens.next() {
                    match token.span_type {
                        SpanType::PathSeparator => {}
                        SpanType::String if operator.is_some() => {
                            match &input[token.range.clone()] {
                                consts::NOT => {
                                    return Err(Error::InvalidNotOrdering {
                                        range: token.range.clone(),
                                    })
                                }
                                path_value => {
                                    value = Some(path_value.to_string());
                                }
                            }
                        }
                        SpanType::String if path_closed => match &input[token.range.clone()] {
                            consts::EQ => {
                                operator = Some(Operator::Equal);
                            }
                            consts::NEQ => {
                                operator = Some(Operator::NotEqual);
                            }
                            consts::GT => {
                                operator = Some(Operator::GreaterThan);
                            }
                            consts::GTE => {
                                operator = Some(Operator::GreaterThanEqual);
                            }
                            consts::LT => {
                                operator = Some(Operator::LessThan);
                            }
                            consts::LTE => {
                                operator = Some(Operator::LessThanEqual);
                            }
                            consts::NOT => {
                                not = true;
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

                let filter = Filter::One(InnerFilter {
                    path,
                    operator: operator.unwrap(),
                    value: value.unwrap(),
                });

                if not {
                    return Ok(Filter::Not(Box::new(filter)));
                } else {
                    return Ok(filter);
                }
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
            operator: Operator::GreaterThanEqual,
            value: "18".to_string(),
        })],
        ..Default::default()
    };
    let out = Ast::from_lexer(input, lexer).unwrap();

    assert_eq!(expected, out);
}

#[test]
fn simple_not_filter() {
    let input = "age=not.gte.18";
    let lexer = Lexer::new(input.chars());
    let expected = Ast {
        filter: vec![Filter::Not(Box::new(Filter::One(InnerFilter {
            path: Path::Leaf("age".to_string()),
            operator: Operator::GreaterThanEqual,
            value: "18".to_string(),
        })))],
        ..Default::default()
    };
    let out = Ast::from_lexer(input, lexer).unwrap();

    assert_eq!(expected, out);
}

#[test]
fn invalid_not_filter() {
    let input = "age=gte.not.18";
    let lexer = Lexer::new(input.chars());
    let expected = Error::InvalidNotOrdering { range: 8..11 };
    let out = Ast::from_lexer(input, lexer).unwrap_err();

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
                operator: Operator::GreaterThanEqual,
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
