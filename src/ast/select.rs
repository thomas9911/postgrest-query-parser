use std::iter::Peekable;

use crate::ast::{Ast, Error};
use crate::lexer::{Lexer, Span, SpanType};

impl Ast {
    pub(crate) fn parse_select<T>(
        input: &str,
        tokens: &mut Peekable<Lexer<T>>,
        level: usize,
    ) -> Result<Select, Error>
    where
        T: Iterator<Item = char>,
    {
        let mut select = Select::default();
        let mut previous: Option<Span> = None;
        let mut alias: Option<String> = None;

        while let Some(token) = tokens.next() {
            // dbg!(&token);
            match token.span_type {
                SpanType::String => {
                    if previous.map(|x| x.span_type) == Some(SpanType::String) {
                        return Err(Error::invalid_token(
                            SpanType::Separator,
                            token.span_type,
                            token.range,
                        ));
                    }
                    if let Some(found_alias) = alias {
                        select.fields.push(Field::aliased(
                            input[token.range.clone()].to_string(),
                            found_alias,
                        ));

                        alias = None;
                    } else if ![Some(&SpanType::Alias), Some(&SpanType::CaptureStart)]
                        .contains(&tokens.peek().map(|x| &x.span_type))
                    {
                        select
                            .fields
                            .push(Field::new(input[token.range.clone()].to_string()));
                    }
                }
                SpanType::CaptureStart
                    if previous.as_ref().map(|x| x.span_type) == Some(SpanType::String) =>
                {
                    match select.fields.last() {
                        Some(Field::Key(FieldKey { alias: Some(_), .. })) => {
                            if let Field::Key(previous) = select.fields.pop().unwrap() {
                                let inner_select = Self::parse_select(input, tokens, level + 1)?;
                                select.fields.push(Field::Nested(previous, inner_select));
                            } else {
                                unreachable!()
                            };
                        }
                        Some(Field::Key(_)) => {
                            let inner_select = Self::parse_select(input, tokens, level + 1)?;
                            let previous =
                                previous.expect("previous is always valid because of if statement");
                            select.fields.push(Field::Nested(
                                FieldKey::new(input[previous.range].to_string()),
                                inner_select,
                            ));
                        }
                        Some(Field::Nested(..)) => {
                            return Err(Error::InvalidNesting { range: token.range })
                        }
                        None => (),
                    }
                }
                SpanType::Separator
                    if previous.as_ref().map(|x| x.span_type) != Some(SpanType::String) =>
                {
                    return Err(Error::invalid_token(
                        SpanType::String,
                        token.span_type,
                        token.range,
                    ));
                }
                SpanType::Separator => (),
                SpanType::Alias
                    if previous.as_ref().map(|x| x.span_type) != Some(SpanType::String) =>
                {
                    return Err(Error::invalid_token(
                        SpanType::String,
                        token.span_type,
                        token.range,
                    ));
                }
                SpanType::Alias if previous.is_some() => {
                    alias = Some(input[previous.unwrap().range.clone()].to_string());
                }
                SpanType::Alias => {
                    return Err(Error::invalid_token(
                        SpanType::String,
                        SpanType::Alias,
                        token.range,
                    ));
                }
                SpanType::CaptureEnd if level > 0 && previous.is_none() => {
                    return Err(Error::MissingFields { range: token.range })
                }
                SpanType::CaptureEnd if level > 0 => {
                    break;
                }
                SpanType::CaptureEnd if level == 0 => {
                    return Err(Error::UnclosedBracket { range: token.range })
                }
                found if previous.is_none() => {
                    return Err(Error::invalid_token(SpanType::String, found, token.range))
                }
                found => {
                    return Err(Error::invalid_token(SpanType::Equal, found, token.range));
                }
            }

            previous = Some(token);
        }

        if previous.is_none() {
            Err(Error::UnexpectedEnd)
        } else {
            Ok(select)
        }
    }
}

#[derive(Debug, PartialEq, Clone, Default)]
pub struct Select {
    pub fields: Vec<Field>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Field {
    Key(FieldKey),
    Nested(FieldKey, Select),
}

impl Field {
    pub fn new(column: String) -> Field {
        Field::Key(FieldKey::new(column))
    }

    pub fn aliased(column: String, alias: String) -> Field {
        Field::Key(FieldKey::aliased(column, alias))
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct FieldKey {
    column: String,
    alias: Option<String>,
}

impl FieldKey {
    pub fn new(column: String) -> FieldKey {
        FieldKey {
            column,
            alias: None,
        }
    }

    pub fn aliased(column: String, alias: String) -> FieldKey {
        FieldKey {
            alias: Some(alias),
            column,
        }
    }
}

#[test]
fn simple_select() {
    let input = "select=first_name,age";
    let lexer = Lexer::new(input.chars());
    let expected = Ast {
        select: Some(Select {
            fields: vec![
                Field::Key(FieldKey::new("first_name".to_string())),
                Field::Key(FieldKey::new("age".to_string())),
            ],
        }),
    };
    let out = Ast::from_lexer(input, lexer).unwrap();

    assert_eq!(expected, out);
}

#[test]
fn select_with_alias() {
    let input = "select=firstName:first_name,age";
    let lexer = Lexer::new(input.chars());

    let expected = Ast {
        select: Some(Select {
            fields: vec![
                Field::Key(FieldKey {
                    alias: Some("firstName".to_string()),
                    column: "first_name".to_string(),
                }),
                Field::Key(FieldKey::new("age".to_string())),
            ],
        }),
    };
    let out = Ast::from_lexer(input, lexer).unwrap();

    assert_eq!(expected, out);
}

#[test]
fn nested_select() {
    let input = "select=id,projects(id,tasks(id,name))";
    let lexer = Lexer::new(input.chars());

    let expected = Ast {
        select: Some(Select {
            fields: vec![
                Field::new("id".to_string()),
                Field::Nested(
                    FieldKey::new("projects".to_string()),
                    Select {
                        fields: vec![
                            Field::new("id".to_string()),
                            Field::Nested(
                                FieldKey::new("tasks".to_string()),
                                Select {
                                    fields: vec![
                                        Field::new("id".to_string()),
                                        Field::new("name".to_string()),
                                    ],
                                },
                            ),
                        ],
                    },
                ),
            ],
        }),
    };
    let out = Ast::from_lexer(input, lexer).unwrap();

    assert_eq!(expected, out);
}

#[test]
fn nested_select_with_aliases() {
    let input = "select=id,projectItems:projects(id,tasks(id,name))";
    let lexer = Lexer::new(input.chars());

    let expected = Ast {
        select: Some(Select {
            fields: vec![
                Field::new("id".to_string()),
                Field::Nested(
                    FieldKey::aliased("projects".to_string(), "projectItems".to_string()),
                    Select {
                        fields: vec![
                            Field::new("id".to_string()),
                            Field::Nested(
                                FieldKey::new("tasks".to_string()),
                                Select {
                                    fields: vec![
                                        Field::new("id".to_string()),
                                        Field::new("name".to_string()),
                                    ],
                                },
                            ),
                        ],
                    },
                ),
            ],
        }),
    };
    let out = Ast::from_lexer(input, lexer).unwrap();

    assert_eq!(expected, out);
}

#[test]
fn invalid_selects() {
    let tests = [
        (
            "select=()",
            Error::InvalidToken {
                expected: SpanType::String,
                found: SpanType::CaptureStart,
                range: 7..8,
            },
        ),
        // (
        //     "select=a(()",
        //     Error::InvalidToken {
        //         expected: SpanType::String,
        //         found: SpanType::CaptureStart,
        //         range: 9..10,
        //     },
        // ),
        ("select=)", Error::UnclosedBracket { range: 7..8 }),
        // ("select=a()", Error::MissingFields { range: 9..10 }),
        // ("select=a()()", Error::MissingFields { range: 9..10 }),
        ("select=", Error::UnexpectedEnd),
        // ("select=a:", Error::UnexpectedEnd),
    ];

    for (input, expected) in tests {
        assert_eq!(
            expected,
            Ast::from_lexer(input, Lexer::new(input.chars())).unwrap_err()
        );
    }
}
