use std::fmt::Debug;
use std::iter::Peekable;
use std::ops::Deref;

use crate::ast::{Ast, Error};
use crate::lexer::{Lexer, Span, SpanType};

#[derive(Debug)]
pub enum JsonPathFormat<T: Debug> {
    Normal(T),
    Binary(T),
}

impl<T: Debug> JsonPathFormat<T> {
    pub fn into_t(self) -> T {
        match self {
            JsonPathFormat::Normal(t) => t,
            JsonPathFormat::Binary(t) => t,
        }
    }
}

impl<T: Debug> Deref for JsonPathFormat<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        match self {
            JsonPathFormat::Normal(t) => t,
            JsonPathFormat::Binary(t) => t,
        }
    }
}

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
        let mut json_path: Option<Vec<JsonPathFormat<Field>>> = None;

        while let Some(token) = tokens.next() {
            dbg!(&token, &select);
            // dbg!((&json_path, &token.span_type, &previous.as_ref().map(|x| x.span_type)));
            // dbg!((&json_path, &token.span_type));
            match token.span_type {
                SpanType::String
                    if previous.as_ref().map(|x| x.span_type) == Some(SpanType::String) =>
                {
                    return Err(Error::invalid_token(
                        SpanType::Separator,
                        token.span_type,
                        token.range,
                    ));
                }
                SpanType::String
                    if [Some(SpanType::Arrow), Some(SpanType::BinaryArrow)]
                        .contains(&previous.as_ref().map(|x| x.span_type)) =>
                {
                    let previous_span_type = previous.as_ref().map(|x| x.span_type).unwrap();

                    if let Some(inner_json_path) = json_path.as_mut() {
                        if let Some(found_alias) = alias {
                            match previous_span_type {
                                SpanType::Arrow => {
                                    inner_json_path.push(JsonPathFormat::Normal(Field::aliased(
                                        input[token.range.clone()].to_string(),
                                        found_alias,
                                    )));
                                }
                                SpanType::BinaryArrow => {
                                    inner_json_path.push(JsonPathFormat::Binary(Field::aliased(
                                        input[token.range.clone()].to_string(),
                                        found_alias,
                                    )));
                                }
                                _ => unreachable!(),
                            }

                            alias = None;
                        } else if ![Some(&SpanType::Alias), Some(&SpanType::CaptureStart)]
                            .contains(&tokens.peek().map(|x| &x.span_type))
                        {
                            // non aliased
                            match previous_span_type {
                                SpanType::Arrow => {
                                    inner_json_path.push(JsonPathFormat::Normal(Field::new(
                                        input[token.range.clone()].to_string(),
                                    )));
                                }
                                SpanType::BinaryArrow => {
                                    inner_json_path.push(JsonPathFormat::Binary(Field::new(
                                        input[token.range.clone()].to_string(),
                                    )));
                                }
                                _ => unreachable!(),
                            }
                        }
                    } else {
                        unreachable!()
                    }

                    if tokens.peek().map(|x| &x.span_type) == Some(&SpanType::Separator) {
                        select.fields.push(json_path_to_field(json_path.unwrap()));
                        json_path = None;
                    }
                }
                SpanType::String => {
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
                        Some(Field::Nested(..))
                        | Some(Field::Json(..))
                        | Some(Field::BinaryJson(..)) => {
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
                SpanType::Arrow | SpanType::BinaryArrow
                    if previous.is_some() && json_path.is_none() =>
                {
                    match select.fields.last() {
                        Some(Field::Key(FieldKey { .. })) => match token.span_type {
                            SpanType::Arrow => {
                                json_path = Some(vec![JsonPathFormat::Normal(
                                    select.fields.pop().unwrap(),
                                )]);
                            }
                            SpanType::BinaryArrow => {
                                json_path = Some(vec![JsonPathFormat::Binary(
                                    select.fields.pop().unwrap(),
                                )]);
                            }
                            _ => unreachable!(),
                        },
                        Some(Field::Nested(..))
                        | Some(Field::Json(..))
                        | Some(Field::BinaryJson(..)) => {
                            return Err(Error::InvalidNesting { range: token.range })
                        }
                        None => unreachable!(),
                    }
                }
                SpanType::Arrow | SpanType::BinaryArrow
                    if previous.is_some() && json_path.is_some() =>
                {
                    if let Some(_) = json_path {
                        match select.fields.last() {
                            Some(Field::Key(FieldKey { .. })) => match token.span_type {
                                SpanType::Arrow | SpanType::BinaryArrow => (),
                                _ => unreachable!(),
                            },
                            Some(Field::Nested(..))
                            | Some(Field::Json(..))
                            | Some(Field::BinaryJson(..)) => {
                                return Err(Error::InvalidNesting { range: token.range })
                            }
                            None => unreachable!(),
                        }
                    }
                }
                SpanType::Arrow | SpanType::BinaryArrow => {
                    return Err(Error::invalid_token(
                        SpanType::String,
                        token.span_type,
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

        if json_path.is_some() {
            select.fields.push(json_path_to_field(json_path.unwrap()))
        }

        if previous.is_none() {
            Err(Error::UnexpectedEnd)
        } else {
            Ok(select)
        }
    }
}

fn json_path_to_field(fields: Vec<JsonPathFormat<Field>>) -> Field {
    debug_assert!(fields.len() > 0);
    let mut field = None;
    for typed_field in fields.into_iter().rev() {
        if let Some(existing_field) = field {
            match typed_field {
                JsonPathFormat::Normal(inner_field) => {
                    field = Some(Field::Json(inner_field.as_key(), Box::new(existing_field)))
                }
                JsonPathFormat::Binary(inner_field) => {
                    field = Some(Field::BinaryJson(
                        inner_field.as_key(),
                        Box::new(existing_field),
                    ))
                }
            }
        } else {
            field = Some(typed_field.into_t());
        }
    }

    field.expect("fields is never zero sized")
}

#[derive(Debug, PartialEq, Clone, Default)]
pub struct Select {
    pub fields: Vec<Field>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Field {
    Key(FieldKey),
    Nested(FieldKey, Select),
    Json(FieldKey, Box<Field>),
    BinaryJson(FieldKey, Box<Field>),
}

impl Field {
    pub fn new(column: String) -> Field {
        Field::Key(FieldKey::new(column))
    }

    pub fn aliased(column: String, alias: String) -> Field {
        Field::Key(FieldKey::aliased(column, alias))
    }

    pub fn as_key(self) -> FieldKey {
        match self {
            Field::Key(key) => key,
            Field::Nested(key, _) => key,
            Field::Json(key, _) => key,
            Field::BinaryJson(key, _) => key,
        }
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
fn select_with_json() {
    let input = "select=id,json_data->age";
    let lexer = Lexer::new(input.chars());

    let expected = Ast {
        select: Some(Select {
            fields: vec![
                Field::new("id".to_string()),
                Field::Json(
                    FieldKey::new("json_data".to_string()),
                    Box::new(Field::new("age".to_string())),
                ),
            ],
        }),
    };
    let out = Ast::from_lexer(input, lexer).unwrap();

    assert_eq!(expected, out);
}

#[test]
fn select_with_binary_json_bug() {
    let input = "select=location->>lat,id";
    let lexer = Lexer::new(input.chars());

    let expected = Ast {
        select: Some(Select {
            fields: vec![
                Field::BinaryJson(
                    FieldKey::new("location".to_string()),
                    Box::new(Field::new("lat".to_string())),
                ),
                Field::Key(FieldKey {
                    column: "id".to_string(),
                    alias: None,
                }),
            ],
        }),
    };
    let out = Ast::from_lexer(input, lexer).unwrap();

    assert_eq!(expected, out);
}

#[test]
fn select_with_multiple_json() {
    let input = "select=id,location->>lat,location->>long,primary_language:languages->0";
    let lexer = Lexer::new(input.chars());

    let expected = Ast {
        select: Some(Select {
            fields: vec![
                Field::Key(FieldKey {
                    column: "id".to_string(),
                    alias: None,
                }),
                Field::BinaryJson(
                    FieldKey::new("location".to_string()),
                    Box::new(Field::new("lat".to_string())),
                ),
                Field::BinaryJson(
                    FieldKey::new("location".to_string()),
                    Box::new(Field::new("long".to_string())),
                ),
                Field::Json(
                    FieldKey {
                        column: "languages".to_string(),
                        alias: Some("primary_language".to_string()),
                    },
                    Box::new(Field::new("0".to_string())),
                ),
            ],
        }),
    };
    let out = Ast::from_lexer(input, lexer).unwrap();

    assert_eq!(expected, out);
}

#[test]
fn select_with_nested_json() {
    let input = "select=id,forums->0->posts->0->comment->>user->name";
    let lexer = Lexer::new(input.chars());

    let expected = Ast {
        select: Some(Select {
            fields: vec![
                Field::new("id".to_string()),
                Field::Json(
                    FieldKey::new("forums".to_string()),
                    Box::new(Field::Json(
                        FieldKey::new("0".to_string()),
                        Box::new(Field::Json(
                            FieldKey::new("posts".to_string()),
                            Box::new(Field::Json(
                                FieldKey::new("0".to_string()),
                                Box::new(Field::Json(
                                    FieldKey::new("comment".to_string()),
                                    Box::new(Field::BinaryJson(
                                        FieldKey::new("user".to_string()),
                                        Box::new(Field::new("name".to_string())),
                                    )),
                                )),
                            )),
                        )),
                    )),
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
