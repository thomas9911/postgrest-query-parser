use std::iter::Peekable;

use crate::ast::{Ast, Error};
use crate::lexer::{Lexer, Span, SpanType};

impl Ast {
    pub(crate) fn parse_select<T>(
        input: &str,
        tokens: &mut Peekable<Lexer<T>>,
    ) -> Result<Select, Error>
    where
        T: Iterator<Item = char>,
    {
        let mut select = Select::default();
        let mut previous: Option<Span> = None;
        let mut alias: Option<String> = None;

        while let Some(token) = tokens.next() {
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
                        select.fields.push(Field::Aliased {
                            alias: found_alias,
                            column: input[token.range.clone()].to_string(),
                        });

                        alias = None;
                    } else {
                        if [Some(&SpanType::Alias), Some(&SpanType::CaptureStart)]
                            .contains(&tokens.peek().map(|x| &x.span_type))
                        {
                            ()
                        } else {
                            select
                                .fields
                                .push(Field::Simple(input[token.range.clone()].to_string()))
                        }
                    }
                }
                SpanType::CaptureStart
                    if previous.as_ref().map(|x| x.span_type) == Some(SpanType::String) =>
                {
                    let inner_select = Self::parse_select(input, tokens)?;
                    select.fields.push(Field::Nested(
                        Box::new(Field::Simple(
                            input[previous.unwrap().range.clone()].to_string(),
                        )),
                        inner_select,
                    ));
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
                SpanType::CaptureEnd => {
                    break;
                }
                found => return Err(Error::invalid_token(SpanType::Equal, found, token.range)),
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
    Simple(String),
    Aliased { alias: String, column: String },
    Nested(Box<Field>, Select),
}

#[test]
fn simple_select() {
    let input = "select=first_name,age";
    let lexer = Lexer::new(input.chars());
    let expected = Ast {
        select: Some(Select {
            fields: vec![
                Field::Simple("first_name".to_string()),
                Field::Simple("age".to_string()),
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
                Field::Aliased {
                    alias: "firstName".to_string(),
                    column: "first_name".to_string(),
                },
                Field::Simple("age".to_string()),
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
                Field::Simple("id".to_string()),
                Field::Nested(
                    Box::new(Field::Simple("projects".to_string())),
                    Select {
                        fields: vec![
                            Field::Simple("id".to_string()),
                            Field::Nested(
                                Box::new(Field::Simple("tasks".to_string())),
                                Select {
                                    fields: vec![
                                        Field::Simple("id".to_string()),
                                        Field::Simple("name".to_string()),
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
