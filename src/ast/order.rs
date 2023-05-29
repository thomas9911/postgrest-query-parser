use std::fmt::Debug;
use std::iter::Peekable;
use std::ops::Deref;
use std::str::FromStr;

use crate::ast::{Ast, Error};
use crate::lexer::{Lexer, Span, SpanType};

#[derive(Debug, PartialEq, Clone, Default)]
pub struct Order {
    fields: Vec<OrderItem>,
}

#[derive(Debug, PartialEq, Clone, Default)]
pub struct OrderItem {
    field: String,
    operator: Operator,
    nulls_position: Option<NullOption>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Operator {
    Asc,
    Desc,
}

#[derive(Debug, PartialEq, Clone)]
pub enum NullOption {
    First,
    Last,
}

impl Default for Operator {
    fn default() -> Self {
        Operator::Asc
    }
}

impl Ast {
    pub(crate) fn parse_order<T>(
        input: &str,
        tokens: &mut Peekable<Lexer<T>>,
    ) -> Result<Order, Error>
    where
        T: Iterator<Item = char>,
    {
        let mut order = Order::default();
        let mut order_item = OrderItem::default();
        let mut field_set = false;
        let mut path_length = 0;

        while let Some(token) = tokens.next() {
            // dbg!(token);
            match token.span_type {
                SpanType::String if field_set == false => {
                    order_item.field = input[token.range].to_string();
                    field_set = true;
                }
                SpanType::String if field_set == true => match &input[token.range.clone()] {
                    "asc" => order_item.operator = Operator::Asc,
                    "desc" => order_item.operator = Operator::Desc,
                    "nullsfirst" => order_item.nulls_position = Some(NullOption::First),
                    "nullslast" => order_item.nulls_position = Some(NullOption::Last),
                    found => {
                        return Err(Error::InvalidOrderDirection {
                            found: found.to_string(),
                            range: token.range,
                        })
                    }
                },
                SpanType::PathSeparator => {
                    path_length += 1;
                    if path_length > 2 {
                        return Err(Error::invalid_token(
                            SpanType::Separator,
                            SpanType::PathSeparator,
                            token.range,
                        ));
                    }
                }
                SpanType::Separator
                    if tokens.peek() == None
                        || tokens.peek().map(|x| x.span_type) == Some(SpanType::And) =>
                {
                    return Err(Error::TrailingComma { range: token.range });
                }
                SpanType::Separator => {
                    order.fields.push(order_item);
                    order_item = OrderItem::default();
                    field_set = false;
                    path_length = 0;
                }
                found => return Err(Error::invalid_token(SpanType::String, found, token.range)),
            }
        }

        order.fields.push(order_item);

        Ok(order)
    }
}

#[test]
fn simple_order() {
    let input = "order=age";
    let lexer = Lexer::new(input.chars());
    let expected = Ast {
        order: Some(Order {
            fields: vec![OrderItem {
                field: "age".to_string(),
                operator: Operator::Asc,
                nulls_position: None,
            }],
        }),
        ..Default::default()
    };
    let out = Ast::from_lexer(input, lexer).unwrap();

    assert_eq!(expected, out);
}

#[test]
fn single_order_with_operator() {
    let input = "order=age.desc";
    let lexer = Lexer::new(input.chars());
    let expected = Ast {
        order: Some(Order {
            fields: vec![OrderItem {
                field: "age".to_string(),
                operator: Operator::Desc,
                nulls_position: None,
            }],
        }),
        ..Default::default()
    };
    let out = Ast::from_lexer(input, lexer).unwrap();

    assert_eq!(expected, out);
}

#[test]
fn single_order_with_operator_and_nulls_option() {
    let input = "order=age.desc.nullslast";
    let lexer = Lexer::new(input.chars());
    let expected = Ast {
        order: Some(Order {
            fields: vec![OrderItem {
                field: "age".to_string(),
                operator: Operator::Desc,
                nulls_position: Some(NullOption::Last),
            }],
        }),
        ..Default::default()
    };
    let out = Ast::from_lexer(input, lexer).unwrap();

    assert_eq!(expected, out);
}

#[test]
fn multiple_order() {
    let input = "order=age.desc,height.asc,width.desc.nullsfirst";
    let lexer = Lexer::new(input.chars());
    let expected = Ast {
        order: Some(Order {
            fields: vec![
                OrderItem {
                    field: "age".to_string(),
                    operator: Operator::Desc,
                    nulls_position: None,
                },
                OrderItem {
                    field: "height".to_string(),
                    operator: Operator::Asc,
                    nulls_position: None,
                },
                OrderItem {
                    field: "width".to_string(),
                    operator: Operator::Desc,
                    nulls_position: Some(NullOption::First),
                },
            ],
        }),
        ..Default::default()
    };
    let out = Ast::from_lexer(input, lexer).unwrap();

    assert_eq!(expected, out);
}

#[test]
fn invalid_order_too_long_path() {
    let input = "order=age.desc.nullsfirst.asc";
    let lexer = Lexer::new(input.chars());
    let out = Ast::from_lexer(input, lexer).unwrap_err();
    let expected = Error::InvalidToken {
        expected: SpanType::Separator,
        found: SpanType::PathSeparator,
        range: 25..26,
    };
    assert_eq!(expected, out);
    assert_eq!(".asc", &input[25..]);
}

#[test]
fn invalid_order_trailing_comma() {
    let input = "order=age.desc,";
    let lexer = Lexer::new(input.chars());
    let out = Ast::from_lexer(input, lexer).unwrap_err();
    let expected = Error::TrailingComma { range: 14..15 };
    assert_eq!(expected, out);
    assert_eq!(",", &input[14..]);
}

#[test]
fn invalid_order_trailing_comma_with_next_item() {
    let input = "order=age.desc,&age=eq.42";
    let lexer = Lexer::new(input.chars());
    let out = Ast::from_lexer(input, lexer).unwrap_err();
    let expected = Error::TrailingComma { range: 14..15 };
    assert_eq!(expected, out);
    assert_eq!(",&", &input[14..16]);
}
