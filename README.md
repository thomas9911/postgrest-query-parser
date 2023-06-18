# postgrest-query-parser

Note that not all operators are implemented, you get an
`Error::OperatorNotImplemented` error when this happens.

## example

```rust
use postgrest_query_parser::{Ast, Lexer};
use postgrest_query_parser::ast::{
    Field, FieldKey, Filter, FilterPath, InnerFilter, Order, OrderItem, Select,
};
use postgrest_query_parser::ast::filter;
use postgrest_query_parser::ast::order;

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
```

You can use the Lexer directly to build the ast yourself. See example below or
check the tests.

```rust
use postgrest_query_parser::Lexer;
use postgrest_query_parser::lexer::{Span, SpanType};

let input = "id=gte.14&order=id.asc&select=id&id=lt.54";
let lexer = Lexer::new(input.chars());
let tokens: Vec<_> = lexer.collect();
let expected = vec![
    Span {
        span_type: SpanType::String,
        range: 0..2,
    },
    Span {
        span_type: SpanType::Equal,
        range: 2..3,
    },
    Span {
        span_type: SpanType::String,
        range: 3..6,
    },
    Span {
        span_type: SpanType::PathSeparator,
        range: 6..7,
    },
    Span {
        span_type: SpanType::String,
        range: 7..9,
    },
    Span {
        span_type: SpanType::And,
        range: 9..10,
    },
    Span {
        span_type: SpanType::String,
        range: 10..15,
    },
    Span {
        span_type: SpanType::Equal,
        range: 15..16,
    },
    Span {
        span_type: SpanType::String,
        range: 16..18,
    },
    Span {
        span_type: SpanType::PathSeparator,
        range: 18..19,
    },
    Span {
        span_type: SpanType::String,
        range: 19..22,
    },
    Span {
        span_type: SpanType::And,
        range: 22..23,
    },
    Span {
        span_type: SpanType::String,
        range: 23..29,
    },
    Span {
        span_type: SpanType::Equal,
        range: 29..30,
    },
    Span {
        span_type: SpanType::String,
        range: 30..32,
    },
    Span {
        span_type: SpanType::And,
        range: 32..33,
    },
    Span {
        span_type: SpanType::String,
        range: 33..35,
    },
    Span {
        span_type: SpanType::Equal,
        range: 35..36,
    },
    Span {
        span_type: SpanType::String,
        range: 36..38,
    },
    Span {
        span_type: SpanType::PathSeparator,
        range: 38..39,
    },
    Span {
        span_type: SpanType::String,
        range: 39..41,
    },
];

assert_eq!(tokens, expected);
```
