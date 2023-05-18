use std::{
    iter::{Enumerate, Peekable},
    ops::Range,
};

pub struct Lexer<T>
where
    T: Iterator<Item = char>,
{
    tokens: Peekable<Tokenizer<T>>,
}

fn one_range(pos: usize) -> Range<usize> {
    pos..(pos + 1)
}

impl<T> Iterator for Lexer<T>
where
    T: Iterator<Item = char>,
{
    type Item = Span;

    fn next(&mut self) -> Option<Self::Item> {
        let mut start = None;
        while let Some(token) = self.tokens.next() {
            if start.is_none() {
                start = Some(token.pos)
            };

            match token.token_type {
                TokenType::Equal => {
                    return Some(Span {
                        span_type: SpanType::Equal,
                        range: one_range(start.unwrap()),
                    })
                }
                TokenType::Ampersand => {
                    return Some(Span {
                        span_type: SpanType::And,
                        range: one_range(start.unwrap()),
                    })
                }
                TokenType::Comma => {
                    return Some(Span {
                        span_type: SpanType::Separator,
                        range: one_range(start.unwrap()),
                    })
                }
                _ => (),
            };

            if token.token_type == TokenType::Char
                && self.tokens.peek().map(|x| &x.token_type) != Some(&TokenType::Char)
            {
                let range = start.unwrap()..(token.pos + 1);

                return Some(Span {
                    span_type: SpanType::String,
                    range,
                });
            }
        }

        None
    }
}

#[derive(Debug, PartialEq)]
pub struct Span {
    pub span_type: SpanType,
    pub range: Range<usize>,
}

#[derive(Debug, PartialEq)]
pub enum SpanType {
    String,
    Equal,
    And,
    Separator,
}

impl<T> Lexer<T>
where
    T: Iterator<Item = char>,
{
    pub fn new(input: T) -> Lexer<T> {
        Lexer {
            tokens: Tokenizer::new(input).peekable(),
        }
    }
}

#[derive(Debug)]
pub struct Tokenizer<T>
where
    T: Iterator<Item = char>,
{
    input: Enumerate<T>,
}

impl<T> Iterator for Tokenizer<T>
where
    T: Iterator<Item = char>,
{
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some((pos, ch)) = self.input.next() {
            return Some(Token {
                token_type: TokenType::from(ch),
                pos,
            });
        }

        None
    }
}

impl<T> Tokenizer<T>
where
    T: Iterator<Item = char>,
{
    pub fn new(input: T) -> Tokenizer<T> {
        Tokenizer {
            input: input.enumerate(),
        }
    }
}

#[derive(Debug)]
pub struct Token {
    pub token_type: TokenType,
    pub pos: usize,
}

#[derive(Debug, PartialEq)]
pub enum TokenType {
    Equal,
    Comma,
    Dot,
    DoubleColon,
    Minus,
    RoundBracketsOpen,
    RoundBracketsClose,
    SquareBracketsOpen,
    SquareBracketsClose,
    AngleBracketsOpen,
    AngleBracketsClose,
    CurlyBracketsOpen,
    CurlyBracketsClose,
    Ampersand,
    Char,
}

impl From<char> for TokenType {
    fn from(ch: char) -> TokenType {
        use TokenType::*;

        match ch {
            '=' => Equal,
            ',' => Comma,
            '.' => Dot,
            ':' => DoubleColon,
            '-' => Minus,
            '>' => AngleBracketsClose,
            '(' => RoundBracketsOpen,
            ')' => RoundBracketsClose,
            '[' => SquareBracketsOpen,
            ']' => SquareBracketsClose,
            '{' => CurlyBracketsOpen,
            '}' => CurlyBracketsClose,
            '&' => Ampersand,
            _ => Char,
        }
    }
}

// postgrest tests: https://github.com/PostgREST/postgrest/blob/main/test/spec/Feature/Query/QuerySpec.hs

#[test]
fn simple_select() {
    let input = "select=first_name,age";
    let lexer = Lexer::new(input.chars());

    let expected = vec![
        (SpanType::String, "select"),
        (SpanType::Equal, "="),
        (SpanType::String, "first_name"),
        (SpanType::Separator, ","),
        (SpanType::String, "age"),
    ];

    let mut out = Vec::new();
    for x in lexer {
        out.push((x.span_type, &input[x.range]));
    }

    assert_eq!(expected, out);
}
