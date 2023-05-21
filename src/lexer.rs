use std::{iter::Enumerate, ops::Range};

use peekmore::{PeekMore, PeekMoreIterator};

pub struct Lexer<T>
where
    T: Iterator<Item = char>,
{
    tokens: PeekMoreIterator<Tokenizer<T>>,
    previous: Option<TokenType>,
}

impl<T> Lexer<T>
where
    T: Iterator<Item = char>,
{
    fn current_is_char_and_next_is_not_char(&mut self, token_type: &TokenType) -> bool {
        let next = self.tokens.peek().map(|x| &x.token_type);

        (token_type == &TokenType::Char
            && next != Some(&TokenType::Char)
            && next != Some(&TokenType::Minus))
            || self.next_will_be_arrow()
    }

    fn current_is_double_colon_and_next_is_double_colon(&mut self, token_type: &TokenType) -> bool {
        token_type == &TokenType::DoubleColon
            && self.tokens.peek().map(|x| &x.token_type) == Some(&TokenType::DoubleColon)
    }

    fn current_is_double_colon_and_previous_was_double_colon(
        &mut self,
        token_type: &TokenType,
    ) -> bool {
        token_type == &TokenType::DoubleColon && self.previous == Some(TokenType::DoubleColon)
    }

    fn current_is_minus_and_next_angle_bracket_close(&mut self, token_type: &TokenType) -> bool {
        token_type == &TokenType::Minus && self.previous == Some(TokenType::AngleBracketsClose)
    }

    fn current_is_angle_bracket_close_and_previous_was_minus(
        &mut self,
        token_type: &TokenType,
    ) -> bool {
        token_type == &TokenType::AngleBracketsClose && self.previous == Some(TokenType::Minus)
    }

    fn set_previous(&mut self, token_type: TokenType) {
        self.previous = Some(token_type);
    }

    fn next_will_be_arrow(&mut self) -> bool {
        (self.tokens.peek().map(|x| &x.token_type)) == Some(&TokenType::Minus)
            && (self.tokens.peek_nth(1).map(|x| &x.token_type))
                == Some(&TokenType::AngleBracketsClose)
    }
}

fn one_range(pos: usize) -> Range<usize> {
    pos..(pos + 1)
}

fn as_single_item(token_type: &TokenType, pos: usize) -> Option<Span> {
    match token_type {
        TokenType::Equal => Some(Span {
            span_type: SpanType::Equal,
            range: one_range(pos),
        }),
        TokenType::Ampersand => Some(Span {
            span_type: SpanType::And,
            range: one_range(pos),
        }),
        TokenType::Comma => Some(Span {
            span_type: SpanType::Separator,
            range: one_range(pos),
        }),
        TokenType::Questionmark => Some(Span {
            span_type: SpanType::QueryStart,
            range: one_range(pos),
        }),
        TokenType::Dot => Some(Span {
            span_type: SpanType::PathSeparator,
            range: one_range(pos),
        }),
        TokenType::RoundBracketsOpen => Some(Span {
            span_type: SpanType::CaptureStart,
            range: one_range(pos),
        }),
        TokenType::RoundBracketsClose => Some(Span {
            span_type: SpanType::CaptureEnd,
            range: one_range(pos),
        }),
        TokenType::CurlyBracketsOpen => Some(Span {
            span_type: SpanType::ListStart,
            range: one_range(pos),
        }),
        TokenType::CurlyBracketsClose => Some(Span {
            span_type: SpanType::ListEnd,
            range: one_range(pos),
        }),
        TokenType::Space => Some(Span {
            span_type: SpanType::Empty,
            range: one_range(pos),
        }),
        TokenType::DoubleColon => Some(Span {
            span_type: SpanType::Alias,
            range: one_range(pos),
        }),
        _ => None,
    }
}

impl<T> Iterator for Lexer<T>
where
    T: Iterator<Item = char>,
{
    type Item = Span;

    fn next(&mut self) -> Option<Self::Item> {
        let mut start = None;
        while let Some(token) = self.tokens.next() {
            // if start.is_none() {
            //     start = Some(token.pos)
            // };

            start = start.or(Some(token.pos));

            let start_pos = start.expect("start is always set");

            if self.current_is_double_colon_and_next_is_double_colon(&token.token_type) {
                self.set_previous(token.token_type);
                continue;
            }

            if self.current_is_double_colon_and_previous_was_double_colon(&token.token_type) {
                let range = start_pos..(token.pos + 1);

                self.set_previous(token.token_type);
                return Some(Span {
                    span_type: SpanType::Cast,
                    range,
                });
            }

            if self.current_is_minus_and_next_angle_bracket_close(&token.token_type) {
                self.set_previous(token.token_type);
                continue;
            }

            if self.current_is_angle_bracket_close_and_previous_was_minus(&token.token_type) {
                if self.tokens.peek().map(|x| &x.token_type) == Some(&TokenType::AngleBracketsClose)
                {
                    // lets roll forward
                    let token = self.tokens.next().unwrap();
                    let range = start_pos..(token.pos + 1);
                    self.set_previous(token.token_type);
                    return Some(Span {
                        span_type: SpanType::BinaryArrow,
                        range,
                    });
                }
                let range = start_pos..(token.pos + 1);

                self.set_previous(token.token_type);
                return Some(Span {
                    span_type: SpanType::Arrow,
                    range,
                });
            }

            if let Some(span) = as_single_item(&token.token_type, start_pos) {
                self.set_previous(token.token_type);
                return Some(span);
            }

            if self.current_is_char_and_next_is_not_char(&token.token_type) {
                let range = start_pos..(token.pos + 1);

                self.set_previous(token.token_type);
                return Some(Span {
                    span_type: SpanType::String,
                    range,
                });
            }

            self.set_previous(token.token_type);
        }

        None
    }
}

#[derive(Debug, PartialEq)]
pub struct Span {
    pub span_type: SpanType,
    pub range: Range<usize>,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum SpanType {
    String,
    Alias,
    Cast,
    Equal,
    And,
    Separator,
    PathSeparator,
    QueryStart,
    CaptureStart,
    CaptureEnd,
    ListStart,
    ListEnd,
    Empty,
    BinaryArrow,
    Arrow,
}

impl<T> Lexer<T>
where
    T: Iterator<Item = char>,
{
    pub fn new(input: T) -> Lexer<T> {
        Lexer {
            tokens: Tokenizer::new(input).peekmore(),
            previous: None,
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

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TokenType {
    Questionmark,
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
    Space,
    Char,
}

impl From<char> for TokenType {
    fn from(ch: char) -> TokenType {
        use TokenType::*;

        match ch {
            '?' => Questionmark,
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
            ' ' => Space,
            _ => Char,
        }
    }
}

// postgrest tests: https://github.com/PostgREST/postgrest/blob/main/test/spec/Feature/Query/QuerySpec.hs

#[test]
fn simple_select() {
    use SpanType::*;
    let input = "select=first_name,age";
    let lexer = Lexer::new(input.chars());

    let expected = vec![
        (String, "select"),
        (Equal, "="),
        (String, "first_name"),
        (Separator, ","),
        (String, "age"),
    ];

    let mut out = Vec::new();
    for x in lexer {
        out.push((x.span_type, &input[x.range]));
    }

    assert_eq!(expected, out);
}

#[test]
fn simple_query() {
    use SpanType::*;

    let input = "?id=not.eq.5&order=id";
    let lexer = Lexer::new(input.chars());

    let expected = vec![
        (QueryStart, "?"),
        (String, "id"),
        (Equal, "="),
        (String, "not"),
        (PathSeparator, "."),
        (String, "eq"),
        (PathSeparator, "."),
        (String, "5"),
        (And, "&"),
        (String, "order"),
        (Equal, "="),
        (String, "id"),
    ];

    let mut out = Vec::new();
    for x in lexer {
        out.push((x.span_type, &input[x.range]));
    }

    assert_eq!(expected, out);
}

#[test]
fn or_statement_query() {
    use SpanType::*;

    let input = "?or=(text_search_vector.phfts(german).Art%20Spass, text_search_vector.phfts(french).amusant, text_search_vector.fts(english).impossible)";
    let lexer = Lexer::new(input.chars());

    let expected = vec![
        (QueryStart, "?"),
        (String, "or"),
        (Equal, "="),
        (CaptureStart, "("),
        (String, "text_search_vector"),
        (PathSeparator, "."),
        (String, "phfts"),
        (CaptureStart, "("),
        (String, "german"),
        (CaptureEnd, ")"),
        (PathSeparator, "."),
        (String, "Art%20Spass"),
        (Separator, ","),
        (Empty, " "),
        (String, "text_search_vector"),
        (PathSeparator, "."),
        (String, "phfts"),
        (CaptureStart, "("),
        (String, "french"),
        (CaptureEnd, ")"),
        (PathSeparator, "."),
        (String, "amusant"),
        (Separator, ","),
        (Empty, " "),
        (String, "text_search_vector"),
        (PathSeparator, "."),
        (String, "fts"),
        (CaptureStart, "("),
        (String, "english"),
        (CaptureEnd, ")"),
        (PathSeparator, "."),
        (String, "impossible"),
        (CaptureEnd, ")"),
    ];

    let mut out = Vec::new();
    for x in lexer {
        out.push((x.span_type, &input[x.range]));
    }

    assert_eq!(expected, out);
}

#[test]
fn nested_statement_query() {
    use SpanType::*;

    let input = "?select=id,projects(id,tasks(id,name))&projects.tasks.name=like.Design*";
    let lexer = Lexer::new(input.chars());

    let expected = vec![
        (QueryStart, "?"),
        (String, "select"),
        (Equal, "="),
        (String, "id"),
        (Separator, ","),
        (String, "projects"),
        (CaptureStart, "("),
        (String, "id"),
        (Separator, ","),
        (String, "tasks"),
        (CaptureStart, "("),
        (String, "id"),
        (Separator, ","),
        (String, "name"),
        (CaptureEnd, ")"),
        (CaptureEnd, ")"),
        (And, "&"),
        (String, "projects"),
        (PathSeparator, "."),
        (String, "tasks"),
        (PathSeparator, "."),
        (String, "name"),
        (Equal, "="),
        (String, "like"),
        (PathSeparator, "."),
        (String, "Design*"),
    ];

    let mut out = Vec::new();
    for x in lexer {
        out.push((x.span_type, &input[x.range]));
    }

    assert_eq!(expected, out);
}

#[test]
fn statement_with_list_query() {
    use SpanType::*;

    let input = "?select=id&arr_data=cd.{1,2,4}";
    let lexer = Lexer::new(input.chars());

    let expected = vec![
        (QueryStart, "?"),
        (String, "select"),
        (Equal, "="),
        (String, "id"),
        (And, "&"),
        (String, "arr_data"),
        (Equal, "="),
        (String, "cd"),
        (PathSeparator, "."),
        (ListStart, "{"),
        (String, "1"),
        (Separator, ","),
        (String, "2"),
        (Separator, ","),
        (String, "4"),
        (ListEnd, "}"),
    ];

    let mut out = Vec::new();
    for x in lexer {
        out.push((x.span_type, &input[x.range]));
    }

    assert_eq!(expected, out);
}

#[test]
fn typecast_statement_query() {
    use SpanType::*;

    let input = "select=clientId:id,oid_col::int,oid_array_col::_int4";
    let lexer = Lexer::new(input.chars());

    let expected = vec![
        (String, "select"),
        (Equal, "="),
        (String, "clientId"),
        (Alias, ":"),
        (String, "id"),
        (Separator, ","),
        (String, "oid_col"),
        (Cast, "::"),
        (String, "int"),
        (Separator, ","),
        (String, "oid_array_col"),
        (Cast, "::"),
        (String, "_int4"),
    ];

    let mut out = Vec::new();
    for x in lexer {
        out.push((x.span_type, &input[x.range]));
    }

    assert_eq!(expected, out);
}

#[test]
fn statement_with_escaped_characters_query() {
    use SpanType::*;

    let input = "?select=%22:arr-%3Eow::cast%22,%22(inside,parens)%22,%22a.dotted.column%22,%22%20%20col%20%20w%20%20space%20%20%22&%22*id*%22=eq.1";
    let lexer = Lexer::new(input.chars());

    let expected = vec![
        (QueryStart, "?"),
        (String, "select"),
        (Equal, "="),
        (String, "%22"),
        (Alias, ":"),
        (String, "arr-%3Eow"),
        (Cast, "::"),
        (String, "cast%22"),
        (Separator, ","),
        (String, "%22"),
        (CaptureStart, "("),
        (String, "inside"),
        (Separator, ","),
        (String, "parens"),
        (CaptureEnd, ")"),
        (String, "%22"),
        (Separator, ","),
        (String, "%22a"),
        (PathSeparator, "."),
        (String, "dotted"),
        (PathSeparator, "."),
        (String, "column%22"),
        (Separator, ","),
        (String, "%22%20%20col%20%20w%20%20space%20%20%22"),
        (And, "&"),
        (String, "%22*id*%22"),
        (Equal, "="),
        (String, "eq"),
        (PathSeparator, "."),
        (String, "1"),
    ];

    let mut out = Vec::new();
    for x in lexer {
        out.push((x.span_type, &input[x.range]));
    }

    assert_eq!(expected, out);
}

#[test]
fn statement_with_json_query() {
    use SpanType::*;

    let input = "select=id,json_data->>blood_type,json_data->phones";
    let lexer = Lexer::new(input.chars());

    let expected = vec![
        (String, "select"),
        (Equal, "="),
        (String, "id"),
        (Separator, ","),
        (String, "json_data"),
        (BinaryArrow, "->>"),
        (String, "blood_type"),
        (Separator, ","),
        (String, "json_data"),
        (Arrow, "->"),
        (String, "phones"),
    ];

    let mut out = Vec::new();
    for x in lexer {
        out.push((x.span_type, &input[x.range]));
    }

    assert_eq!(expected, out);
}
