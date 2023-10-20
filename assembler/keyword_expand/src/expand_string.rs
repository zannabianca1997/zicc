use lazy_regex::regex_replace_all;

use super::KEYWORDS;

use convert_case::Casing;
use itertools::Itertools;

struct Open {
    can_catch: bool,
    content: String,
}

enum ExpandingToken {
    Text(String),
    Open(Open),
    Close {
        can_catch: bool,
        content: String,
        separator: Option<String>,
    },
}

struct Catch {
    separator: Option<String>,
    content: String,
}

enum ExpandingTokenTree {
    Text(String),
    Catch(Catch),
}

fn tokenize(mut input: &str) -> Vec<ExpandingTokenTree> {
    let mut tokens = vec![];
    let mut stack = vec![];
    while !input.is_empty() {
        let (captured, plain, open,close, sep) = lazy_regex::regex_captures!(r#"^(?:((?:[^\$"\/\(\)\[\]\{\}]+|"(?:[^"]+|\\.)*"|'(?:[^"]+|\\.)*'|\/\/[^\n]*|\/\*[^*]*\*+(?:[^*\/][^*]*\*+)*\/|\/|\$\s*[^\s\(])+)|((?:\$\s*)?\(|\[|\{)|(\)(?:\s*([^\s\w])?\s*kwds)?|\]|\}))"#, input).unwrap();
        input = &input[captured.len()..];
        let token = match (plain, open, close) {
            (text, "", "") => ExpandingToken::Text(text.to_string()),
            ("", open, "") => ExpandingToken::Open(Open {
                can_catch: open.len() > 1,
                content: open.to_string(),
            }),
            ("", "", close) => ExpandingToken::Close {
                can_catch: close.len() > 1,
                content: close.to_string(),
                separator: (!sep.is_empty()).then(|| sep.to_string()),
            },
            _ => unreachable!(),
        };
        match token {
            ExpandingToken::Text(text) => {
                let tokens = stack.last_mut().map(|(_, ts)| ts).unwrap_or(&mut tokens);
                tokens.push(ExpandingTokenTree::Text(text))
            }
            ExpandingToken::Open(open) => stack.push((open, vec![])),
            ExpandingToken::Close {
                can_catch: close_can_catch,
                content: close,
                separator,
            } => {
                if let Some((
                    Open {
                        can_catch: open_can_catch,
                        content: open,
                    },
                    content,
                )) = stack.pop()
                {
                    let tokens = stack.last_mut().map(|(_, ts)| ts).unwrap_or(&mut tokens);
                    if open_can_catch && close_can_catch {
                        tokens.push(ExpandingTokenTree::Catch(Catch {
                            separator,
                            content: expand_tokens(content),
                        }))
                    } else {
                        tokens.push(ExpandingTokenTree::Text(open));
                        tokens.extend(content);
                        tokens.push(ExpandingTokenTree::Text(close));
                    }
                } else {
                    // unmatched parenthesis, getting it on the stream as plain text
                    tokens.push(ExpandingTokenTree::Text(close))
                };
            }
        }
    }
    // unmatched parenthesis, getting it on the stream as plain text
    tokens.extend(
        stack
            .into_iter()
            .flat_map(|(Open { content, .. }, tokens)| {
                Some(ExpandingTokenTree::Text(content))
                    .into_iter()
                    .chain(tokens)
            }),
    );
    tokens
}

pub fn expand_string(input: &str) -> String {
    expand_tokens(tokenize(input))
}

fn expand_tokens(input: Vec<ExpandingTokenTree>) -> String {
    input
        .into_iter()
        .map(|tt| match tt {
            ExpandingTokenTree::Text(text) => text,
            ExpandingTokenTree::Catch(catch) => expand_catch(catch),
        })
        .reduce(|mut a, b| {
            a.push_str(&b);
            a
        })
        .unwrap_or_else(|| String::new())
}

fn expand_catch(Catch { separator, content }: Catch) -> String {
    Itertools::intersperse(
        KEYWORDS.iter().map(|keyword| {
            regex_replace_all!(
                r#"\$\s*(?:(Keyword)|(kwd_str)|(kwd_regex))"#,
                &content,
                |_, i, s, r| {
                    match (i, s, r) {
                        (_, "", "") => keyword.to_case(convert_case::Case::Pascal),
                        ("", _, "") => format!("\"{keyword}\""),
                        ("", "", _) => format!("\"(?i){keyword}\""),
                        _ => unreachable!(),
                    }
                }
            )
            .into_owned()
        }),
        separator.unwrap_or_else(|| String::new()),
    )
    .reduce(|mut a, b| {
        a.push_str(&b);
        a
    })
    .unwrap_or_else(|| String::new())
}
