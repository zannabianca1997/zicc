use std::mem;

use arrayvec::ArrayVec;
use convert_case::Casing;
use itertools::Itertools;
use proc_macro2::{Delimiter, Group, Literal, Punct, TokenStream, TokenTree};
use quote::{format_ident, TokenStreamExt};

use crate::KEYWORDS;

struct Catch {
    separator: Option<Punct>,
    content: TokenStream,
}

enum ExpandingTokenTree {
    Text(TokenStream),
    Catch(Catch),
}

fn tokenize(input: TokenStream) -> Vec<ExpandingTokenTree> {
    let mut fill_window = {
        let mut input = input.into_iter();
        move |window: &mut ArrayVec<TokenTree, 4>| {
            while !window.is_full() {
                let Some(tt) = input.next() else {
                    return;
                };
                window.push(tt)
            }
        }
    };
    let mut window = ArrayVec::new();
    fill_window(&mut window);
    let mut output = vec![];
    while !window.is_empty() {
        if window.len() == 4
            && matches!(&window[..], [TokenTree::Punct(dollar), TokenTree::Group(def), TokenTree::Punct(_),TokenTree::Ident(kwds)] if dollar.as_char() == '$' && def.delimiter() == Delimiter::Parenthesis && kwds == "kwds")
        {
            let Ok([_, TokenTree::Group(def), TokenTree::Punct(sep), _]) =
                mem::take(&mut window).into_inner()
            else {
                unreachable!()
            };
            output.push(ExpandingTokenTree::Catch(Catch {
                separator: Some(sep),
                content: expand_token_stream(def.stream()),
            }))
        } else if window.len() >= 3
            && matches!(&window[..3], [TokenTree::Punct(dollar), TokenTree::Group(def), TokenTree::Ident(kwds)] if dollar.as_char() == '$' && def.delimiter() == Delimiter::Parenthesis && kwds == "kwds")
        {
            let Some((_, TokenTree::Group(def), _)) = window.drain(0..3).collect_tuple() else {
                unreachable!()
            };
            output.push(ExpandingTokenTree::Catch(Catch {
                separator: None,
                content: expand_token_stream(def.stream()),
            }))
        } else {
            let token = match window.remove(0) {
                TokenTree::Group(group) => {
                    let mut new_group =
                        Group::new(group.delimiter(), expand_token_stream(group.stream()));
                    new_group.set_span(group.span());
                    TokenTree::Group(new_group)
                }
                token => token,
            };
            if let Some(ExpandingTokenTree::Text(stream)) = output.last_mut() {
                stream.append(token)
            } else {
                output.push(ExpandingTokenTree::Text(token.into()))
            }
        }
        fill_window(&mut window);
    }
    output
}

pub fn expand_token_stream(input: TokenStream) -> TokenStream {
    expand_tokens(tokenize(input))
}

fn expand_tokens(input: Vec<ExpandingTokenTree>) -> TokenStream {
    input
        .into_iter()
        .map(|tt| match tt {
            ExpandingTokenTree::Text(text) => text,
            ExpandingTokenTree::Catch(catch) => expand_catch(catch),
        })
        .concat()
}

fn expand_catch(Catch { separator, content }: Catch) -> TokenStream {
    Itertools::intersperse(
        KEYWORDS
            .iter()
            .map(|keyword| expand_metavars(*keyword, content.clone())),
        separator
            .map(|p| TokenTree::Punct(p).into())
            .unwrap_or_else(|| TokenStream::new()),
    )
    .collect()
}

fn expand_metavars(keyword: &str, content: TokenStream) -> TokenStream {
    content
        .into_iter()
        .map(|tt| match tt {
            TokenTree::Group(group) => {
                let mut new_group =
                    Group::new(group.delimiter(), expand_metavars(keyword, group.stream()));
                new_group.set_span(group.span());
                TokenTree::Group(new_group)
            }
            tt => tt,
        })
        .coalesce(|a, b| match (a, b) {
            (TokenTree::Punct(dollar), TokenTree::Ident(ident))
                if dollar.as_char() == '$' && ident == "Keyword" =>
            {
                Ok(TokenTree::Ident(format_ident!(
                    "{}",
                    keyword.to_case(convert_case::Case::Pascal)
                )))
            }
            (TokenTree::Punct(dollar), TokenTree::Ident(ident))
                if dollar.as_char() == '$' && ident == "kwd_str" =>
            {
                Ok(TokenTree::Literal(Literal::string(&keyword)))
            }
            (TokenTree::Punct(dollar), TokenTree::Ident(ident))
                if dollar.as_char() == '$' && ident == "kwd_regex" =>
            {
                Ok(TokenTree::Literal(Literal::string(&format!(
                    "(?i){keyword}"
                ))))
            }
            (a, b) => Err((a, b)),
        })
        .collect()
}
