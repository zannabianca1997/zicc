use proc_macro::TokenStream;

/// Parse rust tokens as intcode assembly
#[proc_macro]
pub fn ica(tokens: TokenStream) -> TokenStream {
    impls::ica(tokens.into()).into()
}

mod impls {
    use either::Either::{Left, Right};
    use itertools::Itertools;
    use lexer::{Identifier, Lexer, SpecialIdentifier, StringLit, Token};
    use proc_macro2::{Delimiter, Group, TokenStream, TokenTree};
    use quote::quote;

    pub(super) fn ica(tokens: TokenStream) -> TokenStream {
        match parse_macro_input(tokens) {
            Ok(v) => v,
            Err(e) => {
                return e.into_compile_error();
            }
        }
        .into_iter()
        .coalesce(|t1, t2| match (t1, t2) {
            (MacroToken::TokenDef(d1), MacroToken::TokenDef(d2)) => {
                Ok(MacroToken::TokenDef(quote!(#d1, #d2)))
            }
            (t1, t2) => Err((t1, t2)),
        })
        .map(|t| match t {
            MacroToken::TokenDef(t) => quote!([#t]),
            MacroToken::BracedValue(v) => {
                quote!({ #v })
            }
        })
        .with_position()
        .map(|(pos, t)| match pos {
            itertools::Position::First | itertools::Position::Only => quote!(#t.into_iter()),
            itertools::Position::Middle | itertools::Position::Last => quote!(.chain(#t)),
        })
        .concat()
    }

    enum MacroToken {
        TokenDef(TokenStream),
        BracedValue(TokenStream),
    }

    fn parse_macro_input(input: TokenStream) -> syn::Result<Vec<MacroToken>> {
        input
            .into_iter()
            // flattening all groups
            .map(|tt| match tt {
                TokenTree::Group(group) => match group.delimiter() {
                    Delimiter::Parenthesis => parse_macro_input(group.stream()).map(|v| {
                        Right(
                            Some(MacroToken::TokenDef(token_def(Token::OpenPar)))
                                .into_iter()
                                .chain(v)
                                .chain(Some(MacroToken::TokenDef(token_def(Token::ClosePar))))
                                .collect_vec(),
                        )
                    }),
                    Delimiter::None => parse_macro_input(group.stream()).map(Right),
                    Delimiter::Brace => Ok(Right(vec![MacroToken::BracedValue(group.stream())])),
                    Delimiter::Bracket => Err(syn::Error::new_spanned(
                        group,
                        "Bracket [ ... ] are invalid in intcode assembly",
                    )),
                },
                TokenTree::Ident(_) | TokenTree::Punct(_) | TokenTree::Literal(_) => {
                    Ok(Left(vec![tt]))
                }
            })
            // group the longest sequences of parsed elements, or errors
            .coalesce(|a, b| match (a, b) {
                (Ok(Left(mut v1)), Ok(Left(mut v2))) => {
                    v1.append(&mut v2);
                    Ok(Ok(Left(v1)))
                }
                (Ok(Right(mut v1)), Ok(Right(mut v2))) => {
                    v1.append(&mut v2);
                    Ok(Ok(Right(v1)))
                }
                (Err(mut e1), Err(e2)) => {
                    e1.combine(e2);
                    Ok(Err(e1))
                }
                (a, b) => Err((a, b)),
            })
            .map(|r| match r {
                Ok(Left(tts)) => parse_flattened_tokens(tts),
                Ok(Right(mts)) => Ok(mts),
                Err(e) => Err(e),
            })
            .fold(Ok(vec![]), |acc, v| match (acc, v) {
                (Ok(mut v1), Ok(mut v2)) => {
                    v1.append(&mut v2);
                    Ok(v1)
                }
                (Ok(_), Err(e)) | (Err(e), Ok(_)) => Err(e),
                (Err(mut e1), Err(e2)) => {
                    e1.combine(e2);
                    Err(e1)
                }
            })
    }

    fn parse_flattened_tokens(tts: Vec<TokenTree>) -> syn::Result<Vec<MacroToken>> {
        tts.into_iter()
            .map(|t| Ok(Left(t)))
            .coalesce(|t1, t2| match (t1, t2) {
                (Ok(Left(TokenTree::Punct(p1))), Ok(Left(next))) if p1.as_char() == '$' => {
                    Ok(relex_special_id(next).map(|def| Right(MacroToken::TokenDef(def))))
                }
                (t1, t2) => Err((t1, t2)),
            })
            .map(|t| t.map(|e| e.map_either(|x| vec![x], |x| vec![x])))
            // group the longest sequences of parsed elements, or errors
            .coalesce(|a, b| match (a, b) {
                (Ok(Left(mut v1)), Ok(Left(mut v2))) => {
                    v1.append(&mut v2);
                    Ok(Ok(Left(v1)))
                }
                (Ok(Right(mut v1)), Ok(Right(mut v2))) => {
                    v1.append(&mut v2);
                    Ok(Ok(Right(v1)))
                }
                (Err(mut e1), Err(e2)) => {
                    e1.combine(e2);
                    Ok(Err(e1))
                }
                (a, b) => Err((a, b)),
            })
            .map(|r| match r {
                Ok(Left(tts)) => parse_simple_tokens(tts),
                Ok(Right(mts)) => Ok(mts),
                Err(e) => Err(e),
            })
            .fold(Ok(vec![]), |acc, v| match (acc, v) {
                (Ok(mut v1), Ok(mut v2)) => {
                    v1.append(&mut v2);
                    Ok(v1)
                }
                (Ok(_), Err(e)) | (Err(e), Ok(_)) => Err(e),
                (Err(mut e1), Err(e2)) => {
                    e1.combine(e2);
                    Err(e1)
                }
            })
    }

    fn parse_simple_tokens(tts: Vec<TokenTree>) -> syn::Result<Vec<MacroToken>> {
        tts.into_iter()
            .fold(Ok(vec![]), |acc, v| match (acc, relex(v)) {
                (Ok(mut v1), Ok(tok)) => {
                    v1.push(MacroToken::TokenDef(tok));
                    Ok(v1)
                }
                (Ok(_), Err(e)) | (Err(e), Ok(_)) => Err(e),
                (Err(mut e1), Err(e2)) => {
                    e1.combine(e2);
                    Err(e1)
                }
            })
    }

    fn relex(v: TokenTree) -> syn::Result<TokenStream> {
        let tok = format!("{v}");
        let mut lexer = Lexer::new(&tok);
        let lexed = lexer.next().unwrap();
        match lexed {
            Ok((_, tok, _)) => Ok(token_def(tok)),
            Err(e) => Err(syn::Error::new_spanned(v, e)),
        }
    }

    fn relex_special_id(next: TokenTree) -> syn::Result<TokenStream> {
        let tok = format!("${next}");
        let mut lexer = Lexer::new(&tok);
        let lexed = lexer.next().unwrap();
        match lexed {
            Ok((_, tok, _)) => Ok(token_def(tok)),
            Err(e) => Err(syn::Error::new_spanned(next, e)),
        }
    }

    fn token_def(t: Token<'_>) -> TokenStream {
        match t {
            Token::Ints(Identifier::Named(name)) => {
                quote!(::lexer::Token::Ints(::lexer::Identifier::Named(#name)))
            }
            Token::Ints(Identifier::Unnamed(n)) => {
                quote!(::lexer::Token::Ints(::lexer::Identifier::Unnamed(#n)))
            }
            Token::Add(Identifier::Named(name)) => {
                quote!(::lexer::Token::Add(::lexer::Identifier::Named(#name)))
            }
            Token::Add(Identifier::Unnamed(n)) => {
                quote!(::lexer::Token::Add(::lexer::Identifier::Unnamed(#n)))
            }
            Token::Mul(Identifier::Named(name)) => {
                quote!(::lexer::Token::Mul(::lexer::Identifier::Named(#name)))
            }
            Token::Mul(Identifier::Unnamed(n)) => {
                quote!(::lexer::Token::Mul(::lexer::Identifier::Unnamed(#n)))
            }
            Token::In(Identifier::Named(name)) => {
                quote!(::lexer::Token::In(::lexer::Identifier::Named(#name)))
            }
            Token::In(Identifier::Unnamed(n)) => {
                quote!(::lexer::Token::In(::lexer::Identifier::Unnamed(#n)))
            }
            Token::Out(Identifier::Named(name)) => {
                quote!(::lexer::Token::Out(::lexer::Identifier::Named(#name)))
            }
            Token::Out(Identifier::Unnamed(n)) => {
                quote!(::lexer::Token::Out(::lexer::Identifier::Unnamed(#n)))
            }
            Token::Jz(Identifier::Named(name)) => {
                quote!(::lexer::Token::Jz(::lexer::Identifier::Named(#name)))
            }
            Token::Jz(Identifier::Unnamed(n)) => {
                quote!(::lexer::Token::Jz(::lexer::Identifier::Unnamed(#n)))
            }
            Token::Jnz(Identifier::Named(name)) => {
                quote!(::lexer::Token::Jnz(::lexer::Identifier::Named(#name)))
            }
            Token::Jnz(Identifier::Unnamed(n)) => {
                quote!(::lexer::Token::Jnz(::lexer::Identifier::Unnamed(#n)))
            }
            Token::Slt(Identifier::Named(name)) => {
                quote!(::lexer::Token::Slt(::lexer::Identifier::Named(#name)))
            }
            Token::Slt(Identifier::Unnamed(n)) => {
                quote!(::lexer::Token::Slt(::lexer::Identifier::Unnamed(#n)))
            }
            Token::Seq(Identifier::Named(name)) => {
                quote!(::lexer::Token::Seq(::lexer::Identifier::Named(#name)))
            }
            Token::Seq(Identifier::Unnamed(n)) => {
                quote!(::lexer::Token::Seq(::lexer::Identifier::Unnamed(#n)))
            }
            Token::Incb(Identifier::Named(name)) => {
                quote!(::lexer::Token::Incb(::lexer::Identifier::Named(#name)))
            }
            Token::Incb(Identifier::Unnamed(n)) => {
                quote!(::lexer::Token::Incb(::lexer::Identifier::Unnamed(#n)))
            }
            Token::Halt(Identifier::Named(name)) => {
                quote!(::lexer::Token::Halt(::lexer::Identifier::Named(#name)))
            }
            Token::Halt(Identifier::Unnamed(n)) => {
                quote!(::lexer::Token::Halt(::lexer::Identifier::Unnamed(#n)))
            }
            Token::Inc(Identifier::Named(name)) => {
                quote!(::lexer::Token::Inc(::lexer::Identifier::Named(#name)))
            }
            Token::Inc(Identifier::Unnamed(n)) => {
                quote!(::lexer::Token::Inc(::lexer::Identifier::Unnamed(#n)))
            }
            Token::Dec(Identifier::Named(name)) => {
                quote!(::lexer::Token::Dec(::lexer::Identifier::Named(#name)))
            }
            Token::Dec(Identifier::Unnamed(n)) => {
                quote!(::lexer::Token::Dec(::lexer::Identifier::Unnamed(#n)))
            }
            Token::Identifier(Identifier::Named(name)) => {
                quote!(::lexer::Token::Identifier(::lexer::Identifier::Named(#name)))
            }
            Token::Identifier(Identifier::Unnamed(n)) => {
                quote!(::lexer::Token::Identifier(::lexer::Identifier::Unnamed(#n)))
            }

            Token::SpecialIdentifier(SpecialIdentifier::End) => quote!(
                ::lexer::Token::SpecialIdentifier(::lexer::SpecialIdentifier::End)
            ),
            Token::SpecialIdentifier(SpecialIdentifier::Start) => quote!(
                ::lexer::Token::SpecialIdentifier(::lexer::SpecialIdentifier::Start)
            ),
            Token::SpecialIdentifier(SpecialIdentifier::UnitEnd) => quote!(
                ::lexer::Token::SpecialIdentifier(::lexer::SpecialIdentifier::UnitEnd)
            ),
            Token::SpecialIdentifier(SpecialIdentifier::UnitStart) => quote!(
                ::lexer::Token::SpecialIdentifier(::lexer::SpecialIdentifier::UnitStart)
            ),

            Token::Number(n) => quote!(::lexer::Token::Number(#n)),

            Token::StringLit(StringLit { content }) => {
                quote!(::lexer::Token::StringLit(StringLit { content: #content }))
            }

            Token::Newline => quote!(::lexer::Token::Newline),
            Token::Comma => quote!(::lexer::Token::Comma),
            Token::Colon => quote!(::lexer::Token::Colon),
            Token::At => quote!(::lexer::Token::At),
            Token::Pound => quote!(::lexer::Token::Pound),
            Token::OpenPar => quote!(::lexer::Token::OpenPar),
            Token::ClosePar => quote!(::lexer::Token::ClosePar),
            Token::Plus => quote!(::lexer::Token::Plus),
            Token::Minus => quote!(::lexer::Token::Minus),
            Token::Times => quote!(::lexer::Token::Times),
            Token::Div => quote!(::lexer::Token::Div),
            Token::Mod => quote!(::lexer::Token::Mod),
        }
    }
}
