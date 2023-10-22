use parser::ast::File;
use proc_macro::TokenStream;
use quote::ToTokens;
use syn::parse_macro_input;

/// Parse rust tokens as intcode assembly
#[proc_macro]
pub fn ica(tokens: TokenStream) -> TokenStream {
    let file = parse_macro_input!(tokens as impls::AstDef<File<'static>>);
    file.into_token_stream().into()
}

mod impls {
    use std::marker::PhantomData;

    use parser::ast::{File, Labelled, Statement};
    use proc_macro2::{Delimiter, Group, TokenStream, TokenTree};
    use quote::{quote, ToTokens};
    use syn::{braced, ext::IdentExt, parse::Parse, punctuated::Punctuated, Ident, LitInt, Token};
    use uncased::AsUncased;

    pub struct AstDef<T>(TokenStream, PhantomData<T>);

    impl<T> ToTokens for AstDef<T> {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            self.0.to_tokens(tokens)
        }
    }

    impl Parse for AstDef<File<'static>> {
        fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
            let statements = input
                .parse_terminated(
                    AstDef::<Labelled<'static, Option<Statement<'static>>>>::parse,
                    Token![;],
                )?
                .into_iter();
            Ok(Self(
                quote!(::parser::ast::File {
                    statements: ::std::vec![#(#statements),*]
                }),
                PhantomData,
            ))
        }
    }

    impl<T> Parse for AstDef<Labelled<'static, T>>
    where
        AstDef<T>: Parse,
    {
        fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
            let mut labels = vec![];
            while input.peek2(Token![:]) || (input.peek(Token![$]) && input.peek3(Token![:])) {
                labels.push(if input.peek(Token![$]) {
                    let _: Token![$] = input.parse().unwrap();
                    let n: LitInt = input.parse()?;
                    let _: Token![:] = input.parse().unwrap();
                    quote!(::parser::ast::LabelDef { label: ::lexer::Identifier::Unnamed(#n)})
                } else {
                    let tt: TokenTree = input.parse()?;
                    let _: Token![:] = input.parse().unwrap();
                    match tt {
                        TokenTree::Ident(ident) if !ident.to_string().contains('#') => {
                            let ident = ident.to_string();
                            quote!(::parser::ast::LabelDef { label: ::lexer::Identifier::Named(#ident)})
                        },
                        TokenTree::Group( group) if group.delimiter() == Delimiter::Brace => {
                            // cleaning eventual `unused braces` warning
                            let group = Group::new(Delimiter::Parenthesis, group.stream());
                            quote!(::parser::ast::LabelDef { label: #group.into()})
                        },
                        _ => {
                            return Err(syn::Error::new_spanned(
                                tt,
                                "Expected identifier or braced value",
                            ))
                        }
                    }
                })
            }
            let content: AstDef<T> = input.parse()?;
            Ok(AstDef(
                quote!(
                    ::parser::ast::Labelled {
                        labels: ::std::collections::BTreeSet::from([#(#labels),*]),
                        content: #content
                    }
                ),
                PhantomData,
            ))
        }
    }

    impl Parse for AstDef<Option<Statement<'static>>> {
        fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
            let content = if input.is_empty() || input.peek(Token![;]) {
                None
            } else {
                Some(input.parse::<AstDef<Statement<'static>>>()?)
            };
            Ok(AstDef(
                if let Some(content) = content {
                    quote!(::std::option::Option::Some(#content))
                } else {
                    quote!(::std::option::Option::None)
                },
                PhantomData,
            ))
        }
    }
    impl Parse for AstDef<Statement<'static>> {
        fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
            let lookahead = input.lookahead1();
            if lookahead.peek(syn::token::Brace) {
                let content;
                let _ = braced!(content in input);
                let content: TokenStream = content.parse().unwrap();
                Ok(AstDef(quote!({#content}), PhantomData))
            } else if lookahead.peek(Ident::peek_any) {
                let cmd_ident = input.call(Ident::parse_any).unwrap();
                let cmd_params = input.;
                let cmd = cmd_ident.to_string();
                let cmd = cmd.as_uncased();
                if cmd == "ints" {
                    todo!()
                } else if cmd == "add" {
                    todo!()
                } else if cmd == "mul" {
                    todo!()
                } else if cmd == "in" {
                    todo!()
                } else if cmd == "out" {
                    todo!()
                } else if cmd == "jz" {
                    todo!()
                } else if cmd == "jnz" {
                    todo!()
                } else if cmd == "slt" {
                    todo!()
                } else if cmd == "seq" {
                    todo!()
                } else if cmd == "incb" {
                    todo!()
                } else if cmd == "halt" {
                    todo!()
                } else if cmd == "inc" {
                    todo!()
                } else if cmd == "dec" {
                    todo!()
                } else {
                    Err(syn::Error::new_spanned(
                        cmd_ident,
                        "Expected command keyword",
                    ))
                }
            } else {
                Err(lookahead.error())
            }
        }
    }
}
