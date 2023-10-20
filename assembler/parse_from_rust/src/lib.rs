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

    use either::Either::{Left, Right};
    use itertools::Itertools;
    use keyword_expand_macro::expand_keywords;
    use parser::ast::{File, Labelled, Statement};
    use proc_macro2::{Delimiter, Group, TokenStream, TokenTree};
    use quote::{quote, ToTokens};
    use syn::{parse::Parse, punctuated::Punctuated, Token};

    pub struct AstDef<T>(TokenStream, PhantomData<T>);

    impl<T> ToTokens for AstDef<T> {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            self.0.to_tokens(tokens)
        }
    }

    impl Parse for AstDef<File<'static>> {
        fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
            let statements = Punctuated::<
                AstDef<Labelled<'static, Option<Statement<'static>>>>,
                Token![;],
            >::parse_terminated(input)?
            .into_iter();
            Ok(Self(
                quote!(::parser::ast::File {
                    statement: ::std::vec![#(#statements),*]
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
            todo!()
        }
    }

    impl Parse for AstDef<Option<Statement<'static>>> {
        fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
            let content = if input.is_empty() || input.peek(Token![;]) {
                None
            } else {
                Some(AstDef::<Statement<'static>>::parse(input)?)
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
            todo!()
        }
    }
}
