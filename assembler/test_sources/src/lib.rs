use proc_macro::TokenStream;

#[proc_macro_attribute]
pub fn test_sources(params: TokenStream, function: TokenStream) -> TokenStream {
    impls::test_sources(params.into(), function.into()).into()
}
#[proc_macro_attribute]
pub fn test_io(params: TokenStream, function: TokenStream) -> TokenStream {
    impls::test_io(params.into(), function.into()).into()
}

mod impls {
    use std::mem;

    use quote::{format_ident, quote, ToTokens};
    use vm::VMInt;

    static SOURCES: &[(&str, SourceFile)] = include!(env!("SOURCES"));

    struct SourceFile {
        source: Source,
        tests: &'static [(&'static str, Test)],
    }

    struct Source {
        descr: Option<&'static str>,
        source: &'static str,
    }

    struct Test {
        descr: Option<&'static str>,
        r#in: &'static [VMInt],
        out: &'static [VMInt],
    }
    use proc_macro2::TokenStream;

    pub(crate) fn test_sources(params: TokenStream, function: TokenStream) -> TokenStream {
        let mut test_fn: syn::ItemFn = {
            let mut errors = None;
            if !params.is_empty() {
                errors = Some(syn::Error::new_spanned(
                    params,
                    "The attribute takes no parameter",
                ))
            }
            match syn::parse2(function) {
                Ok(f) => f,
                Err(e2) => {
                    return match errors {
                        Some(mut e1) => {
                            e1.combine(e2);
                            e1
                        }
                        None => e2,
                    }
                    .into_compile_error();
                }
            }
        };
        // give the function a new name and visibility
        let mod_vis = mem::replace(&mut test_fn.vis, syn::Visibility::Inherited);
        let mod_name = mem::replace(&mut test_fn.sig.ident, {
            let mut fn_name_prefix = String::new();
            while SOURCES.iter().any(|(n, _)| n.starts_with(&fn_name_prefix)) {
                fn_name_prefix.push('_')
            }
            format_ident!("{fn_name_prefix}test_fn")
        });
        // insert in the function the use statement to access all the object in the mod namespace
        test_fn.block.stmts.insert(
            0,
            syn::parse_quote!(
                use super::*;
            ),
        );
        // make the function immutable from now
        let test_fn = test_fn;

        let sources_fns = SOURCES.into_iter().map(
            |(
                name,
                SourceFile {
                    source: Source { descr, source },
                    ..
                },
            )| {
                let descr = descr
                    .map(|d| quote!(#[doc = #descr]))
                    .unwrap_or_else(|| quote!());
                let name = format_ident!("{name}");
                let test_fn_name = &test_fn.sig.ident;

                quote!(
                    #descr
                    #[test]
                    pub fn #name() {
                        #test_fn_name(#source)
                    }
                )
            },
        );

        quote!(
            #mod_vis mod #mod_name {

                #test_fn

                #(#sources_fns)*
            }
        )
        .into_token_stream()
    }

    pub(crate) fn test_io(params: TokenStream, function: TokenStream) -> TokenStream {
        let mut test_fn: syn::ItemFn = {
            let mut errors = None;
            if !params.is_empty() {
                errors = Some(syn::Error::new_spanned(
                    params,
                    "The attribute takes no parameter",
                ))
            }
            match syn::parse2(function) {
                Ok(f) => f,
                Err(e2) => {
                    return match errors {
                        Some(mut e1) => {
                            e1.combine(e2);
                            e1
                        }
                        None => e2,
                    }
                    .into_compile_error();
                }
            }
        };
        // give the function a new name and visibility
        let mod_vis = mem::replace(&mut test_fn.vis, syn::Visibility::Inherited);
        let mod_name = mem::replace(&mut test_fn.sig.ident, {
            let mut fn_name_prefix = String::new();
            while SOURCES.iter().any(|(n, _)| n.starts_with(&fn_name_prefix)) {
                fn_name_prefix.push('_')
            }
            format_ident!("{fn_name_prefix}test_fn")
        });
        // insert in the function the use statement to access all the object in the mod namespace
        test_fn.block.stmts.insert(
            0,
            syn::parse_quote!(
                use super::*;
            ),
        );
        // make the function immutable from now
        let test_fn = test_fn;

        let sources_fns = SOURCES.into_iter().map(
            |(
                name,
                SourceFile {
                    source: Source { descr, source },
                    tests,
                },
            )| {
                let descr = descr
                    .map(|descr| quote!(#[doc = #descr]))
                    .unwrap_or_else(|| quote!());
                let name = format_ident!("{name}");

                let test_fns = tests.into_iter().map(|(name, Test { descr, r#in, out })| {
                    let descr = descr
                        .map(|descr| quote!(#[doc = #descr]))
                        .unwrap_or_else(|| quote!());
                    let name = format_ident!("{name}");
                    let test_fn_name = &test_fn.sig.ident;
                    quote!(
                        #descr
                        #[test]
                        pub fn #name() {
                            super::#test_fn_name(#source, &[#(#r#in),*], &[#(#out),*])
                        }
                    )
                });

                quote!(
                    #descr
                    pub mod #name {
                        #(#test_fns)*
                    }
                )
            },
        );

        quote!(
            #mod_vis mod #mod_name {

                #test_fn

                #(#sources_fns)*
            }
        )
        .into_token_stream()
    }
}
