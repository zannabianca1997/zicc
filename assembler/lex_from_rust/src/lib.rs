use proc_macro::TokenStream;

#[proc_macro]
pub fn tokens(tokens: TokenStream) -> TokenStream {
    impls::tokens(tokens.into()).into()
}

mod impls {
    use proc_macro2::{Delimiter, Group, TokenStream};
    use quote::quote;

    pub(super) fn tokens(tokens: TokenStream) -> TokenStream {
        let mut stream = quote!([].into_iter());
        add_tokens_to_stream(tokens, &mut stream);
        stream
    }

    fn add_tokens_to_stream(tokens: TokenStream, stream: &mut TokenStream) {
        for token in tokens {
            match token {
                proc_macro2::TokenTree::Group(g) if g.delimiter() == Delimiter::Parenthesis => {
                    todo!()
                }
                proc_macro2::TokenTree::Group(g) if g.delimiter() == Delimiter::Brace => todo!(),
                proc_macro2::TokenTree::Ident(_) => todo!(),
                proc_macro2::TokenTree::Punct(_) => todo!(),
                proc_macro2::TokenTree::Literal(_) => todo!(),
                _ => todo!(),
            }
        }
    }
}
