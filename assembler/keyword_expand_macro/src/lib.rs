use proc_macro::TokenStream;

#[proc_macro]
pub fn expand_keywords(input: TokenStream) -> TokenStream {
    keyword_expand::expand_token_stream(input.into()).into()
}
