use proc_macro::TokenStream;

/// Parse rust tokens as intcode assembly
#[proc_macro]
pub fn ica(tokens: TokenStream) -> TokenStream {
    impls::ica(tokens.into()).into()
}
/*
mod impls {
    use either::Either::{Left, Right};
    use itertools::{peek_nth, Group, Itertools};
    use proc_macro2::{Delimiter, Punct, TokenStream, TokenTree};
    use quote::{quote, TokenStreamExt};
    use uncased::AsUncased;

    pub(crate) fn ica(tokens: TokenStream) -> TokenStream {
        match file_ast(tokens) {
            Ok(ast) => ast,
            Err(err) => err.into_compile_error(),
        }
    }

    fn file_ast(tokens: TokenStream) -> syn::Result<TokenStream> {
        // split lines
        let statements = tokens
            .into_iter()
            .batching(|tokens| {
                if let Some(first) = tokens.next() {
                    let mut line = TokenStream::from(first);
                    while let Some(token) = tokens.next() {
                        if matches!(&token, TokenTree::Punct(p) if p.as_char() == ';') {
                            break;
                        }
                        line.append(token)
                    }
                    Some(line)
                } else {
                    None
                }
            })
            .map(|tokens| labelled_ast(tokens, |tokens| option_ast(tokens, statement_ast)))
            .fold(Ok(vec![]), |acc, stm| match (acc, stm) {
                (Ok(mut acc), Ok(stm)) => {
                    acc.push(stm);
                    Ok(acc)
                }
                (Ok(_), Err(e)) | (Err(e), Ok(_)) => Err(e),
                (Err(mut e1), Err(e2)) => {
                    e1.combine(e2);
                    Err(e1)
                }
            })?;
        Ok(quote!(::parser::ast::File {
            statements: ::std::vec![#(#statements),*]
        }))
    }

    /// Either { .. } of element
    fn element_or_group_ast(
        tokens: TokenStream,
        inner_fn: impl FnOnce(TokenStream) -> syn::Result<TokenStream>,
    ) {
        let mut tokens = tokens.into_iter().peekable();
        if
    }

    /// build an ast for Labelled<T>
    fn labelled_ast(
        tokens: TokenStream,
        inner_fn: impl FnOnce(TokenStream) -> syn::Result<TokenStream>,
    ) -> syn::Result<TokenStream> {
        let mut inner_fn = Some(inner_fn);
        let (labels, content) = peek_nth(tokens).batching(|tokens| {
            if matches!(tokens.peek_nth(1), Some(TokenTree::Punct(p)) if p.as_char() == ':') {
                let (name, _) = tokens.next_tuple().unwrap();
                if let TokenTree::Ident(ident) = name {
                    let ident = ident.to_string();
                    Some(Ok(Right(quote!(::lexer::Identifier::Named(#ident)))))
                } else if let TokenTree::Group(group) = name {
                    let group = group.stream();
                    Some(Ok(Right(quote!({#group}.into()))))
                } else {
                    Some(Err(syn::Error::new_spanned(
                        name,
                        "Expected identifier or { .. }",
                    )))
                }
            } else if matches!(tokens.peek_nth(0), Some(TokenTree::Punct(p)) if p.as_char() == '$')
                && matches!(tokens.peek_nth(2), Some(TokenTree::Punct(p)) if p.as_char() == ':')
            {
                let (_, lit, _) = tokens.next_tuple().unwrap();
                if let TokenTree::Literal(lit) = lit {
                    Some(Ok(Right(quote!(::lexer::Identifier::Unnamed(#lit)))))
                } else {
                    Some(Err(syn::Error::new_spanned(lit, "Expected literal")))
                }
            } else if tokens.peek().is_some() {
                Some(
                    (inner_fn
                        .take()
                        .expect("This should be called only once at the end of the labels"))(
                        tokens.collect(),
                    )
                    .map(Left),
                )
            } else {
                None
            }
        }).fold(Ok((vec![], None)), |acc, stm| match (acc, stm) {
            (Ok((mut labels, None)), Ok(Right(ident))) => {
                labels.push(quote!(::parser::ast::LabelDef { label: #ident }));
                Ok((labels, None))
            }
            (Ok((labels, None)), Ok(Left(content))) => {
                Ok((labels, Some(content)))
            }
            (Ok((_, Some(_))), _) => {
                unreachable!("Nothing should be produced after the end of the line")
            }
            (Ok(_), Err(e)) | (Err(e), Ok(_)) => Err(e),
            (Err(mut e1), Err(e2)) => {
                e1.combine(e2);
                Err(e1)
            }
        })?;
        let content = match (content, inner_fn) {
            (Some(content), None) => content,
            (None, Some(inner_fn)) => inner_fn(TokenStream::new())?,
            _ => unreachable!(),
        };
        Ok(quote!(
            ::parser::ast::Labelled {
                labels: ::std::collections::BTreeSet::from([#(#labels),*]),
                content: #content,
            }
        ))
    }

    /// build an ast for Option<T>
    fn option_ast(
        tokens: TokenStream,
        inner_fn: impl FnOnce(TokenStream) -> syn::Result<TokenStream>,
    ) -> syn::Result<TokenStream> {
        if tokens.is_empty() {
            Ok(quote!(::std::option::Option::None))
        } else {
            let content = inner_fn(tokens)?;
            Ok(quote!(::std::option::Option::Some(#content)))
        }
    }

    fn statement_ast(tokens: TokenStream) -> syn::Result<TokenStream> {
        if tokens.is_empty() {
            return Err(syn::Error::new_spanned(tokens, "Expected statement"));
        }
        let mut tokens = tokens.into_iter();
        let cmd = tokens.next().unwrap();
        let args = tokens.collect::<TokenStream>();
        match cmd {
            TokenTree::Group(group) if group.delimiter() == Delimiter::Brace => {
                if !args.is_empty() {
                    return Err(syn::Error::new_spanned(args, "Unexpected tokens"));
                }
                let group = group.stream();
                Ok(quote!({#group}.into()))
            }
            TokenTree::Ident(ident) if ident.to_string().as_uncased() == "ints" => {
                let stm = ints_stm_ast(args)?;
                Ok(quote!(::parser::ast::Statement::IntsStm(#stm)))
            }
            TokenTree::Ident(ident) if ident.to_string().as_uncased() == "add" => {
                let instr = add_instr_ast(args)?;
                Ok(quote!(::parser::ast::Statement::Instruction(#instr)))
            }
            TokenTree::Ident(ident) if ident.to_string().as_uncased() == "mul" => {
                let instr = mul_instr_ast(args)?;
                Ok(quote!(::parser::ast::Statement::Instruction(#instr)))
            }
            TokenTree::Ident(ident) if ident.to_string().as_uncased() == "in" => {
                let instr = in_instr_ast(args)?;
                Ok(quote!(::parser::ast::Statement::Instruction(#instr)))
            }
            TokenTree::Ident(ident) if ident.to_string().as_uncased() == "out" => {
                let instr = out_instr_ast(args)?;
                Ok(quote!(::parser::ast::Statement::Instruction(#instr)))
            }
            TokenTree::Ident(ident) if ident.to_string().as_uncased() == "jz" => {
                let instr = jz_instr_ast(args)?;
                Ok(quote!(::parser::ast::Statement::Instruction(#instr)))
            }
            TokenTree::Ident(ident) if ident.to_string().as_uncased() == "jnz" => {
                let instr = jnz_instr_ast(args)?;
                Ok(quote!(::parser::ast::Statement::Instruction(#instr)))
            }
            TokenTree::Ident(ident) if ident.to_string().as_uncased() == "slt" => {
                let instr = slt_instr_ast(args)?;
                Ok(quote!(::parser::ast::Statement::Instruction(#instr)))
            }
            TokenTree::Ident(ident) if ident.to_string().as_uncased() == "seq" => {
                let instr = seq_instr_ast(args)?;
                Ok(quote!(::parser::ast::Statement::Instruction(#instr)))
            }
            TokenTree::Ident(ident) if ident.to_string().as_uncased() == "incb" => {
                let instr = incb_instr_ast(args)?;
                Ok(quote!(::parser::ast::Statement::Instruction(#instr)))
            }
            TokenTree::Ident(ident) if ident.to_string().as_uncased() == "halt" => {
                let instr = halt_instr_ast(args)?;
                Ok(quote!(::parser::ast::Statement::Instruction(#instr)))
            }
            TokenTree::Ident(ident) if ident.to_string().as_uncased() == "inc" => {
                let stm = inc_stm_ast(args)?;
                Ok(quote!(::parser::ast::Statement::Inc(#stm)))
            }
            TokenTree::Ident(ident) if ident.to_string().as_uncased() == "dec" => {
                let stm = dec_stm_ast(args)?;
                Ok(quote!(::parser::ast::Statement::Dec(#stm)))
            }
            other => Err(syn::Error::new_spanned(other, "Expected keyword or { .. }")),
        }
    }

    fn ints_stm_ast(args: TokenStream) -> syn::Result<TokenStream> {
        let values = args
            .into_iter()
            .batching(|tokens| {
                if let Some(first) = tokens.next() {
                    let mut line = TokenStream::from(first);
                    while let Some(token) = tokens.next() {
                        if matches!(&token, TokenTree::Punct(p) if p.as_char() == ';') {
                            break;
                        }
                        line.append(token)
                    }
                    Some(line)
                } else {
                    None
                }
            })
            .map(|tokens| labelled_ast(tokens, either_box_expression_string_lit_ast))
            .fold(Ok(vec![]), |acc, stm| match (acc, stm) {
                (Ok(mut acc), Ok(stm)) => {
                    acc.push(stm);
                    Ok(acc)
                }
                (Ok(_), Err(e)) | (Err(e), Ok(_)) => Err(e),
                (Err(mut e1), Err(e2)) => {
                    e1.combine(e2);
                    Err(e1)
                }
            })?;
        Ok(quote!(::parser::ast::IntsStm {
            values: vec![#(#values),*]
        }))
    }

    fn add_instr_ast(args: TokenStream) -> syn::Result<TokenStream> {
        todo!()
    }

    fn mul_instr_ast(args: TokenStream) -> syn::Result<TokenStream> {
        todo!()
    }

    fn in_instr_ast(args: TokenStream) -> syn::Result<TokenStream> {
        todo!()
    }

    fn out_instr_ast(args: TokenStream) -> syn::Result<TokenStream> {
        todo!()
    }

    fn jz_instr_ast(args: TokenStream) -> syn::Result<TokenStream> {
        todo!()
    }

    fn jnz_instr_ast(args: TokenStream) -> syn::Result<TokenStream> {
        todo!()
    }

    fn slt_instr_ast(args: TokenStream) -> syn::Result<TokenStream> {
        todo!()
    }

    fn seq_instr_ast(args: TokenStream) -> syn::Result<TokenStream> {
        todo!()
    }

    fn incb_instr_ast(args: TokenStream) -> syn::Result<TokenStream> {
        todo!()
    }

    fn halt_instr_ast(args: TokenStream) -> syn::Result<TokenStream> {
        todo!()
    }

    fn inc_stm_ast(args: TokenStream) -> syn::Result<TokenStream> {
        todo!()
    }

    fn dec_stm_ast(args: TokenStream) -> syn::Result<TokenStream> {
        todo!()
    }

    fn either_box_expression_string_lit_ast(tokens: TokenStream) -> syn::Result<TokenStream> {
        todo!()
    }
}
 */
/*  mod impls {
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
} */

mod impls {
    use std::marker::PhantomData;

    use itertools::Itertools;

    use parser::ast::{
        Expression, IntsParam, Labelled, ReadParam, Statement, UnlabelledNonImmediateReadParam,
        UnlabelledReadParam, UnlabelledWriteParam, WriteParam,
    };
    use proc_macro2::{Delimiter, Group, TokenStream, TokenTree};
    use quote::{format_ident, quote, ToTokens, TokenStreamExt};
    use syn::{
        braced,
        ext::IdentExt,
        parse::{discouraged::Speculative, Parse},
        punctuated::Punctuated,
        spanned::Spanned,
        Block, ExprBinary, ExprBlock, ExprGroup, ExprLit, ExprParen, ExprPath, ExprUnary, Ident,
        Lit, LitInt, LitStr, Token,
    };
    use uncased::AsUncased;

    pub struct Ast<T>(TokenStream, PhantomData<T>);

    impl<T> ToTokens for Ast<T> {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            self.0.to_tokens(tokens)
        }
    }

    pub struct AstOrBrace<T>(TokenStream, PhantomData<T>);

    impl<T> ToTokens for AstOrBrace<T> {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            self.0.to_tokens(tokens)
        }
    }

    impl<T> Parse for AstOrBrace<T>
    where
        Ast<T>: Parse,
    {
        fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
            if input.peek(syn::token::Brace) {
                let content;
                let _ = braced!(content in input);
                let content: TokenStream = content.parse().unwrap();
                Ok(Self(quote!({#content}.into()), PhantomData))
            } else {
                let Ast::<T>(s, p) = input.parse()?;
                Ok(Self(s, p))
            }
        }
    }

    pub(crate) fn ica(tokens: TokenStream) -> TokenStream {
        match file_ast(tokens) {
            Ok(ast) => ast,
            Err(err) => err.into_compile_error(),
        }
    }

    fn file_ast(tokens: TokenStream) -> syn::Result<TokenStream> {
        // split lines
        let statements = tokens
            .into_iter()
            .batching(|tokens| {
                if let Some(first) = tokens.next() {
                    let mut line = TokenStream::from(first);
                    while let Some(token) = tokens.next() {
                        if matches!(&token, TokenTree::Punct(p) if p.as_char() == ';') {
                            break;
                        }
                        line.append(token)
                    }
                    Some(line)
                } else {
                    None
                }
            })
            .map(|line| {
                syn::parse2::<AstOrBrace<Labelled<'static, Option<Statement<'static>>>>>(line)
            })
            .fold(Ok(vec![]), |acc, stm| match (acc, stm) {
                (Ok(mut acc), Ok(stm)) => {
                    acc.push(stm);
                    Ok(acc)
                }
                (Ok(_), Err(e)) | (Err(e), Ok(_)) => Err(e),
                (Err(mut e1), Err(e2)) => {
                    e1.combine(e2);
                    Err(e1)
                }
            })?;
        Ok(quote!(::parser::ast::File {
            statements: ::std::vec![#(#statements),*]
        }))
    }

    impl<T> Parse for Ast<Labelled<'static, T>>
    where
        AstOrBrace<T>: Parse,
    {
        fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
            let mut labels = vec![];
            while input.peek2(Token![:]) || (input.peek(Token![$]) && input.peek3(Token![:])) {
                labels.push(if input.peek(Token![$]) {
                    let _: Token![$] = input.parse().unwrap();
                    let n: LitInt = input.parse()?;
                    let _: Token![:] = input.parse().unwrap();
                    quote!(::parser::ast::LabelDef { label: ::lexer::Identifier::Unnamed(#n, (0,0))})
                } else {
                    let tt: TokenTree = input.parse()?;
                    let _: Token![:] = input.parse().unwrap();
                    match tt {
                        TokenTree::Ident(ident) if !ident.to_string().contains('#') => {
                            let ident = ident.to_string();
                            quote!(::parser::ast::LabelDef { label: ::lexer::Identifier::Named(#ident, 0)})
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
            let content: AstOrBrace<T> = input.parse()?;
            Ok(Ast(
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

    impl Parse for Ast<Option<Statement<'static>>> {
        fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
            let content = if input.is_empty() || input.peek(Token![;]) {
                None
            } else {
                Some(input.parse::<AstOrBrace<Statement<'static>>>()?)
            };
            Ok(Ast(
                if let Some(content) = content {
                    quote!(::std::option::Option::Some(#content))
                } else {
                    quote!(::std::option::Option::None)
                },
                PhantomData,
            ))
        }
    }

    impl<T> Parse for Ast<Box<T>>
    where
        AstOrBrace<T>: Parse,
    {
        fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
            let content: AstOrBrace<T> = input.parse()?;
            Ok(Ast(quote!(::std::boxed::Box::new(#content)), PhantomData))
        }
    }

    impl Parse for Ast<Statement<'static>> {
        fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
            let cmd_ident = input.call(Ident::parse_any)?;
            let cmd = cmd_ident.to_string();
            let cmd = cmd.as_uncased();
            if cmd == "ints" {
                let values = input
                    .call(
                        Punctuated::<
                            AstOrBrace<Labelled<'static, IntsParam<'static>>>,
                            Option<Token![,]>,
                        >::parse_terminated,
                    )?
                    .into_iter();
                Ok(Ast(
                    quote!(::parser::ast::Statement::Ints(::parser::ast::IntsStm {
                        values: vec![#(#values),*]
                    })),
                    PhantomData,
                ))
            } else if cmd == "add" {
                let p1: AstOrBrace<ReadParam<'static>> = input.parse()?;
                let _: Option<Token![,]> = input.parse()?;
                let p2: AstOrBrace<ReadParam<'static>> = input.parse()?;
                let _: Option<Token![,]> = input.parse()?;
                let p3: AstOrBrace<WriteParam<'static>> = input.parse()?;
                Ok(Ast(
                    quote!(::parser::ast::Statement::Instruction(::parser::ast::Instruction::Add(
                        #p1,
                        #p2,
                        #p3
                    ))),
                    PhantomData,
                ))
            } else if cmd == "mul" {
                let p1: AstOrBrace<ReadParam<'static>> = input.parse()?;
                let _: Option<Token![,]> = input.parse()?;
                let p2: AstOrBrace<ReadParam<'static>> = input.parse()?;
                let _: Option<Token![,]> = input.parse()?;
                let p3: AstOrBrace<WriteParam<'static>> = input.parse()?;
                Ok(Ast(
                    quote!(::parser::ast::Statement::Instruction(::parser::ast::Instruction::Mul(
                        #p1,
                        #p2,
                        #p3
                    ))),
                    PhantomData,
                ))
            } else if cmd == "in" {
                let p: AstOrBrace<WriteParam<'static>> = input.parse()?;
                Ok(Ast(
                    quote!(::parser::ast::Statement::Instruction(::parser::ast::Instruction::In(
                        #p
                    ))),
                    PhantomData,
                ))
            } else if cmd == "out" {
                let p: AstOrBrace<ReadParam<'static>> = input.parse()?;
                Ok(Ast(
                    quote!(::parser::ast::Statement::Instruction(::parser::ast::Instruction::Out(
                        #p
                    ))),
                    PhantomData,
                ))
            } else if cmd == "jz" {
                let p1: AstOrBrace<ReadParam<'static>> = input.parse()?;
                let _: Option<Token![,]> = input.parse()?;
                let p2: AstOrBrace<ReadParam<'static>> = input.parse()?;
                Ok(Ast(
                    quote!(::parser::ast::Statement::Instruction(::parser::ast::Instruction::Jz(
                        #p1,
                        #p2
                    ))),
                    PhantomData,
                ))
            } else if cmd == "jnz" {
                let p1: AstOrBrace<ReadParam<'static>> = input.parse()?;
                let _: Option<Token![,]> = input.parse()?;
                let p2: AstOrBrace<ReadParam<'static>> = input.parse()?;
                Ok(Ast(
                    quote!(::parser::ast::Statement::Instruction(::parser::ast::Instruction::Jnz(
                        #p1,
                        #p2
                    ))),
                    PhantomData,
                ))
            } else if cmd == "slt" {
                let p1: AstOrBrace<ReadParam<'static>> = input.parse()?;
                let _: Option<Token![,]> = input.parse()?;
                let p2: AstOrBrace<ReadParam<'static>> = input.parse()?;
                let _: Option<Token![,]> = input.parse()?;
                let p3: AstOrBrace<WriteParam<'static>> = input.parse()?;
                Ok(Ast(
                    quote!(::parser::ast::Statement::Instruction(::parser::ast::Instruction::Slt(
                        #p1,
                        #p2,
                        #p3
                    ))),
                    PhantomData,
                ))
            } else if cmd == "seq" {
                let p1: AstOrBrace<ReadParam<'static>> = input.parse()?;
                let _: Option<Token![,]> = input.parse()?;
                let p2: AstOrBrace<ReadParam<'static>> = input.parse()?;
                let _: Option<Token![,]> = input.parse()?;
                let p3: AstOrBrace<WriteParam<'static>> = input.parse()?;
                Ok(Ast(
                    quote!(::parser::ast::Statement::Instruction(::parser::ast::Instruction::Seq(
                        #p1,
                        #p2,
                        #p3
                    ))),
                    PhantomData,
                ))
            } else if cmd == "incb" {
                let p: AstOrBrace<ReadParam<'static>> = input.parse()?;
                Ok(Ast(
                    quote!(::parser::ast::Statement::Instruction(::parser::ast::Instruction::Incb(
                        #p
                    ))),
                    PhantomData,
                ))
            } else if cmd == "halt" {
                Ok(Ast(
                    quote!(::parser::ast::Statement::Instruction(
                        ::parser::ast::Instruction::Halt
                    )),
                    PhantomData,
                ))
            } else if cmd == "inc" {
                let p: AstOrBrace<UnlabelledWriteParam<'static>> = input.parse()?;
                Ok(Ast(
                    quote!(::parser::ast::Statement::Inc(::parser::ast::IncStm(
                        #p
                    ))),
                    PhantomData,
                ))
            } else if cmd == "dec" {
                let p: AstOrBrace<UnlabelledWriteParam<'static>> = input.parse()?;
                Ok(Ast(
                    quote!(::parser::ast::Statement::Dec(::parser::ast::DecStm(
                        #p
                    ))),
                    PhantomData,
                ))
            } else if cmd == "jmp" {
                let p: AstOrBrace<ReadParam<'static>> = input.parse()?;
                Ok(Ast(
                    quote!(::parser::ast::Statement::Jmp(::parser::ast::JmpStm(
                        #p
                    ))),
                    PhantomData,
                ))
            } else if cmd == "mov" {
                let forked = input.fork();
                let a: AstOrBrace<ReadParam<'static>> = forked.parse()?;
                let _: Option<Token![,]> = forked.parse().unwrap();
                let b: AstOrBrace<WriteParam<'static>> = forked.parse()?;
                let _: Option<Token![,]> = forked.parse().unwrap();
                match forked.parse::<AstOrBrace<Box<Expression<'static>>>>() {
                    Ok(_) => {
                        // need to reparse as the stricter version
                        // this could be avoided by building an ast
                        let a: AstOrBrace<UnlabelledNonImmediateReadParam<'static>> =
                            input.parse()?;
                        let _: Option<Token![,]> = input.parse().unwrap();
                        let b: AstOrBrace<UnlabelledWriteParam<'static>> = input.parse()?;
                        let _: Option<Token![,]> = input.parse().unwrap();
                        let n: AstOrBrace<Box<Expression<'static>>> = input.parse()?;
                        Ok(Ast(
                            quote!(::parser::ast::Statement::Mov(::parser::ast::MovStm::Multiple(
                                #a,#b,#n
                            ))),
                            PhantomData,
                        ))
                    }
                    Err(_) => {
                        // joining the stream
                        input.advance_to(&forked);
                        Ok(Ast(
                            quote!(::parser::ast::Statement::Mov(::parser::ast::MovStm::Single(
                                #a,#b
                            ))),
                            PhantomData,
                        ))
                    }
                }
            } else if cmd == "push" {
                let forked = input.fork();
                let a: AstOrBrace<ReadParam<'static>> = forked.parse()?;
                let _: Option<Token![,]> = forked.parse().unwrap();
                match forked.parse::<AstOrBrace<Box<Expression<'static>>>>() {
                    Ok(_) => {
                        // need to reparse as the stricter version
                        // this could be avoided by building an ast
                        let a: AstOrBrace<UnlabelledNonImmediateReadParam<'static>> =
                            input.parse()?;
                        let _: Option<Token![,]> = input.parse().unwrap();
                        let n: AstOrBrace<Box<Expression<'static>>> = input.parse()?;
                        Ok(Ast(
                            quote!(::parser::ast::Statement::Push(::parser::ast::PushStm::Multiple(
                                #a,#n
                            ))),
                            PhantomData,
                        ))
                    }
                    Err(_) => {
                        // joining the stream
                        input.advance_to(&forked);
                        Ok(Ast(
                            quote!(::parser::ast::Statement::Push(::parser::ast::PushStm::Single(
                                #a
                            ))),
                            PhantomData,
                        ))
                    }
                }
            } else if cmd == "pop" {
                let forked = input.fork();
                let a: AstOrBrace<WriteParam<'static>> = forked.parse()?;
                let _: Option<Token![,]> = forked.parse().unwrap();
                match forked.parse::<AstOrBrace<Box<Expression<'static>>>>() {
                    Ok(_) => {
                        // need to reparse as the stricter version
                        // this could be avoided by building an ast
                        let a: AstOrBrace<UnlabelledWriteParam<'static>> = input.parse()?;
                        let _: Option<Token![,]> = input.parse().unwrap();
                        let n: AstOrBrace<Box<Expression<'static>>> = input.parse()?;
                        Ok(Ast(
                            quote!(::parser::ast::Statement::Pop(::parser::ast::PopStm::Multiple(
                                #a,#n
                            ))),
                            PhantomData,
                        ))
                    }
                    Err(_) => {
                        // joining the stream
                        input.advance_to(&forked);
                        Ok(Ast(
                            quote!(::parser::ast::Statement::Pop(::parser::ast::PopStm::Single(
                                #a
                            ))),
                            PhantomData,
                        ))
                    }
                }
            } else if cmd == "call" {
                let p: AstOrBrace<ReadParam<'static>> = input.parse()?;
                Ok(Ast(
                    quote!(::parser::ast::Statement::Call(::parser::ast::CallStm(
                        #p
                    ))),
                    PhantomData,
                ))
            } else if cmd == "ret" {
                Ok(Ast(
                    quote!(::parser::ast::Statement::Ret(::parser::ast::RetStm)),
                    PhantomData,
                ))
            } else {
                Err(syn::Error::new_spanned(
                    cmd_ident,
                    "Expected command keyword",
                ))
            }
        }
    }

    impl AstOrBrace<Box<Expression<'static>>> {
        fn from_rs_expr(expr: syn::Expr) -> syn::Result<Self> {
            match expr {
                syn::Expr::Binary(ExprBinary {
                    attrs,
                    left,
                    op,
                    right,
                }) => {
                    if !attrs.is_empty() {
                        return Err(syn::Error::new(
                            attrs
                                .into_iter()
                                .map(|a| a.span())
                                .reduce(|a, b| a.join(b).unwrap_or(a))
                                .unwrap(),
                            "Expected expression",
                        ));
                    }
                    let left = Self::from_rs_expr(*left);
                    let right = Self::from_rs_expr(*right);
                    let op = match op {
                        syn::BinOp::Add(_) => Ok(format_ident!("Sum")),
                        syn::BinOp::Sub(_) => Ok(format_ident!("Sub")),
                        syn::BinOp::Mul(_) => Ok(format_ident!("Mul")),
                        syn::BinOp::Div(_) => Ok(format_ident!("Div")),
                        syn::BinOp::Rem(_) => Ok(format_ident!("Mod")),
                        other => Err(syn::Error::new_spanned(other, "invalid binary operator")),
                    };
                    match (left, op, right) {
                        (Ok(left), Ok(op), Ok(right)) => Ok(Self(
                            quote!(::std::boxed::Box::new(::parser::ast::Expression::#op(#left, #right))),
                            PhantomData,
                        )),
                        (Ok(_), Ok(_), Err(e))
                        | (Ok(_), Err(e), Ok(_))
                        | (Err(e), Ok(_), Ok(_)) => Err(e),
                        (Ok(_), Err(e2), Err(mut e1))
                        | (Err(mut e1), Ok(_), Err(e2))
                        | (Err(mut e1), Err(e2), Ok(_)) => {
                            e1.combine(e2);
                            Err(e1)
                        }
                        (Err(mut e1), Err(e2), Err(e3)) => {
                            e1.combine(e2);
                            e1.combine(e3);
                            Err(e1)
                        }
                    }
                }
                syn::Expr::Unary(ExprUnary { attrs, op, expr }) => {
                    if !attrs.is_empty() {
                        return Err(syn::Error::new(
                            attrs
                                .into_iter()
                                .map(|a| a.span())
                                .reduce(|a, b| a.join(b).unwrap_or(a))
                                .unwrap(),
                            "Expected expression",
                        ));
                    }
                    let expr = Self::from_rs_expr(*expr);
                    let op = match op {
                        syn::UnOp::Neg(_) => Ok(format_ident!("Neg")),
                        other => Err(syn::Error::new_spanned(other, "invalid unary operator")),
                    };
                    match (op, expr) {
                        (Ok(op), Ok(expr)) => Ok(Self(
                            quote!(::std::boxed::Box::new(::parser::ast::Expression::#op(#expr))),
                            PhantomData,
                        )),
                        (Ok(_), Err(e)) | (Err(e), Ok(_)) => Err(e),
                        (Err(mut e1), Err(e2)) => {
                            e1.combine(e2);
                            Err(e1)
                        }
                    }
                }
                syn::Expr::Block(ExprBlock {
                    attrs,
                    label,
                    block: Block { stmts, .. },
                }) => {
                    if !attrs.is_empty() {
                        return Err(syn::Error::new(
                            attrs
                                .into_iter()
                                .map(|a| a.span())
                                .reduce(|a, b| a.join(b).unwrap_or(a))
                                .unwrap(),
                            "Expected expression",
                        ));
                    };
                    if let Some(label) = label {
                        return Err(syn::Error::new_spanned(label, "Expected expression"));
                    }
                    Ok(Self(quote!({#(#stmts)*}.into()), PhantomData))
                }
                syn::Expr::Lit(ExprLit {
                    attrs,
                    lit: Lit::Int(num),
                }) => {
                    if !attrs.is_empty() {
                        return Err(syn::Error::new(
                            attrs
                                .into_iter()
                                .map(|a| a.span())
                                .reduce(|a, b| a.join(b).unwrap_or(a))
                                .unwrap(),
                            "Expected expression",
                        ));
                    };
                    Ok(Self(
                        quote!(::std::boxed::Box::new(::parser::ast::Expression::Num(#num.into()))),
                        PhantomData,
                    ))
                }
                syn::Expr::Path(ExprPath { attrs, qself, path }) => {
                    if !attrs.is_empty() {
                        return Err(syn::Error::new(
                            attrs
                                .into_iter()
                                .map(|a| a.span())
                                .reduce(|a, b| a.join(b).unwrap_or(a))
                                .unwrap(),
                            "Expected expression",
                        ));
                    };
                    if let Some(qself) = qself {
                        return Err(syn::Error::new(qself.span(), "Expected expression"));
                    }
                    let ident = path.require_ident()?.to_string();
                    Ok(Self(
                        quote!(::std::boxed::Box::new(::parser::ast::Expression::Ref(::parser::ast::LabelRef::Identifier(::lexer::Identifier::Named(#ident,0))))),
                        PhantomData,
                    ))
                }
                syn::Expr::Group(ExprGroup { attrs, expr, .. })
                | syn::Expr::Paren(ExprParen { attrs, expr, .. }) => {
                    if !attrs.is_empty() {
                        return Err(syn::Error::new(
                            attrs
                                .into_iter()
                                .map(|a| a.span())
                                .reduce(|a, b| a.join(b).unwrap_or(a))
                                .unwrap(),
                            "Expected expression",
                        ));
                    };
                    Self::from_rs_expr(*expr)
                }
                exp => Err(syn::Error::new_spanned(
                    exp,
                    "Expected binary operation, block, number or identifier",
                )),
            }
        }
    }

    impl Parse for AstOrBrace<Box<Expression<'static>>> {
        fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
            Self::from_rs_expr(input.parse()?)
        }
    }

    impl Parse for Ast<IntsParam<'static>> {
        fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
            if input.peek(syn::LitStr) {
                let lit: LitStr = input.parse().unwrap();
                Ok(Self(
                    quote!(::parser::ast::IntsParam::Str(::lexer::StringLit {
                        content: #lit
                    })),
                    PhantomData,
                ))
            } else {
                let left: AstOrBrace<Box<Expression<'static>>> = input.parse()?;
                Ok(Self(
                    quote!(::parser::ast::IntsParam::Int(#left)),
                    PhantomData,
                ))
            }
        }
    }

    impl Parse for Ast<ReadParam<'static>> {
        fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
            if input.peek(Token![#]) {
                let _: Token![#] = input.parse().unwrap();
                let value: AstOrBrace<Labelled<'static, Box<Expression>>> = input.parse()?;
                Ok(Self(
                    quote!(::parser::ast::ReadParam::Immediate(::parser::ast::ImmediateParam{value:#value})),
                    PhantomData,
                ))
            } else if input.peek(Token![@]) {
                let _: Token![@] = input.parse().unwrap();
                let value: AstOrBrace<Labelled<'static, Box<Expression>>> = input.parse()?;
                Ok(Self(
                    quote!(::parser::ast::ReadParam::Relative(::parser::ast::RelativeParam{value:#value})),
                    PhantomData,
                ))
            } else {
                let value: AstOrBrace<Labelled<'static, Box<Expression>>> = input.parse()?;
                Ok(Self(
                    quote!(::parser::ast::ReadParam::Absolute(::parser::ast::AbsoluteParam{value:#value})),
                    PhantomData,
                ))
            }
        }
    }

    impl Parse for Ast<WriteParam<'static>> {
        fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
            if input.peek(Token![@]) {
                let _: Token![@] = input.parse().unwrap();
                let value: AstOrBrace<Labelled<'static, Box<Expression>>> = input.parse()?;
                Ok(Self(
                    quote!(::parser::ast::WriteParam::Relative(::parser::ast::RelativeParam{value:#value})),
                    PhantomData,
                ))
            } else {
                let value: AstOrBrace<Labelled<'static, Box<Expression>>> = input.parse()?;
                Ok(Self(
                    quote!(::parser::ast::WriteParam::Absolute(::parser::ast::AbsoluteParam{value:#value})),
                    PhantomData,
                ))
            }
        }
    }

    impl Parse for Ast<UnlabelledReadParam<'static>> {
        fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
            if input.peek(Token![#]) {
                let _: Token![#] = input.parse().unwrap();
                let value: AstOrBrace<Box<Expression>> = input.parse()?;
                Ok(Self(
                    quote!(::parser::ast::UnlabelledReadParam::Immediate(::parser::ast::UnlabelledImmediateParam{value:#value})),
                    PhantomData,
                ))
            } else if input.peek(Token![@]) {
                let _: Token![@] = input.parse().unwrap();
                let value: AstOrBrace<Box<Expression>> = input.parse()?;
                Ok(Self(
                    quote!(::parser::ast::UnlabelledReadParam::Relative(::parser::ast::UnlabelledRelativeParam{value:#value})),
                    PhantomData,
                ))
            } else {
                let value: AstOrBrace<Box<Expression>> = input.parse()?;
                Ok(Self(
                    quote!(::parser::ast::UnlabelledReadParam::Absolute(::parser::ast::UnlabelledAbsoluteParam{value:#value})),
                    PhantomData,
                ))
            }
        }
    }

    impl Parse for Ast<UnlabelledWriteParam<'static>> {
        fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
            if input.peek(Token![@]) {
                let _: Token![@] = input.parse().unwrap();
                let value: AstOrBrace<Box<Expression>> = input.parse()?;
                Ok(Self(
                    quote!(::parser::ast::UnlabelledWriteParam::Relative(::parser::ast::UnlabelledRelativeParam{value:#value})),
                    PhantomData,
                ))
            } else {
                let value: AstOrBrace<Box<Expression>> = input.parse()?;
                Ok(Self(
                    quote!(::parser::ast::UnlabelledWriteParam::Absolute(::parser::ast::UnlabelledAbsoluteParam{value:#value})),
                    PhantomData,
                ))
            }
        }
    }
}
