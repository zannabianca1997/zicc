use proc_macro::TokenStream;

/// Parse rust tokens as intcode assembly
#[proc_macro]
pub fn ica(tokens: TokenStream) -> TokenStream {
    impls::ica(tokens.into()).into()
}

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
