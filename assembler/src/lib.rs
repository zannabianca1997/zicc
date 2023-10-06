#![feature(char_indices_offset)]
pub mod tokens;

pub mod ast {
    use std::ops::Range;

    use errors::{Accumulator, Spanned};
    use thiserror::Error;

    use crate::parse_all_from_parse;
    use crate::tokens::{
        At, Colon, Comma, Div, Identifier, Minus, Mod, Mul, Number, ParClose, ParOpen, Plus, Pound,
        Punctuator, Token, Tokens, TokensSlice,
    };

    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct File<'s> {
        pub statements: Vec<Labelled<'s, Option<Statement<'s>>>>,
    }
    impl<'s> File<'s> {
        pub fn parse<'t>(
            tokens: TokensSlice<'s, 't>,
            errors: &mut Accumulator<impl From<ParseError>>,
        ) -> File<'s> {
            ParseAll::parse_all(tokens, errors).unwrap()
        }
    }
    impl<'s> ParseAll<'s> for File<'s> {
        fn parse_all<'t>(
            tokens: TokensSlice<'s, 't>,
            errors: &mut Accumulator<impl From<ParseError>>,
        ) -> Option<Self> {
            // no statement contains newline, so we can split them directly
            let statements = tokens
                .split_newlines()
                .flat_map(|l| ParseAll::parse_all(l, errors))
                .collect();
            Some(File { statements })
        }
    }

    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct Labelled<'s, T> {
        pub labels: Punctuated<LabelDef<'s>, ()>,
        pub content: T,
    }

    impl<'s, T> Parse<'s> for Labelled<'s, T>
    where
        T: Parse<'s>,
    {
        fn parse<'t>(
            tokens: TokensSlice<'s, 't>,
            errors: &mut Accumulator<impl From<ParseError>>,
        ) -> Option<(Self, TokensSlice<'s, 't>)> {
            let (labels, tokens) = Parse::parse(tokens, errors)?;
            let (content, tokens) = Parse::parse(tokens, errors)?;
            Some((Labelled { labels, content }, tokens))
        }
    }

    impl<'s, T> ParseAll<'s> for Labelled<'s, T>
    where
        T: ParseAll<'s>,
    {
        fn parse_all<'t>(
            tokens: TokensSlice<'s, 't>,
            errors: &mut Accumulator<impl From<ParseError>>,
        ) -> Option<Self> {
            let (labels, tokens) = Parse::parse(tokens, errors)?;
            let content = ParseAll::parse_all(tokens, errors)?;
            Some(Labelled { labels, content })
        }
    }

    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct LabelDef<'s> {
        pub label: Identifier<'s>,
        pub colon: Colon,
    }

    impl<'s> Parse<'s> for LabelDef<'s> {
        fn parse<'t>(
            tokens: TokensSlice<'s, 't>,
            errors: &mut Accumulator<impl From<ParseError>>,
        ) -> Option<(Self, TokensSlice<'s, 't>)> {
            let (label, tokens) = Parse::parse(tokens, errors)?;
            let (colon, tokens) = Parse::parse(tokens, errors)?;
            Some((LabelDef { label, colon }, tokens))
        }
    }

    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub enum Statement<'s> {
        Ints(Ints<'s>),
    }

    impl<'s> Parse<'s> for Statement<'s> {
        fn parse<'t>(
            tokens: TokensSlice<'s, 't>,
            errors: &mut Accumulator<impl From<ParseError>>,
        ) -> Option<(Self, TokensSlice<'s, 't>)> {
            let (cmd_ident @ Identifier { value: command, .. }, _) = Parse::parse(tokens, errors)?;
            match command {
                "ints" => {
                    let (ints, tokens) = Parse::parse(tokens, errors)?;
                    Some((Statement::Ints(ints), tokens))
                }
                _ => {
                    errors.push(ParseError::UnknowCommand(cmd_ident.span()));
                    None
                }
            }
        }
    }
    impl<'s> ParseAll<'s> for Statement<'s> {
        fn parse_all<'t>(
            tokens: TokensSlice<'s, 't>,
            errors: &mut Accumulator<impl From<ParseError>>,
        ) -> Option<Self> {
            let (cmd_ident @ Identifier { value: command, .. }, _) = Parse::parse(tokens, errors)?;
            match command {
                "ints" => {
                    let ints = ParseAll::parse_all(tokens, errors)?;
                    Some(Statement::Ints(ints))
                }
                _ => {
                    errors.push(ParseError::UnknowCommand(cmd_ident.span()));
                    None
                }
            }
        }
    }

    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct Ints<'s> {
        pub ints: Identifier<'s>,
        pub values: Punctuated<Labelled<'s, Expression<'s>>, Option<Comma>>,
    }

    impl<'s> Parse<'s> for Ints<'s> {
        fn parse<'t>(
            tokens: TokensSlice<'s, 't>,
            errors: &mut Accumulator<impl From<ParseError>>,
        ) -> Option<(Self, TokensSlice<'s, 't>)> {
            let Some((Token::Identifier(ints @ Identifier { value: "ints", .. }), tokens)) =
                tokens.split_first()
            else {
                errors.push(ParseError::ExpectedToken("`ints`", tokens.span().start));
                return None;
            };
            let (values, tokens) = Parse::parse(tokens, errors)?;
            Some((
                Ints {
                    ints: *ints,
                    values,
                },
                tokens,
            ))
        }
    }
    impl<'s> ParseAll<'s> for Ints<'s> {
        fn parse_all<'t>(
            tokens: TokensSlice<'s, 't>,
            errors: &mut Accumulator<impl From<ParseError>>,
        ) -> Option<Self> {
            let Some((Token::Identifier(ints @ Identifier { value: "ints", .. }), tokens)) =
                tokens.split_first()
            else {
                errors.push(ParseError::ExpectedToken("`ints`", tokens.span().start));
                return None;
            };
            let values = ParseAll::parse_all(tokens, errors)?;
            Some(Ints {
                ints: *ints,
                values,
            })
        }
    }

    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct Expression<'s> {
        pub addends: Punctuated<Addend<'s>, SumOp>,
    }

    impl<'s> Parse<'s> for Expression<'s> {
        fn parse<'t>(
            tokens: TokensSlice<'s, 't>,
            errors: &mut Accumulator<impl From<ParseError>>,
        ) -> Option<(Self, TokensSlice<'s, 't>)> {
            let (addends, tokens) = Parse::parse(tokens, errors)?;
            Some((Expression { addends }, tokens))
        }
    }

    impl<'s> ParseAll<'s> for Expression<'s> {
        fn parse_all<'t>(
            tokens: TokensSlice<'s, 't>,
            errors: &mut Accumulator<impl From<ParseError>>,
        ) -> Option<Self> {
            let addends = ParseAll::parse_all(tokens, errors)?;
            Some(Expression { addends })
        }
    }

    #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub enum SumOp {
        Plus(Plus),
        Minus(Minus),
    }
    impl<'s> Parse<'s> for SumOp {
        fn parse<'t>(
            tokens: TokensSlice<'s, 't>,
            errors: &mut Accumulator<impl From<ParseError>>,
        ) -> Option<(Self, TokensSlice<'s, 't>)> {
            match tokens.split_first() {
                Some((Token::Punctuator(Punctuator::Plus(plus)), tokens)) => {
                    Some((SumOp::Plus(*plus), tokens))
                }
                Some((Token::Punctuator(Punctuator::Minus(minus)), tokens)) => {
                    Some((SumOp::Minus(*minus), tokens))
                }
                _ => {
                    errors.push(ParseError::ExpectedToken(
                        "Plus or Minus",
                        tokens.span().start,
                    ));
                    None
                }
            }
        }
    }

    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct Addend<'s> {
        pub factors: Punctuated<Factor<'s>, MulOp>,
    }

    impl<'s> Parse<'s> for Addend<'s> {
        fn parse<'t>(
            tokens: TokensSlice<'s, 't>,
            errors: &mut Accumulator<impl From<ParseError>>,
        ) -> Option<(Self, TokensSlice<'s, 't>)> {
            let (factors, tokens) = Parse::parse(tokens, errors)?;
            Some((Addend { factors }, tokens))
        }
    }
    impl<'s> ParseAll<'s> for Addend<'s> {
        fn parse_all<'t>(
            tokens: TokensSlice<'s, 't>,
            errors: &mut Accumulator<impl From<ParseError>>,
        ) -> Option<Self> {
            let factors = ParseAll::parse_all(tokens, errors)?;
            Some(Addend { factors })
        }
    }

    #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub enum MulOp {
        Mul(Mul),
        Div(Div),
        Mod(Mod),
    }
    impl<'s> Parse<'s> for MulOp {
        fn parse<'t>(
            tokens: TokensSlice<'s, 't>,
            errors: &mut Accumulator<impl From<ParseError>>,
        ) -> Option<(Self, TokensSlice<'s, 't>)> {
            match tokens.split_first() {
                Some((Token::Punctuator(Punctuator::Mul(mul)), tokens)) => {
                    Some((MulOp::Mul(*mul), tokens))
                }
                Some((Token::Punctuator(Punctuator::Div(div)), tokens)) => {
                    Some((MulOp::Div(*div), tokens))
                }
                Some((Token::Punctuator(Punctuator::Mod(m)), tokens)) => {
                    Some((MulOp::Mod(*m), tokens))
                }
                _ => {
                    errors.push(ParseError::ExpectedToken(
                        "Plus or Minus",
                        tokens.span().start,
                    ));
                    None
                }
            }
        }
    }

    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub enum Factor<'s> {
        Neg(Neg<'s>),
        Number(Number),
        Reference(Reference<'s>),
        Parenthesis(Parenthesis<Box<Expression<'s>>>),
    }

    impl<'s> Parse<'s> for Factor<'s> {
        fn parse<'t>(
            tokens: TokensSlice<'s, 't>,
            errors: &mut Accumulator<impl From<ParseError>>,
        ) -> Option<(Self, TokensSlice<'s, 't>)> {
            match tokens.split_first() {
                Some((Token::Punctuator(crate::tokens::Punctuator::Minus(_)), _)) => {
                    let (neg, tokens) = Parse::parse(tokens, errors)?;
                    Some((Factor::Neg(neg), tokens))
                }
                Some((Token::Number(num), rest)) => Some((Factor::Number(*num), rest)),
                Some((
                    Token::Identifier(_)
                    | Token::Punctuator(
                        crate::tokens::Punctuator::Pound(_) | crate::tokens::Punctuator::At(_),
                    ),
                    _,
                )) => {
                    let (r, tokens) = Parse::parse(tokens, errors)?;
                    Some((Factor::Reference(r), tokens))
                }
                Some((Token::Punctuator(crate::tokens::Punctuator::ParOpen(_)), _)) => {
                    let (p, tokens) = Parse::parse(tokens, errors)?;
                    Some((Factor::Parenthesis(p), tokens))
                }
                _ => {
                    errors.push(ParseError::ExpectedToken(
                        "Minus, Reference, Number or ParOpen",
                        tokens.span().start,
                    ));
                    None
                }
            }
        }
    }

    impl<'s> ParseAll<'s> for Factor<'s> {
        fn parse_all<'t>(
            tokens: TokensSlice<'s, 't>,
            errors: &mut Accumulator<impl From<ParseError>>,
        ) -> Option<Self> {
            match tokens.split_first() {
                Some((Token::Punctuator(crate::tokens::Punctuator::Minus(_)), _)) => {
                    let neg = ParseAll::parse_all(tokens, errors)?;
                    Some(Factor::Neg(neg))
                }
                Some((Token::Number(_), _)) => {
                    let num = ParseAll::parse_all(tokens, errors)?;
                    Some(Factor::Number(num))
                }
                Some((
                    Token::Identifier(_)
                    | Token::Punctuator(
                        crate::tokens::Punctuator::Pound(_) | crate::tokens::Punctuator::At(_),
                    ),
                    _,
                )) => {
                    let r = ParseAll::parse_all(tokens, errors)?;
                    Some(Factor::Reference(r))
                }
                Some((Token::Punctuator(crate::tokens::Punctuator::ParOpen(_)), _)) => {
                    let p = ParseAll::parse_all(tokens, errors)?;
                    Some(Factor::Parenthesis(p))
                }
                _ => {
                    errors.push(ParseError::ExpectedToken(
                        "Minus, Reference, Number or ParOpen",
                        tokens.span().start,
                    ));
                    None
                }
            }
        }
    }

    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub enum Reference<'s> {
        Absolute {
            identifier: Identifier<'s>,
        },
        Relative {
            at: At,
            identifier: Identifier<'s>,
        },
        Immediate {
            pound: Pound,
            identifier: Identifier<'s>,
        },
    }
    impl<'s> Parse<'s> for Reference<'s> {
        fn parse<'t>(
            tokens: TokensSlice<'s, 't>,
            errors: &mut Accumulator<impl From<ParseError>>,
        ) -> Option<(Self, TokensSlice<'s, 't>)> {
            match tokens.split_first() {
                Some((Token::Identifier(identifier), tokens)) => Some((
                    Reference::Absolute {
                        identifier: *identifier,
                    },
                    tokens,
                )),
                Some((Token::Punctuator(crate::tokens::Punctuator::Pound(pound)), tokens)) => {
                    let (identifier, tokens) = Parse::parse(tokens, errors)?;
                    Some((
                        Reference::Immediate {
                            pound: *pound,
                            identifier,
                        },
                        tokens,
                    ))
                }
                Some((Token::Punctuator(crate::tokens::Punctuator::At(at)), tokens)) => {
                    let (identifier, tokens) = Parse::parse(tokens, errors)?;
                    Some((
                        Reference::Relative {
                            at: *at,
                            identifier,
                        },
                        tokens,
                    ))
                }
                _ => {
                    errors.push(ParseError::ExpectedToken(
                        "Pound, At or Identifier",
                        tokens.span().start,
                    ));
                    None
                }
            }
        }
    }
    parse_all_from_parse!(Reference<'s>);

    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct Neg<'s> {
        pub minus: Minus,
        pub factor: Box<Factor<'s>>,
    }

    impl<'s> Parse<'s> for Neg<'s> {
        fn parse<'t>(
            tokens: TokensSlice<'s, 't>,
            errors: &mut Accumulator<impl From<ParseError>>,
        ) -> Option<(Self, TokensSlice<'s, 't>)> {
            let ((minus, factor), tokens) = Parse::parse(tokens, errors)?;
            Some((Neg { minus, factor }, tokens))
        }
    }

    impl<'s> ParseAll<'s> for Neg<'s> {
        fn parse_all<'t>(
            tokens: TokensSlice<'s, 't>,
            errors: &mut Accumulator<impl From<ParseError>>,
        ) -> Option<Self> {
            let (minus, factor) = ParseAll::parse_all(tokens, errors)?;
            Some(Neg { minus, factor })
        }
    }

    #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct Parenthesis<T> {
        pub par_open: ParOpen,
        pub inner: T,
        pub par_close: ParClose,
    }
    impl<'s, T> Parse<'s> for Parenthesis<T>
    where
        T: ParseAll<'s>,
    {
        fn parse<'t>(
            tokens: TokensSlice<'s, 't>,
            errors: &mut Accumulator<impl From<ParseError>>,
        ) -> Option<(Self, TokensSlice<'s, 't>)> {
            // I know parenthesis are balanced, so I can use that to use parse_all
            let (par_open, tokens) = ParOpen::parse(tokens, errors)?;

            let mut par_close = None;
            let mut depth = 1usize;
            for (i, t) in tokens.tokens().into_iter().enumerate() {
                match t {
                    crate::tokens::Token::Punctuator(crate::tokens::Punctuator::ParOpen(_)) => {
                        depth += 1
                    }
                    crate::tokens::Token::Punctuator(crate::tokens::Punctuator::ParClose(tok)) => {
                        depth -= 1;
                        if depth == 0 {
                            par_close = Some((i, *tok));
                            break;
                        }
                    }
                    _ => (),
                }
            }
            if let Some((close_idx, par_close)) = par_close {
                // separate inner
                let (inner, tokens) = tokens.split_at(close_idx);
                // split off par_close
                let (_, tokens) = tokens.split_at(1);

                // parse the inner stuff
                let inner = T::parse_all(inner, errors)?;

                Some((
                    Parenthesis {
                        par_open,
                        inner,
                        par_close,
                    },
                    tokens,
                ))
            } else {
                errors.push(ParseError::UnmatchedParenthesis(par_open));
                // parse T anyway so we get some more errors
                T::parse_all(tokens, errors);

                return None;
            }
        }
    }
    impl<'s, T> ParseAll<'s> for Parenthesis<T>
    where
        T: ParseAll<'s>,
    {
        fn parse_all<'t>(
            tokens: TokensSlice<'s, 't>,
            errors: &mut Accumulator<impl From<ParseError>>,
        ) -> Option<Self> {
            let Some((Token::Punctuator(Punctuator::ParOpen(par_open)), tokens)) =
                tokens.split_first()
            else {
                errors.push(ParseError::ExpectedToken("ParOpen", tokens.span().start));
                return None;
            };
            let Some((Token::Punctuator(Punctuator::ParClose(par_close)), tokens)) =
                tokens.split_last()
            else {
                errors.push(ParseError::ExpectedToken("ParClose", tokens.span().end));
                return None;
            };
            Some(Parenthesis {
                par_open: *par_open,
                inner: ParseAll::parse_all(tokens, errors)?,
                par_close: *par_close,
            })
        }
    }

    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub enum Punctuated<T, P> {
        NonEmpty { head: T, tail: Vec<(P, T)> },
        Empty,
    }
    impl<'s, T, P> Parse<'s> for Punctuated<T, P>
    where
        T: Parse<'s>,
        P: Parse<'s>,
    {
        fn parse<'t>(
            tokens: TokensSlice<'s, 't>,
            errors: &mut Accumulator<impl From<ParseError>>,
        ) -> Option<(Self, TokensSlice<'s, 't>)> {
            let (head, mut tokens) = Parse::parse(tokens, errors)?;
            if let Some(head) = head {
                let mut tail = vec![];
                while let Some((Some(el), rest)) = <Option<(P, T)>>::parse(tokens, errors) {
                    tail.push(el);
                    tokens = rest;
                }
                Some((Punctuated::NonEmpty { head, tail }, tokens))
            } else {
                Some((Punctuated::Empty, tokens))
            }
        }
    }
    impl<'s, T, P> ParseAll<'s> for Punctuated<T, P>
    where
        T: Parse<'s> + ParseAll<'s>,
        P: Parse<'s>,
    {
        fn parse_all<'t>(
            tokens: TokensSlice<'s, 't>,
            errors: &mut Accumulator<impl From<ParseError>>,
        ) -> Option<Self> {
            let (head, mut tokens) = Parse::parse(tokens, errors)?;
            if let Some(head) = head {
                let mut tail = vec![];
                while !tokens.is_empty() {
                    let (el, rest) = <(P, T)>::parse(tokens, errors)?;
                    tail.push(el);
                    tokens = rest;
                }
                Some(Punctuated::NonEmpty { head, tail })
            } else {
                if tokens.is_empty() {
                    Some(Punctuated::Empty)
                } else {
                    // get all the errors on the tracker
                    T::parse_all(tokens, errors);
                    None
                }
            }
        }
    }

    #[derive(Debug, Clone, PartialEq, Eq, Hash, Error)]
    pub enum ParseError {
        #[error("Unexpected token")]
        UnexpectedTokens(Range<usize>),
        #[error("Expected {0}")]
        ExpectedToken(&'static str, usize),
        #[error("Unmatched parentheses")]
        UnmatchedParenthesis(ParOpen),
        #[error("Unknow command")]
        UnknowCommand(Range<usize>),
    }
    impl Spanned for ParseError {
        fn span(&self) -> Range<usize> {
            match self {
                ParseError::UnexpectedTokens(span) => span.clone(),
                ParseError::ExpectedToken(_, pos) => *pos..*pos,
                ParseError::UnmatchedParenthesis(par) => par.span(),
                ParseError::UnknowCommand(span) => span.clone(),
            }
        }
    }

    pub(crate) trait Parse<'s>: Sized {
        fn parse<'t>(
            tokens: TokensSlice<'s, 't>,
            errors: &mut Accumulator<impl From<ParseError>>,
        ) -> Option<(Self, TokensSlice<'s, 't>)>;
    }

    pub(crate) trait ParseAll<'s>: Sized {
        fn parse_all<'t>(
            tokens: TokensSlice<'s, 't>,
            errors: &mut Accumulator<impl From<ParseError>>,
        ) -> Option<Self>;
    }

    #[macro_export]
    macro_rules! parse_all_from_parse {
        ($T:ty) => {
            impl<'s> ParseAll<'s> for $T {
                fn parse_all<'t>(
                    tokens: TokensSlice<'s, 't>,
                    errors: &mut Accumulator<impl From<ParseError>>,
                ) -> Option<Self> {
                    let (parsed, rem) = Parse::parse(tokens, errors)?;
                    if !rem.is_empty() {
                        errors.push(ParseError::UnexpectedTokens(tokens.span()))
                    }
                    Some(parsed)
                }
            }
        };
    }

    impl<'s, T> Parse<'s> for Option<T>
    where
        T: Parse<'s>,
    {
        fn parse<'t>(
            tokens: TokensSlice<'s, 't>,
            _errors: &mut Accumulator<impl From<ParseError>>,
        ) -> Option<(Self, TokensSlice<'s, 't>)> {
            let mut throwaway = Accumulator::<ParseError>::new();
            Some(match T::parse(tokens, &mut throwaway) {
                Some((v, rest)) => (Some(v), rest),
                None => (None, tokens),
            })
        }
    }
    impl<'s, T> ParseAll<'s> for Option<T>
    where
        T: ParseAll<'s>,
    {
        fn parse_all<'t>(
            tokens: TokensSlice<'s, 't>,
            errors: &mut Accumulator<impl From<ParseError>>,
        ) -> Option<Self> {
            if tokens.is_empty() {
                None
            } else {
                Some(T::parse_all(tokens, errors))
            }
        }
    }
    impl<'s, T> Parse<'s> for Box<T>
    where
        T: Parse<'s>,
    {
        fn parse<'t>(
            tokens: TokensSlice<'s, 't>,
            errors: &mut Accumulator<impl From<ParseError>>,
        ) -> Option<(Self, TokensSlice<'s, 't>)> {
            T::parse(tokens, errors).map(|(t, rest)| (Box::new(t), rest))
        }
    }
    impl<'s, T> ParseAll<'s> for Box<T>
    where
        T: ParseAll<'s>,
    {
        fn parse_all<'t>(
            tokens: TokensSlice<'s, 't>,
            errors: &mut Accumulator<impl From<ParseError>>,
        ) -> Option<Self> {
            T::parse_all(tokens, errors).map(Box::new)
        }
    }

    impl<'s> Parse<'s> for () {
        fn parse<'t>(
            tokens: TokensSlice<'s, 't>,
            _errors: &mut Accumulator<impl From<ParseError>>,
        ) -> Option<(Self, TokensSlice<'s, 't>)> {
            Some(((), tokens))
        }
    }

    impl<'s> ParseAll<'s> for () {
        fn parse_all<'t>(
            tokens: TokensSlice<'s, 't>,
            errors: &mut Accumulator<impl From<ParseError>>,
        ) -> Option<Self> {
            if tokens.is_empty() {
                Some(())
            } else {
                errors.push(ParseError::UnexpectedTokens(tokens.span()));
                None
            }
        }
    }

    impl<'s, A, B> Parse<'s> for (A, B)
    where
        A: Parse<'s>,
        B: Parse<'s>,
    {
        fn parse<'t>(
            tokens: TokensSlice<'s, 't>,
            errors: &mut Accumulator<impl From<ParseError>>,
        ) -> Option<(Self, TokensSlice<'s, 't>)> {
            let (a, tokens) = A::parse(tokens, errors)?;
            let (b, tokens) = B::parse(tokens, errors)?;
            Some(((a, b), tokens))
        }
    }

    impl<'s, A, B> ParseAll<'s> for (A, B)
    where
        A: Parse<'s>,
        B: ParseAll<'s>,
    {
        fn parse_all<'t>(
            tokens: TokensSlice<'s, 't>,
            errors: &mut Accumulator<impl From<ParseError>>,
        ) -> Option<Self> {
            let (a, tokens) = A::parse(tokens, errors)?;
            let b = B::parse_all(tokens, errors)?;
            Some((a, b))
        }
    }
}
