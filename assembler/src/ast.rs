use std::hash::Hash;
use std::iter::Chain;
use std::ops::Range;

use errors::{Accumulator, Spanned};
use thiserror::Error;
use uncased::AsUncased;

use crate::parse_all_from_parse;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct File<'s> {
    pub statements: Vec<Labelled<'s, Option<Statement<'s>>>>,
}
pub fn parse<'s>(source: &'s str, errors: &mut Accumulator<impl From<ParseError>>) -> File<'s> {
    todo!()
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Labelled<'s, T> {
    pub labels: Vec<LabelDef<'s>>,
    pub content: T,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct LabelDef<'s> {
    pub label: Identifier<'s>,
    pub colon: Colon,
}

impl Spanned for LabelDef<'_> {
    fn span(&self) -> Range<usize> {
        self.label.span().start..self.colon.span().end
    }
}

macro_rules! keywords {
    (
        $(
            $kwd:literal => $name:ident,
        )*
    ) => {
        #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
        pub enum KeyWord {
            $($name($name),)*
        }
        impl KeyWord {
            pub fn len(&self)->usize {
                match self {
                    $(KeyWord::$name(v) => v.len(),)*
                }
            }
            pub fn as_str(&self)->&'static str {
                match self {
                    $(KeyWord::$name(v) => v.as_str(),)*
                }
            }
        }
        impl Spanned for KeyWord {
            fn span(&self)->std::ops::Range<usize> {
                match self {
                    $(KeyWord::$name(v) => v.span(),)*
                }
            }
        }
        impl From<KeyWord> for &'static str {
            fn from(value:KeyWord) -> &'static str {
                value.as_str()
            }
        }
        $(
            #[derive(Debug, Clone, Copy)]
            pub struct $name {
                pos: usize
            }
            impl $name {
                fn new(pos:usize)-> $name {
                    $name { pos }
                }
                pub const fn len(&self)->usize {
                    $kwd.len()
                }
                pub const fn as_str(&self)->&'static str {
                    $kwd
                }
            }
            impl Spanned for $name {
                fn span(&self) -> std::ops::Range<usize> {
                    self.pos..(self.pos + self.len())
                }
            }
            impl<'s> Parse<'s> for $name {
                fn parse<'t>(
                    tokens: TokensSlice<'s, 't>,
                    errors: &mut Accumulator<impl From<ParseError>>,
                ) -> Option<(Self, TokensSlice<'s, 't>)> {
                    if let Some((Token::Identifier(ident @ Identifier { value, ..}), rest)) = tokens.split_first() {
                        if value.as_uncased()== $kwd.as_uncased(){
                            return Some(($name::new(ident.span().start), rest))
                        }
                        errors.push(ParseError::ExpectedToken(stringify!($name), tokens.span().start));
                        None
                    } else {
                        errors.push(ParseError::ExpectedToken(stringify!($name), tokens.span().start));
                        None
                    }
                }
            }
            parse_all_from_parse!($name);
            impl PartialEq for $name {
                fn eq(&self, _other: &Self) -> bool {
                    true
                }
            }
            impl Eq for $name {}
            impl PartialOrd for $name {
                fn partial_cmp(&self, _other: &Self) -> Option<std::cmp::Ordering> {
                    Some(std::cmp::Ordering::Equal)
                }
            }
            impl Ord for $name {
                fn cmp(&self, _other: &Self) -> std::cmp::Ordering {
                    std::cmp::Ordering::Equal
                }
            }
            impl Hash for $name {
                fn hash<H: std::hash::Hasher>(&self, _state: &mut H) {}
            }
        )*
    };
}
keywords! {
    "ints"=>IntsKwd,
    "add"=>AddKwd,
    "mul"=>MulKwd,
    "in"=>InKwd,
    "out"=>OutKwd,
    "jz"=>JzKwd,
    "jnz"=>JnzKwd,
    "slt"=>SltKwd,
    "seq"=>SeqKwd,
    "incb"=>IncbKwd,
    "halt"=>HaltKwd,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Statement<'s> {
    IntsStm(IntsStm<'s>),
}

impl<'s> Parse<'s> for Statement<'s> {
    fn parse<'t>(
        tokens: TokensSlice<'s, 't>,
        errors: &mut Accumulator<impl From<ParseError>>,
    ) -> Option<(Self, TokensSlice<'s, 't>)> {
        let (cmd, _) = KeyWord::parse(tokens, errors)?;
        match cmd {
            KeyWord::IntsKwd(_) => {
                let (ints_stm, tokens) = Parse::parse(tokens, errors)?;
                Some((Statement::IntsStm(ints_stm), tokens))
            }
            KeyWord::AddKwd(_)
            | KeyWord::MulKwd(_)
            | KeyWord::InKwd(_)
            | KeyWord::OutKwd(_)
            | KeyWord::JzKwd(_)
            | KeyWord::JnzKwd(_)
            | KeyWord::SltKwd(_)
            | KeyWord::SeqKwd(_)
            | KeyWord::IncbKwd(_)
            | KeyWord::HaltKwd(_) => {
                todo!()
            }
        }
    }
}
impl<'s> ParseAll<'s> for Statement<'s> {
    fn parse_all<'t>(
        tokens: TokensSlice<'s, 't>,
        errors: &mut Accumulator<impl From<ParseError>>,
    ) -> Option<Self> {
        let (cmd, _) = KeyWord::parse(tokens, errors)?;
        match cmd {
            KeyWord::IntsKwd(_) => {
                let ints_stm = ParseAll::parse_all(tokens, errors)?;
                Some(Statement::IntsStm(ints_stm))
            }
            KeyWord::AddKwd(_)
            | KeyWord::MulKwd(_)
            | KeyWord::InKwd(_)
            | KeyWord::OutKwd(_)
            | KeyWord::JzKwd(_)
            | KeyWord::JnzKwd(_)
            | KeyWord::SltKwd(_)
            | KeyWord::SeqKwd(_)
            | KeyWord::IncbKwd(_)
            | KeyWord::HaltKwd(_) => {
                todo!()
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct IntsStm<'s> {
    pub ints: IntsKwd,
    pub values: Punctuated<Labelled<'s, Expression<'s>>, Option<Comma>>,
}

impl<'s> Parse<'s> for IntsStm<'s> {
    fn parse<'t>(
        tokens: TokensSlice<'s, 't>,
        errors: &mut Accumulator<impl From<ParseError>>,
    ) -> Option<(Self, TokensSlice<'s, 't>)> {
        let ((ints, values), tokens) = Parse::parse(tokens, errors)?;
        Some((IntsStm { ints, values }, tokens))
    }
}
impl<'s> ParseAll<'s> for IntsStm<'s> {
    fn parse_all<'t>(
        tokens: TokensSlice<'s, 't>,
        errors: &mut Accumulator<impl From<ParseError>>,
    ) -> Option<Self> {
        let (ints, values) = ParseAll::parse_all(tokens, errors)?;
        Some(IntsStm { ints, values })
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ReadParam<'s> {
    Absolute(AbsoluteParam<'s>),
    Immediate(ImmediateParam<'s>),
    Relative(RelativeParam<'s>),
}
impl<'s> Parse<'s> for ReadParam<'s> {
    fn parse<'t>(
        tokens: TokensSlice<'s, 't>,
        errors: &mut Accumulator<impl From<ParseError>>,
    ) -> Option<(Self, TokensSlice<'s, 't>)> {
        match tokens.split_first() {
            Some((Token::Punctuator(Punctuator::Pound(_)), _)) => {
                let (param, tokens) = Parse::parse(tokens, errors)?;
                Some((ReadParam::Immediate(param), tokens))
            }
            Some((Token::Punctuator(Punctuator::At(_)), _)) => {
                let (param, tokens) = Parse::parse(tokens, errors)?;
                Some((ReadParam::Relative(param), tokens))
            }
            Some(_) => {
                let (param, tokens) = Parse::parse(tokens, errors)?;
                Some((ReadParam::Absolute(param), tokens))
            }
            None => errors.push(err),
        }
    }
}
impl<'s> ParseAll<'s> for ReadParam<'s> {
    fn parse_all<'t>(
        tokens: TokensSlice<'s, 't>,
        errors: &mut Accumulator<impl From<ParseError>>,
    ) -> Option<Self> {
        match tokens.split_first() {
            Some((Token::Punctuator(Punctuator::Pound(_)), _)) => {
                let param = ParseAll::parse_all(tokens, errors)?;
                Some(ReadParam::Immediate(param))
            }
            Some((Token::Punctuator(Punctuator::At(_)), _)) => {
                let param = ParseAll::parse_all(tokens, errors)?;
                Some(ReadParam::Relative(param))
            }
            Some(_) => {
                let param = ParseAll::parse_all(tokens, errors)?;
                Some(ReadParam::Absolute(param))
            }
            None => todo!(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum WriteParam<'s> {
    Absolute(AbsoluteParam<'s>),
    Relative(RelativeParam<'s>),
}
impl<'s> Parse<'s> for WriteParam<'s> {
    fn parse<'t>(
        tokens: TokensSlice<'s, 't>,
        errors: &mut Accumulator<impl From<ParseError>>,
    ) -> Option<(Self, TokensSlice<'s, 't>)> {
        match tokens.split_first() {
            Some((Token::Punctuator(Punctuator::At(_)), _)) => {
                let (param, tokens) = Parse::parse(tokens, errors)?;
                Some((WriteParam::Relative(param), tokens))
            }
            Some(_) => {
                let (param, tokens) = Parse::parse(tokens, errors)?;
                Some((WriteParam::Absolute(param), tokens))
            }
            None => todo!(),
        }
    }
}
impl<'s> ParseAll<'s> for WriteParam<'s> {
    fn parse_all<'t>(
        tokens: TokensSlice<'s, 't>,
        errors: &mut Accumulator<impl From<ParseError>>,
    ) -> Option<Self> {
        match tokens.split_first() {
            Some((Token::Punctuator(Punctuator::At(_)), _)) => {
                let param = ParseAll::parse_all(tokens, errors)?;
                Some(WriteParam::Relative(param))
            }
            Some(_) => {
                let param = ParseAll::parse_all(tokens, errors)?;
                Some(WriteParam::Absolute(param))
            }
            None => todo!(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ImmediateParam<'s> {
    pound: Pound,
    value: Labelled<'s, Expression<'s>>,
}
impl<'s> Parse<'s> for ImmediateParam<'s> {
    fn parse<'t>(
        tokens: TokensSlice<'s, 't>,
        errors: &mut Accumulator<impl From<ParseError>>,
    ) -> Option<(Self, TokensSlice<'s, 't>)> {
        let ((pound, value), tokens) = Parse::parse(tokens, errors)?;
        Some((ImmediateParam { pound, value }, tokens))
    }
}
impl<'s> ParseAll<'s> for ImmediateParam<'s> {
    fn parse_all<'t>(
        tokens: TokensSlice<'s, 't>,
        errors: &mut Accumulator<impl From<ParseError>>,
    ) -> Option<Self> {
        let (pound, value) = ParseAll::parse_all(tokens, errors)?;
        Some(ImmediateParam { pound, value })
    }
}
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct AbsoluteParam<'s> {
    value: Labelled<'s, Expression<'s>>,
}
impl<'s> Parse<'s> for AbsoluteParam<'s> {
    fn parse<'t>(
        tokens: TokensSlice<'s, 't>,
        errors: &mut Accumulator<impl From<ParseError>>,
    ) -> Option<(Self, TokensSlice<'s, 't>)> {
        let (value, tokens) = Parse::parse(tokens, errors)?;
        Some((AbsoluteParam { value }, tokens))
    }
}
impl<'s> ParseAll<'s> for AbsoluteParam<'s> {
    fn parse_all<'t>(
        tokens: TokensSlice<'s, 't>,
        errors: &mut Accumulator<impl From<ParseError>>,
    ) -> Option<Self> {
        let value = ParseAll::parse_all(tokens, errors)?;
        Some(AbsoluteParam { value })
    }
}
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct RelativeParam<'s> {
    at: At,
    value: Labelled<'s, Expression<'s>>,
}
impl<'s> Parse<'s> for RelativeParam<'s> {
    fn parse<'t>(
        tokens: TokensSlice<'s, 't>,
        errors: &mut Accumulator<impl From<ParseError>>,
    ) -> Option<(Self, TokensSlice<'s, 't>)> {
        let ((at, value), tokens) = Parse::parse(tokens, errors)?;
        Some((RelativeParam { at, value }, tokens))
    }
}
impl<'s> ParseAll<'s> for RelativeParam<'s> {
    fn parse_all<'t>(
        tokens: TokensSlice<'s, 't>,
        errors: &mut Accumulator<impl From<ParseError>>,
    ) -> Option<Self> {
        let (at, value) = ParseAll::parse_all(tokens, errors)?;
        Some(RelativeParam { at, value })
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Expression<'s> {
    pub addends: Punctuated<Addend<'s>, SumOp>,
}

impl Expression<'_> {
    pub fn calculate(
        self,
        solver: &mut impl FnMut(&Identifier) -> Option<vm::VMInt>,
    ) -> Option<vm::VMInt> {
        match self.addends {
            Punctuated::NonEmpty { head, tail } => {
                let mut total = head.calculate(solver)?;
                for (op, addend) in tail {
                    match op {
                        SumOp::Plus(_) => total += addend.calculate(solver)?,
                        SumOp::Minus(_) => total -= addend.calculate(solver)?,
                    }
                }
                Some(total)
            }
            Punctuated::Empty => Some(0),
        }
    }
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

impl Addend<'_> {
    pub fn calculate(
        self,
        solver: &mut impl FnMut(&Identifier) -> Option<vm::VMInt>,
    ) -> Option<vm::VMInt> {
        match self.factors {
            Punctuated::NonEmpty { head, tail } => {
                let mut product = head.calculate(solver)?;
                for (op, factor) in tail {
                    match op {
                        MulOp::Mul(_) => product *= factor.calculate(solver)?,
                        MulOp::Div(_) => product /= factor.calculate(solver)?,
                        MulOp::Mod(_) => product %= factor.calculate(solver)?,
                    }
                }
                Some(product)
            }
            Punctuated::Empty => Some(0),
        }
    }
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
            Some((Token::Punctuator(Punctuator::Mod(m)), tokens)) => Some((MulOp::Mod(*m), tokens)),
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
    Identifier(Identifier<'s>),
    Parenthesis(Parenthesis<Box<Expression<'s>>>),
}
impl Factor<'_> {
    pub fn calculate(
        self,
        solver: &mut impl FnMut(&Identifier) -> Option<vm::VMInt>,
    ) -> Option<vm::VMInt> {
        match self {
            Factor::Neg(neg) => neg.calculate(solver),
            Factor::Number(num) => Some(num.value),
            Factor::Identifier(id) => solver(&id),
            Factor::Parenthesis(Parenthesis { inner, .. }) => inner.calculate(solver),
        }
    }
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
            Some((Token::Identifier(id), rest)) => Some((Factor::Identifier(*id), rest)),
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
            Some((Token::Identifier(_), _)) => {
                let id = ParseAll::parse_all(tokens, errors)?;
                Some(Factor::Identifier(id))
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
pub struct Neg<'s> {
    pub minus: Minus,
    pub factor: Box<Factor<'s>>,
}
impl Neg<'_> {
    pub fn calculate(
        self,
        solver: &mut impl FnMut(&Identifier) -> Option<vm::VMInt>,
    ) -> Option<vm::VMInt> {
        self.factor.calculate(solver).map(|n| -n)
    }
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
        let Some((Token::Punctuator(Punctuator::ParOpen(par_open)), tokens)) = tokens.split_first()
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

#[derive(Debug, Clone)]
pub struct PunctuatedIter<T, P> {
    head: Option<T>,
    tail: <Vec<(P, T)> as IntoIterator>::IntoIter,
}
impl<T, P> IntoIterator for Punctuated<T, P> {
    type Item = T;

    type IntoIter = PunctuatedIter<T, P>;

    fn into_iter(self) -> Self::IntoIter {
        match self {
            Punctuated::NonEmpty { head, tail } => PunctuatedIter {
                head: Some(head),
                tail: tail.into_iter(),
            },
            Punctuated::Empty => PunctuatedIter {
                head: None,
                tail: vec![].into_iter(),
            },
        }
    }
}
impl<T, P> Iterator for PunctuatedIter<T, P> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        self.head
            .take()
            .or_else(|| self.tail.next().map(|(_, t)| t))
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
