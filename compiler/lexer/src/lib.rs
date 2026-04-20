use std::cell::RefCell;

use keywords::logos_keywords;
use logos::{Logos, SpannedIter};
use macro_rules_attribute::apply;
use punctuators::logos_punctuators;
use string_interner::DefaultStringInterner;

use identifiers::Identifier;
use int_literal::IntLiteral;

use crate::display::Displayable;

pub mod display;
pub mod identifiers;
pub mod int_literal;
pub mod keywords;
pub mod punctuators;

/// A token of a zicc source
#[apply(logos_punctuators)] // Add a `Punctuator` variant
#[apply(logos_keywords)] // Add a `Keyword` variant
#[derive(Logos, Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[logos(extras=LexerExtras<'s>)]
#[logos(error=InvalidToken)]
#[logos(skip r"(?:/\*([^*]|\*+[^*/])*\*+/|//[^\n]*|\s)+")]
pub enum Token {
    #[regex(r#"[\w&&[^\d_]]\w*|_+[\w&&[^_]]\w*"#, |lex| Identifier::new(lex.slice(), &mut *lex.extras.interner.borrow_mut()).unwrap())]
    Identifier(Identifier),
    #[regex(r#"\d+"#, |lex| IntLiteral::parse(lex.slice()).unwrap())]
    IntLiteral(IntLiteral),
}

impl Displayable for Token {}

pub struct LexerExtras<'i> {
    pub interner: &'i RefCell<DefaultStringInterner>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct InvalidToken;

pub fn lex<'s>(
    source: &'s str,
    interner: &'s RefCell<DefaultStringInterner>,
) -> SpannedIter<'s, Token> {
    Token::lexer_with_extras(source, LexerExtras { interner }).spanned()
}

#[cfg(test)]
mod tests;
