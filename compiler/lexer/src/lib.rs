use keywords::logos_keywords;
use logos::Logos;
use macro_rules_attribute::apply;
use punctuators::logos_punctuators;
use string_interner::DefaultStringInterner;

use identifiers::Identifier;
use int_literal::IntLiteral;

pub mod identifiers;
pub mod int_literal;
pub mod keywords;
pub mod punctuators;

/// A token of a zicc source
#[apply(logos_punctuators)] // Add a `Punctuator` variant
#[apply(logos_keywords)] // Add a `Keyword` variant
#[derive(Logos, Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[logos(extras=LexerExtras<'s>)]
#[logos(skip r"(?:/\*(?:.|\n)*?\*/|//[^\n]*|\s)+")]
pub enum Token {
    #[regex(r#"[\w&&[^\d_]]\w*|_+[\w&&[^_]]\w*"#, |lex| Identifier::new(lex.slice(), &mut lex.extras.interner).unwrap())]
    Identifier(Identifier),
    #[regex(r#"\d+"#, |lex| IntLiteral::parse(lex.slice()).unwrap())]
    IntLiteral(IntLiteral),
}

pub struct LexerExtras<'i> {
    pub interner: &'i mut DefaultStringInterner,
}

#[cfg(test)]
mod tests;
