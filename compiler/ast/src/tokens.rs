use keywords::logos_keywords;
use logos::Logos;
use macro_rules_attribute::apply;
use punctuators::logos_punctuators;
use string_interner::DefaultStringInterner;

use crate::tokens::identifiers::Identifier;

pub mod identifiers;
pub mod keywords;
pub mod punctuators;

/// A token of a zicc source
#[apply(logos_punctuators)] // Add a `Punctuator` variant
#[apply(logos_keywords)] // Add a `Keyword` variant
#[derive(Logos)]
#[logos(extras=LexerExtras)]
pub enum Token {
    #[regex(r#"[\w&&[^\d_]]\w*|_+[\w&&[^_]]\w*"#, |lex| Identifier::new(lex.slice(), &mut lex.extras.interner))]
    Identifier(Identifier),
}

pub struct LexerExtras {
    pub interner: DefaultStringInterner,
}
