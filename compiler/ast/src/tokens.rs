use keywords::logos_keywords;
use logos::Logos;
use macro_rules_attribute::apply;
use punctuators::logos_punctuators;

use crate::tokens::identifiers::Identifier;

pub mod identifiers;
pub mod keywords;
pub mod punctuators;

/// A token of a zicc source
#[apply(logos_punctuators)] // Add a `Punctuator` variant
#[apply(logos_keywords)] // Add a `Keyword` variant
#[derive(Logos)]
pub enum Token {
    Identifier(Identifier),
}
