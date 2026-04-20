use string_interner::DefaultStringInterner;

use crate::{Token, identifiers::Identifier};

#[derive(std::fmt::Debug, Clone, Copy)]
pub struct Display<'a, T: ?Sized>(&'a T, &'a DefaultStringInterner);

/// A type that can be displayed with a string interner
pub trait Displayable
where
    for<'a> Display<'a, Self>: std::fmt::Display,
{
    /// Display adapter
    ///
    /// Return a structure that implements display.
    ///
    /// Given that some form of tokens cannot be displayed without the interner,
    /// this binds the interner to the token returning an adapter
    fn display<'a>(&'a self, string_interner: &'a DefaultStringInterner) -> Display<'a, Self> {
        Display(self, string_interner)
    }
}

// trivial implementation
impl<'a, T> std::fmt::Display for Display<'a, T>
where
    T: std::fmt::Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(&self.0, f)
    }
}

impl<'a> std::fmt::Display for Display<'a, Identifier> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(&self.1.resolve(self.0.0).ok_or(std::fmt::Error)?, f)
    }
}

impl<'a> std::fmt::Display for Display<'a, Token> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.0 {
            Token::Identifier(identifier) => std::fmt::Display::fmt(&identifier.display(self.1), f),
            Token::IntLiteral(int_literal) => {
                std::fmt::Display::fmt(&int_literal.display(self.1), f)
            }
            Token::Punctuator(punctuator) => std::fmt::Display::fmt(&punctuator.display(self.1), f),
            Token::Keyword(keyword) => std::fmt::Display::fmt(&keyword.display(self.1), f),
        }
    }
}
