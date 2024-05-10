use std::{cell::RefCell, fmt::Display, hash::Hash, rc::Rc};

use logos::Logos;
use string_interner::{symbol::DefaultSymbol, DefaultStringInterner as StringInterner};
use thiserror::Error;

use display_context::{display_with_any_context, DisplayWithContext};
use errors::Accumulator;
use spans::{Pos, Span, Spanned};
use vm::VMInt;

use crate::ast_node::AstNode;

#[derive(Debug, Clone, Copy)]
pub struct Identifier {
    symbol: DefaultSymbol,
    span: Span,
}
impl AstNode for Identifier {
    fn visited_by<Visitor: crate::ast_node::AstVisitor>(
        &self,
        visitor: &mut Visitor,
    ) -> Visitor::Result {
        let child_visitor = visitor.enter(self);
        visitor.exit(self, child_visitor)
    }

    fn visited_by_mut<Visitor: crate::ast_node::AstVisitorMut>(
        &mut self,
        visitor: &mut Visitor,
    ) -> Visitor::Result {
        let child_visitor = visitor.enter_mut(self);
        visitor.exit_mut(self, child_visitor)
    }
}
impl DisplayWithContext<StringInterner> for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, context: &StringInterner) -> std::fmt::Result {
        f.write_str(
            context
                .resolve(self.symbol)
                .expect("Indentifier should be displaied with the right interner"),
        )
    }
}
impl PartialEq for Identifier {
    fn eq(&self, other: &Self) -> bool {
        self.symbol == other.symbol
    }
}
impl Eq for Identifier {}
impl PartialOrd for Identifier {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.symbol.partial_cmp(&other.symbol)
    }
}
impl Ord for Identifier {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.symbol.cmp(&other.symbol)
    }
}
impl Hash for Identifier {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.symbol.hash(state);
    }
}
impl Spanned for Identifier {
    fn span(&self) -> Span {
        self.span
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Literal {
    span: Span,
    value: VMInt,
}
impl AstNode for Literal {
    fn visited_by<Visitor: crate::ast_node::AstVisitor>(
        &self,
        visitor: &mut Visitor,
    ) -> Visitor::Result {
        let child_visitor = visitor.enter(self);
        visitor.exit(self, child_visitor)
    }

    fn visited_by_mut<Visitor: crate::ast_node::AstVisitorMut>(
        &mut self,
        visitor: &mut Visitor,
    ) -> Visitor::Result {
        let child_visitor = visitor.enter_mut(self);
        visitor.exit_mut(self, child_visitor)
    }
}
impl Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        <VMInt as Display>::fmt(&self.value, f)
    }
}
display_with_any_context! {Literal}
impl PartialEq for Literal {
    fn eq(&self, other: &Self) -> bool {
        self.value == other.value
    }
}
impl Eq for Literal {}
impl PartialOrd for Literal {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.value.partial_cmp(&other.value)
    }
}
impl Ord for Literal {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.value.cmp(&other.value)
    }
}
impl Hash for Literal {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.value.hash(state);
    }
}
impl Spanned for Literal {
    fn span(&self) -> Span {
        self.span
    }
}

// Common macro so token are automagically derived

macro_rules! keywords_and_puncts {
    (
        keywords { $($keyword_ident:ident $keyword_str:literal ;)* }
        punctuators { $($punct_ident:ident $punct_str:literal ;)* }
    ) => {
        ::paste::paste!{
            #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
            /// Arbitrary enum keyword
            pub enum Keyword {
                $(
                    $keyword_ident([<Keyword $keyword_ident>]),
                )*
            }
            impl std::fmt::Display for Keyword {
                fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                    match self {
                        $(
                            Self::$keyword_ident(kwd) => <[<Keyword $keyword_ident>] as std::fmt::Display>::fmt(kwd, f),
                        )*
                    }
                }
            }
            display_with_any_context!{Keyword}

            $(
                #[derive(Debug, Clone, Copy)]
                #[doc = "The `" $keyword_str "` keyword"]
                pub struct [<Keyword $keyword_ident>] (Pos);
                impl std::fmt::Display for [<Keyword $keyword_ident>] {
                    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                        f.write_str($keyword_str)
                    }
                }
                impl AstNode for [<Keyword $keyword_ident>] {
                    fn visited_by<Visitor: crate::ast_node::AstVisitor>(
                        &self,
                        visitor: &mut Visitor,
                    ) -> Visitor::Result {
                        let child_visitor = visitor.enter(self);
                        visitor.exit(self, child_visitor)
                    }

                    fn visited_by_mut<Visitor: crate::ast_node::AstVisitorMut>(
                        &mut self,
                        visitor: &mut Visitor,
                    ) -> Visitor::Result {
                        let child_visitor = visitor.enter_mut(self);
                        visitor.exit_mut(self, child_visitor)
                    }
                }
                display_with_any_context!{[<Keyword $keyword_ident>]}
                impl [<Keyword $keyword_ident>] {
                    pub fn new()->Self {
                        Self(Pos::missing())
                    }
                }
                impl PartialEq for [<Keyword $keyword_ident>] {
                    fn eq(&self, _: &Self) -> bool {
                        true
                    }
                }
                impl Eq for [<Keyword $keyword_ident>] {}
                impl PartialOrd for [<Keyword $keyword_ident>] {
                    fn partial_cmp(&self, _: &Self) -> Option<std::cmp::Ordering> {
                        Some(std::cmp::Ordering::Equal)
                    }
                }
                impl Ord for [<Keyword $keyword_ident>] {
                    fn cmp(&self, _: &Self) -> std::cmp::Ordering {
                        std::cmp::Ordering::Equal
                    }
                }
                impl Hash for [<Keyword $keyword_ident>] {
                    fn hash<H: std::hash::Hasher>(&self, _: &mut H) {}
                }
            )*

            #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
            /// Any punctuator
            pub enum Punct {
                $(
                    $punct_ident([<Punct $punct_ident>]),
                )*
            }

            impl std::fmt::Display for Punct {
                fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                    match self {
                        $(
                            Self::$punct_ident(kwd) => <[<Punct $punct_ident>] as std::fmt::Display>::fmt(kwd, f),
                        )*
                    }
                }
            }
            display_with_any_context!{Punct}

            $(
                #[derive(Debug, Clone, Copy)]
                #[doc = "The `" $punct_str "` punctuator"]
                pub struct [<Punct $punct_ident>] (Pos);

                impl AstNode for [<Punct $punct_ident>] {
                    fn visited_by<Visitor: crate::ast_node::AstVisitor>(
                        &self,
                        visitor: &mut Visitor,
                    ) -> Visitor::Result {
                        let child_visitor = visitor.enter(self);
                        visitor.exit(self, child_visitor)
                    }

                    fn visited_by_mut<Visitor: crate::ast_node::AstVisitorMut>(
                        &mut self,
                        visitor: &mut Visitor,
                    ) -> Visitor::Result {
                        let child_visitor = visitor.enter_mut(self);
                        visitor.exit_mut(self, child_visitor)
                    }
                }

                impl std::fmt::Display for [<Punct $punct_ident>] {
                    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                        f.write_str($punct_str)
                    }
                }
                display_with_any_context!{[<Punct $punct_ident>]}
                impl PartialEq for [<Punct $punct_ident>] {
                    fn eq(&self, _: &Self) -> bool {
                        true
                    }
                }
                impl Eq for [<Punct $punct_ident>] {}
                impl PartialOrd for [<Punct $punct_ident>] {
                    fn partial_cmp(&self, _: &Self) -> Option<std::cmp::Ordering> {
                        Some(std::cmp::Ordering::Equal)
                    }
                }
                impl Ord for [<Punct $punct_ident>] {
                    fn cmp(&self, _: &Self) -> std::cmp::Ordering {
                        std::cmp::Ordering::Equal
                    }
                }
                impl Hash for [<Punct $punct_ident>] {
                    fn hash<H: std::hash::Hasher>(&self, _: &mut H) {}
                }
            )*

            #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Logos)]
            #[logos(skip r"(?:\s+|//[^\n]*|/\*[^*]*\*+(?:[^*/][^*]*\*+)*/)+")]
            #[logos(extras = LexerExtras)]
            #[logos(error = LexError)]
            pub enum Token {
                $(
                    #[token($keyword_str, |lex| [<Keyword $keyword_ident>](pos_from_lex(lex)))]
                    [<Keyword $keyword_ident>]([<Keyword $keyword_ident>]),
                )*
                $(
                    #[token($punct_str, |lex| [<Punct $punct_ident>](pos_from_lex(lex)))]
                    [<Punct $punct_ident>]([<Punct $punct_ident>]),
                )*

                #[regex(r"(?:[a-zA-Z]|_[a-zA-Z0-9_])[a-zA-Z0-9_]*", lex_identifier)]
                Identifier(Identifier),

                #[regex(r"[0-9]+", lex_number)]
                Literal(Literal),
            }


            impl DisplayWithContext<StringInterner> for Token {
                fn fmt(&self, f: &mut std::fmt::Formatter<'_>, context: &StringInterner) -> std::fmt::Result {
                    match self {
                        $(
                            Token::[<Keyword $keyword_ident>](kwd) => {
                                <[<Keyword $keyword_ident>] as DisplayWithContext<StringInterner>>::fmt(kwd, f, context)
                            }
                        )*
                        $(
                            Token::[<Punct $punct_ident>](kwd) => {
                                <[<Punct $punct_ident>] as DisplayWithContext<StringInterner>>::fmt(kwd, f, context)
                            }
                        )*
                        Token::Identifier(ident) => {
                            <Identifier as DisplayWithContext<StringInterner>>::fmt(ident, f, context)
                        }
                        Token::Literal(lit) => {
                            <Literal as DisplayWithContext<StringInterner>>::fmt(lit, f, context)
                        }
                    }
                }
            }
        }
    };
}
keywords_and_puncts! {
    keywords {
        Type   "type";
        Struct "struct";
        Union  "union";
        Fn     "fn";
        If     "if";
        For    "for";
        While  "while";
        Loop   "loop";
        Pub    "pub";
        Static "static";
        Extern "extern";
        Int    "int";
        Let    "let";
        Else   "else";
        SizeOf "size_of";
    }
    punctuators {
        Colon   ":";
        Eq      "=";
        Semi    ";";
        Comma   ",";
        Dot     ".";
        Plus    "+";
        Minus   "-";
        Star    "*";
        EqEq    "==";
        NoEq    "!=";
        Lt      "<";
        Gt      ">";
        Le      "<=";
        Ge      ">=";
        BraceOpen  "{";
        BraceClose "}";
        BracketOpen  "[";
        BracketClose "]";
        ParenOpen  "(";
        ParenClose ")";
        Ampersand  "&";
        At         "@";
        Underscore "_";
        RightArrow "->";
    }
}

fn lex_identifier<'s>(lex: &mut logos::Lexer<'s, Token>) -> Identifier {
    Identifier {
        symbol: lex.extras.interner.borrow_mut().get_or_intern(lex.slice()),
        span: Span {
            source: lex.extras.source,
            start: Some((lex.span().start as u32).try_into().unwrap()),
            end: Some((lex.span().end as u32).try_into().unwrap()),
        },
    }
}
fn lex_number<'s>(lex: &mut logos::Lexer<'s, Token>) -> Literal {
    Literal {
        span: Span {
            source: lex.extras.source,
            start: Some((lex.span().start as u32).try_into().unwrap()),
            end: Some((lex.span().end as u32).try_into().unwrap()),
        },
        value: lex.slice().parse().unwrap(),
    }
}
fn pos_from_lex<'s>(lex: &mut logos::Lexer<'s, Token>) -> Pos {
    Pos {
        source: lex.extras.source,
        position: Some((lex.span().start as u32).try_into().unwrap()),
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Error)]
pub enum LexError {
    #[error("Unknow token")]
    Unknow { span: Span },
    #[error("Literal too big to fit in VMInt")]
    UnnamedTooLong { span: Span },
}
impl Default for LexError {
    fn default() -> Self {
        Self::Unknow {
            span: Span {
                source: None,
                start: None,
                end: None,
            },
        }
    }
}

pub struct LexerExtras {
    interner: Rc<RefCell<StringInterner>>,
    source: Option<DefaultSymbol>,
}
impl LexerExtras {
    pub fn new(source: Option<&str>, interner: Rc<RefCell<StringInterner>>) -> Self {
        Self {
            source: source.map(|s| interner.borrow_mut().get_or_intern(s)),
            interner,
        }
    }
}

pub fn lex(
    source: &str,
    source_name: Option<&str>,
    mut errors: impl Accumulator<Error = LexError>,
    interner: Rc<RefCell<StringInterner>>,
) -> Box<[Token]> {
    errors
        .handle_iter(Token::lexer_with_extras(
            source,
            LexerExtras::new(source_name, interner),
        ))
        .flatten()
        .collect()
}
