use std::{cell::RefCell, fmt::Display, hash::Hash, rc::Rc};

use logos::Logos;
use string_interner::{symbol::DefaultSymbol, DefaultStringInterner as StringInterner};
use thiserror::Error;

use display_context::{display_with_any_context, DisplayWithContext};
use errors::Accumulator;
use spans::{Pos, Span, Spanned};
use vm::VMInt;

use crate::ast_node::AstNode;

macro_rules! keywords {
    ( $($ident:ident $str:literal ;)*) => {
        ::paste::paste!{
            #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
            pub enum Keyword {
                $(
                    $ident([<Keyword $ident>]),
                )*
            }
            impl std::fmt::Display for Keyword {
                fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                    match self {
                        $(
                            Self::$ident(kwd) => <[<Keyword $ident>] as std::fmt::Display>::fmt(kwd, f),
                        )*
                    }
                }
            }
            display_with_any_context!{Keyword}

            $(
                #[derive(Debug, Clone, Copy)]
                pub struct [<Keyword $ident>] (Pos);
                impl std::fmt::Display for [<Keyword $ident>] {
                    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                        f.write_str($str)
                    }
                }
                impl AstNode for [<Keyword $ident>] {
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
                display_with_any_context!{[<Keyword $ident>]}
                impl [<Keyword $ident>] {
                    pub fn new()->Self {
                        Self(Pos::missing())
                    }
                }
                impl PartialEq for [<Keyword $ident>] {
                    fn eq(&self, _: &Self) -> bool {
                        true
                    }
                }
                impl Eq for [<Keyword $ident>] {}
                impl PartialOrd for [<Keyword $ident>] {
                    fn partial_cmp(&self, _: &Self) -> Option<std::cmp::Ordering> {
                        Some(std::cmp::Ordering::Equal)
                    }
                }
                impl Ord for [<Keyword $ident>] {
                    fn cmp(&self, _: &Self) -> std::cmp::Ordering {
                        std::cmp::Ordering::Equal
                    }
                }
                impl Hash for [<Keyword $ident>] {
                    fn hash<H: std::hash::Hasher>(&self, _: &mut H) {}
                }
            )*
        }
    };
}

keywords! {
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
}

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

macro_rules! punctuators {
    ( $($ident:ident $str:literal ;)*) => {
        ::paste::paste!{
            #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
            pub enum Punct {
                $(
                    $ident([<Punct $ident>]),
                )*
            }

            impl std::fmt::Display for Punct {
                fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                    match self {
                        $(
                            Self::$ident(kwd) => <[<Punct $ident>] as std::fmt::Display>::fmt(kwd, f),
                        )*
                    }
                }
            }
            display_with_any_context!{Punct}

            $(
                #[derive(Debug, Clone, Copy)]
                pub struct [<Punct $ident>] (Pos);

                impl AstNode for [<Punct $ident>] {
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

                impl std::fmt::Display for [<Punct $ident>] {
                    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                        f.write_str($str)
                    }
                }
                display_with_any_context!{[<Punct $ident>]}
                impl PartialEq for [<Punct $ident>] {
                    fn eq(&self, _: &Self) -> bool {
                        true
                    }
                }
                impl Eq for [<Punct $ident>] {}
                impl PartialOrd for [<Punct $ident>] {
                    fn partial_cmp(&self, _: &Self) -> Option<std::cmp::Ordering> {
                        Some(std::cmp::Ordering::Equal)
                    }
                }
                impl Ord for [<Punct $ident>] {
                    fn cmp(&self, _: &Self) -> std::cmp::Ordering {
                        std::cmp::Ordering::Equal
                    }
                }
                impl Hash for [<Punct $ident>] {
                    fn hash<H: std::hash::Hasher>(&self, _: &mut H) {}
                }
            )*
        }
    };
}

punctuators! {
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Logos)]
#[logos(skip r"(?:\s+|//[^\n]*|/\*[^*]*\*+(?:[^*/][^*]*\*+)*/)+")]
#[logos(extras = LexerExtras)]
#[logos(error = LexError)]
pub enum Token {
    #[token("type", |lex| Keyword::Type(KeywordType(pos_from_lex(lex))))]
    #[token("struct", |lex| Keyword::Struct(KeywordStruct(pos_from_lex(lex))))]
    #[token("union", |lex| Keyword::Union(KeywordUnion(pos_from_lex(lex))))]
    #[token("fn", |lex| Keyword::Fn(KeywordFn(pos_from_lex(lex))))]
    #[token("if", |lex| Keyword::If(KeywordIf(pos_from_lex(lex))))]
    #[token("for", |lex| Keyword::For(KeywordFor(pos_from_lex(lex))))]
    #[token("while", |lex| Keyword::While(KeywordWhile(pos_from_lex(lex))))]
    #[token("loop", |lex| Keyword::Loop(KeywordLoop(pos_from_lex(lex))))]
    #[token("pub", |lex| Keyword::Pub(KeywordPub(pos_from_lex(lex))))]
    #[token("static", |lex| Keyword::Static(KeywordStatic(pos_from_lex(lex))))]
    #[token("extern", |lex| Keyword::Extern(KeywordExtern(pos_from_lex(lex))))]
    #[token("int", |lex| Keyword::Int(KeywordInt(pos_from_lex(lex))))]
    #[token("let", |lex| Keyword::Let(KeywordLet(pos_from_lex(lex))))]
    #[token("else", |lex| Keyword::Else(KeywordElse(pos_from_lex(lex))))]
    Keyword(Keyword),

    #[token(":", |lex| Punct::Colon(PunctColon(pos_from_lex(lex))))]
    #[token("=", |lex| Punct::Eq(PunctEq(pos_from_lex(lex))))]
    #[token(";", |lex| Punct::Semi(PunctSemi(pos_from_lex(lex))))]
    #[token(",", |lex| Punct::Comma(PunctComma(pos_from_lex(lex))))]
    #[token(".", |lex| Punct::Dot(PunctDot(pos_from_lex(lex))))]
    #[token("+", |lex| Punct::Plus(PunctPlus(pos_from_lex(lex))))]
    #[token("-", |lex| Punct::Minus(PunctMinus(pos_from_lex(lex))))]
    #[token("*", |lex| Punct::Star(PunctStar(pos_from_lex(lex))))]
    #[token("==", |lex| Punct::EqEq(PunctEqEq(pos_from_lex(lex))))]
    #[token("!=", |lex| Punct::NoEq(PunctNoEq(pos_from_lex(lex))))]
    #[token("<", |lex| Punct::Lt(PunctLt(pos_from_lex(lex))))]
    #[token(">", |lex| Punct::Gt(PunctGt(pos_from_lex(lex))))]
    #[token("<=", |lex| Punct::Le(PunctLe(pos_from_lex(lex))))]
    #[token(">=", |lex| Punct::Ge(PunctGe(pos_from_lex(lex))))]
    #[token("{", |lex| Punct::BraceOpen(PunctBraceOpen(pos_from_lex(lex))))]
    #[token("}", |lex| Punct::BraceClose(PunctBraceClose(pos_from_lex(lex))))]
    #[token("[", |lex| Punct::BracketOpen(PunctBracketOpen(pos_from_lex(lex))))]
    #[token("]", |lex| Punct::BracketClose(PunctBracketClose(pos_from_lex(lex))))]
    #[token("(", |lex| Punct::ParenOpen(PunctParenOpen(pos_from_lex(lex))))]
    #[token(")", |lex| Punct::ParenClose(PunctParenClose(pos_from_lex(lex))))]
    #[token("&", |lex| Punct::Ampersand(PunctAmpersand(pos_from_lex(lex))))]
    #[token("@", |lex| Punct::At(PunctAt(pos_from_lex(lex))))]
    #[token("_", |lex| Punct::Underscore(PunctUnderscore(pos_from_lex(lex))))]
    #[token("->", |lex| Punct::RightArrow(PunctRightArrow(pos_from_lex(lex))))]
    Punct(Punct),

    #[regex(r"(?:[a-zA-Z]|_[a-zA-Z0-9_])[a-zA-Z0-9_]*", lex_identifier)]
    Identifier(Identifier),

    #[regex(r"[0-9]+", lex_number)]
    Literal(Literal),
}

impl DisplayWithContext<StringInterner> for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, context: &StringInterner) -> std::fmt::Result {
        match self {
            Token::Keyword(kwd) => {
                <Keyword as DisplayWithContext<StringInterner>>::fmt(kwd, f, context)
            }
            Token::Punct(punct) => {
                <Punct as DisplayWithContext<StringInterner>>::fmt(punct, f, context)
            }
            Token::Identifier(ident) => {
                <Identifier as DisplayWithContext<StringInterner>>::fmt(ident, f, context)
            }
            Token::Literal(lit) => {
                <Literal as DisplayWithContext<StringInterner>>::fmt(lit, f, context)
            }
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
