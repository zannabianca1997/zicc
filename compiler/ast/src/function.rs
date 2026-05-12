use zicc_compiler_lexer::{
    identifiers::Identifier,
    keywords::Fn,
    punctuators::{
        BraceClose, BraceOpen, Colon, Comma, ParenthesesClose, ParenthesesOpen, RightArrow,
    },
};

use crate::{punctuated::Punctuated, type_def::TypeDef};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ItemFunction {
    pub k_fn: Fn,
    pub ident: Identifier,
    pub p_parentheses_open: ParenthesesOpen,
    pub arg_list: Punctuated<Argument, Comma>,
    pub p_parentheses_close: ParenthesesClose,
    pub return_type: Option<ReturnType>,
    pub p_brace_open: BraceOpen,
    pub p_brace_close: BraceClose,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Argument {
    pub ident: Identifier,
    pub p_colon: Colon,
    pub type_def: TypeDef,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ReturnType {
    pub p_right_arrow: RightArrow,
    pub type_def: TypeDef,
}
