//! Ast expressions

use super::operators::{BinaryOp, UnaryOp};

/// Expression
pub(super) enum Expression {
    Constant(isize),
    BinOp(BinaryOp, Box<Expression>, Box<Expression>),
    UnaryOp(UnaryOp, Box<Expression>),
    Call(Box<Expression>, Vec<Expression>),
}
