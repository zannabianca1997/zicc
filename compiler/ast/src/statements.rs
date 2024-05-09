use super::{expression::Expression, punctuated::Punctuated, tokens::*, typedef::TypeDefData};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Statement {
    Block(StatementBlock),
    Let(StatementLet),
    Expr(StatementExpr),
    Loop(StatementLoop),
    If(StatementIf),
    For(StatementFor),
    While(StatementWhile),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct StatementBlock {
    pub brace_open: PunctBraceOpen,
    pub statements: Punctuated<Statement, PunctSemi>,
    pub paren_close: PunctBraceClose,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct StatementLet {
    pub let_kw: KeywordLet,
    pub ident: Identifier,
    pub colon: PunctColon,
    pub ty: TypeDefData,
    pub initializer: Option<(PunctEq, Expression)>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct StatementExpr {
    pub expr: Expression,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct StatementLoop {
    pub loop_kw: KeywordLoop,
    pub body: StatementBlock,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct StatementIf {
    pub if_kw: KeywordIf,
    pub condition: Expression,
    pub body: StatementBlock,
    pub else_branch: Option<(KeywordElse, StatementBlock)>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct StatementFor {}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct StatementWhile {}
