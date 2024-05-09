use std::fmt::Write as _;

use display_context::DisplayWithContext;
use indenter::indented;
use string_interner::DefaultStringInterner;

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

impl DisplayWithContext<DefaultStringInterner> for Statement {
    fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        context: &DefaultStringInterner,
    ) -> std::fmt::Result {
        match self {
            Statement::Block(stm) => DisplayWithContext::fmt(stm, f, context),
            Statement::Let(stm) => DisplayWithContext::fmt(stm, f, context),
            Statement::Expr(stm) => DisplayWithContext::fmt(stm, f, context),
            Statement::Loop(stm) => DisplayWithContext::fmt(stm, f, context),
            Statement::If(stm) => DisplayWithContext::fmt(stm, f, context),
            Statement::For(stm) => DisplayWithContext::fmt(stm, f, context),
            Statement::While(stm) => DisplayWithContext::fmt(stm, f, context),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct StatementBlock {
    pub brace_open: PunctBraceOpen,
    pub statements: Punctuated<Statement, PunctSemi>,
    pub brace_close: PunctBraceClose,
}

impl DisplayWithContext<DefaultStringInterner> for StatementBlock {
    fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        context: &DefaultStringInterner,
    ) -> std::fmt::Result {
        DisplayWithContext::fmt(&self.brace_open, f, context)?;
        {
            let mut f = indented(f);
            for statement_or_semi in self.statements.iter_all() {
                match statement_or_semi {
                    either::Either::Left(stm) => write!(f, "\n{}", stm.with_context(context))?,
                    either::Either::Right(semi) => write!(f, "{}", semi.with_context(context))?,
                }
            }
        }
        if !self.statements.is_empty() {
            f.write_str("\n")?;
        }
        DisplayWithContext::fmt(&self.brace_close, f, context)?;
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct StatementLet {
    pub let_kw: KeywordLet,
    pub ident: Identifier,
    pub colon: PunctColon,
    pub ty: TypeDefData,
    pub initializer: Option<(PunctEq, Expression)>,
}

impl DisplayWithContext<DefaultStringInterner> for StatementLet {
    fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        context: &DefaultStringInterner,
    ) -> std::fmt::Result {
        DisplayWithContext::fmt(&self.let_kw, f, context)?;
        f.write_str(" ")?;
        DisplayWithContext::fmt(&self.ident, f, context)?;
        DisplayWithContext::fmt(&self.colon, f, context)?;
        f.write_str(" ")?;
        DisplayWithContext::fmt(&self.ty, f, context)?;
        if let Some((eq, expr)) = &self.initializer {
            f.write_str(" ")?;
            DisplayWithContext::fmt(eq, f, context)?;
            f.write_str(" ")?;
            DisplayWithContext::fmt(expr, f, context)?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct StatementExpr {
    pub expr: Expression,
}

impl DisplayWithContext<DefaultStringInterner> for StatementExpr {
    fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        context: &DefaultStringInterner,
    ) -> std::fmt::Result {
        DisplayWithContext::fmt(&self.expr, f, context)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct StatementLoop {
    pub loop_kw: KeywordLoop,
    pub body: StatementBlock,
}

impl DisplayWithContext<DefaultStringInterner> for StatementLoop {
    fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        context: &DefaultStringInterner,
    ) -> std::fmt::Result {
        DisplayWithContext::fmt(&self.loop_kw, f, context)?;
        f.write_str(" ")?;
        DisplayWithContext::fmt(&self.body, f, context)?;
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct StatementIf {
    pub if_kw: KeywordIf,
    pub condition: Expression,
    pub body: StatementBlock,
    pub else_branch: Option<(KeywordElse, StatementBlock)>,
}

impl DisplayWithContext<DefaultStringInterner> for StatementIf {
    fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        context: &DefaultStringInterner,
    ) -> std::fmt::Result {
        DisplayWithContext::fmt(&self.if_kw, f, context)?;
        f.write_str(" ")?;
        DisplayWithContext::fmt(&self.condition, f, context)?;
        f.write_str(" ")?;
        DisplayWithContext::fmt(&self.body, f, context)?;
        if let Some((else_kw, block)) = &self.else_branch {
            f.write_str(" ")?;
            DisplayWithContext::fmt(else_kw, f, context)?;
            f.write_str(" ")?;
            DisplayWithContext::fmt(block, f, context)?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct StatementFor {}

impl DisplayWithContext<DefaultStringInterner> for StatementFor {
    fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        context: &DefaultStringInterner,
    ) -> std::fmt::Result {
        todo!()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct StatementWhile {}

impl DisplayWithContext<DefaultStringInterner> for StatementWhile {
    fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        context: &DefaultStringInterner,
    ) -> std::fmt::Result {
        todo!()
    }
}
