use std::fmt::Write as _;

use display_context::DisplayWithContext;
use indenter::indented;
use string_interner::DefaultStringInterner;

use crate::ast_node::{AstNode, AstVisitorMut};

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

impl AstNode for Statement {
    fn visited_by<Visitor: crate::ast_node::AstVisitor>(
        &self,
        visitor: &mut Visitor,
    ) -> Visitor::Result {
        let mut child_visitor = visitor.enter(self);
        match self {
            Statement::Block(stm) => stm.visited_by(&mut child_visitor),
            Statement::Let(stm) => stm.visited_by(&mut child_visitor),
            Statement::Expr(stm) => stm.visited_by(&mut child_visitor),
            Statement::Loop(stm) => stm.visited_by(&mut child_visitor),
            Statement::If(stm) => stm.visited_by(&mut child_visitor),
            Statement::For(stm) => stm.visited_by(&mut child_visitor),
            Statement::While(stm) => stm.visited_by(&mut child_visitor),
        };
        visitor.exit(self, child_visitor)
    }

    fn visited_by_mut<Visitor: crate::ast_node::AstVisitorMut>(
        &mut self,
        visitor: &mut Visitor,
    ) -> Visitor::Result {
        let mut child_visitor = visitor.enter_mut(self);
        match self {
            Statement::Block(stm) => stm.visited_by_mut(&mut child_visitor),
            Statement::Let(stm) => stm.visited_by_mut(&mut child_visitor),
            Statement::Expr(stm) => stm.visited_by_mut(&mut child_visitor),
            Statement::Loop(stm) => stm.visited_by_mut(&mut child_visitor),
            Statement::If(stm) => stm.visited_by_mut(&mut child_visitor),
            Statement::For(stm) => stm.visited_by_mut(&mut child_visitor),
            Statement::While(stm) => stm.visited_by_mut(&mut child_visitor),
        };
        visitor.exit_mut(self, child_visitor)
    }
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

impl AstNode for StatementBlock {
    fn visited_by<Visitor: crate::ast_node::AstVisitor>(
        &self,
        visitor: &mut Visitor,
    ) -> Visitor::Result {
        let mut child_visitor = visitor.enter(self);
        self.brace_open.visited_by(&mut child_visitor);
        for child in self.statements.iter_all() {
            match child {
                either::Either::Left(child) => child.visited_by(&mut child_visitor),
                either::Either::Right(child) => child.visited_by(&mut child_visitor),
            };
        }
        self.brace_close.visited_by(&mut child_visitor);
        visitor.exit(self, child_visitor)
    }

    fn visited_by_mut<Visitor: crate::ast_node::AstVisitorMut>(
        &mut self,
        visitor: &mut Visitor,
    ) -> Visitor::Result {
        let mut child_visitor = visitor.enter_mut(self);
        self.brace_open.visited_by_mut(&mut child_visitor);
        for child in self.statements.iter_all_mut() {
            match child {
                either::Either::Left(child) => child.visited_by_mut(&mut child_visitor),
                either::Either::Right(child) => child.visited_by_mut(&mut child_visitor),
            };
        }
        self.brace_close.visited_by_mut(&mut child_visitor);
        visitor.exit_mut(self, child_visitor)
    }
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

impl AstNode for StatementLet {
    fn visited_by<Visitor: crate::ast_node::AstVisitor>(
        &self,
        visitor: &mut Visitor,
    ) -> Visitor::Result {
        let mut child_visitor = visitor.enter(self);
        self.let_kw.visited_by(&mut child_visitor);
        self.ident.visited_by(&mut child_visitor);
        self.colon.visited_by(&mut child_visitor);
        self.ty.visited_by(&mut child_visitor);
        if let Some((eq, init)) = &self.initializer {
            eq.visited_by(&mut child_visitor);
            init.visited_by(&mut child_visitor);
        }
        visitor.exit(self, child_visitor)
    }

    fn visited_by_mut<Visitor: crate::ast_node::AstVisitorMut>(
        &mut self,
        visitor: &mut Visitor,
    ) -> Visitor::Result {
        let mut child_visitor = visitor.enter_mut(self);
        self.let_kw.visited_by_mut(&mut child_visitor);
        self.ident.visited_by_mut(&mut child_visitor);
        self.colon.visited_by_mut(&mut child_visitor);
        self.ty.visited_by_mut(&mut child_visitor);
        if let Some((eq, init)) = &mut self.initializer {
            eq.visited_by_mut(&mut child_visitor);
            init.visited_by_mut(&mut child_visitor);
        }
        visitor.exit_mut(self, child_visitor)
    }
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

impl AstNode for StatementExpr {
    fn visited_by<Visitor: crate::ast_node::AstVisitor>(
        &self,
        visitor: &mut Visitor,
    ) -> Visitor::Result {
        let mut child_visitor = visitor.enter(self);
        self.expr.visited_by(&mut child_visitor);
        visitor.exit(self, child_visitor)
    }

    fn visited_by_mut<Visitor: crate::ast_node::AstVisitorMut>(
        &mut self,
        visitor: &mut Visitor,
    ) -> Visitor::Result {
        let mut child_visitor = visitor.enter_mut(self);
        self.expr.visited_by_mut(&mut child_visitor);
        visitor.exit_mut(self, child_visitor)
    }
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

impl AstNode for StatementLoop {
    fn visited_by<Visitor: crate::ast_node::AstVisitor>(
        &self,
        visitor: &mut Visitor,
    ) -> Visitor::Result {
        let mut child_visitor = visitor.enter(self);
        self.loop_kw.visited_by(&mut child_visitor);
        self.body.visited_by(&mut child_visitor);
        visitor.exit(self, child_visitor)
    }

    fn visited_by_mut<Visitor: crate::ast_node::AstVisitorMut>(
        &mut self,
        visitor: &mut Visitor,
    ) -> Visitor::Result {
        let mut child_visitor = visitor.enter_mut(self);
        self.loop_kw.visited_by_mut(&mut child_visitor);
        self.body.visited_by_mut(&mut child_visitor);
        visitor.exit_mut(self, child_visitor)
    }
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

impl AstNode for StatementIf {
    fn visited_by<Visitor: crate::ast_node::AstVisitor>(
        &self,
        visitor: &mut Visitor,
    ) -> Visitor::Result {
        let mut child_visitor = visitor.enter(self);
        self.if_kw.visited_by(&mut child_visitor);
        self.condition.visited_by(&mut child_visitor);
        self.body.visited_by(&mut child_visitor);
        if let Some((else_kw, else_body)) = &self.else_branch {
            else_kw.visited_by(&mut child_visitor);
            else_body.visited_by(&mut child_visitor);
        }
        visitor.exit(self, child_visitor)
    }

    fn visited_by_mut<Visitor: crate::ast_node::AstVisitorMut>(
        &mut self,
        visitor: &mut Visitor,
    ) -> Visitor::Result {
        let mut child_visitor = visitor.enter_mut(self);
        self.if_kw.visited_by_mut(&mut child_visitor);
        self.condition.visited_by_mut(&mut child_visitor);
        self.body.visited_by_mut(&mut child_visitor);
        if let Some((else_kw, else_body)) = &mut self.else_branch {
            else_kw.visited_by_mut(&mut child_visitor);
            else_body.visited_by_mut(&mut child_visitor);
        }
        visitor.exit_mut(self, child_visitor)
    }
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

impl AstNode for StatementFor {
    fn visited_by<Visitor: crate::ast_node::AstVisitor>(
        &self,
        visitor: &mut Visitor,
    ) -> Visitor::Result {
        todo!()
    }

    fn visited_by_mut<Visitor: AstVisitorMut>(&mut self, visitor: &mut Visitor) -> Visitor::Result {
        todo!()
    }
}

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

impl AstNode for StatementWhile {
    fn visited_by<Visitor: crate::ast_node::AstVisitor>(
        &self,
        visitor: &mut Visitor,
    ) -> Visitor::Result {
        todo!()
    }

    fn visited_by_mut<Visitor: AstVisitorMut>(&mut self, visitor: &mut Visitor) -> Visitor::Result {
        todo!()
    }
}

impl DisplayWithContext<DefaultStringInterner> for StatementWhile {
    fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        context: &DefaultStringInterner,
    ) -> std::fmt::Result {
        todo!()
    }
}
